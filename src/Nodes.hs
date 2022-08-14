{-# LANGUAGE 
    LambdaCase,
    OverloadedStrings, 
    DeriveGeneric, 
    FlexibleInstances, 
    FlexibleContexts 
#-} 

module Nodes where 
import Lightningd
import Cli
import Jspec 
import GHC.Generics
import Control.Monad.IO.Class    
import System.IO    
import Data.List 
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe
import Control.Monad
import GHC.IORef
import Data.Text (Text) 
import System.IO.Unsafe
import System.IO
import Data.Foldable
import Control.Monad.Trans.State
import qualified Data.Map.Strict as H



type HT k v = H.Map k v

mapref :: IORef (HT NodeId Node) 
mapref = unsafePerformIO $ newIORef H.empty
{-# NOINLINE mapref #-} 

type NodeId = String
type ShortId = String

em = Node "" [] [] Nothing -- remove 
data Node = Node {
      nodeId :: NodeId
    , edges :: [Edge] 
    , crumbs :: [Crumb]
    , peer :: Maybe Peer
    } deriving (Show)    

data Peer = Peer { 
    bal :: Sat 
    } deriving (Show, Eq)

data Crumb = Crumb {
      hops :: Int
    , cost :: Fee 
    , neck :: Sat   
    , arrow :: ShortId
    } deriving (Generic, Show, Eq) 
instance ToJSON Crumb

data Edge = Edge {
      target :: NodeId
    , shortId :: ShortId
    , sats :: Sat 
    , fee :: Fee 
    } deriving (Show, Generic, Eq)
instance ToJSON Edge   

data Circle = Circle {
      peera :: Node 
    , peerb :: Node
    , chops :: [ShortId]
    , feeEst :: Fee 
    } deriving (Show, Generic) 


instance Ord Fee where 
    compare f1 f2 = compare (base f1 + ppm f1) (base f2 + ppm f2)   -- ?>?>

instance ToJSON Circle where 
    toJSON c = object [
              "fees" .= feeEst c 
            , "peera" .= ( (nodeId.peera) c <> howFull (peera c) )
            , "peerb" .= ( (nodeId.peerb) c <> howFull (peera c) )  
        ] 
howFull :: Node -> String 
howFull _ = "\n****************\n^^^^^^\n" 

loadNode :: Handle -> NodeId -> IO Node
loadNode handle i = do 
    g <- channelsbysource handle i 
    nodemap <- readIORef mapref 
    case g of 
        (Just (Correct (Res l _))) -> pure $ toNode l  
        otherwise                  -> pure em -- xxx 

lookupNode :: NodeId -> IO (Maybe Node) 
lookupNode i = (readIORef mapref) >>= (\map -> pure $ H.lookup i map) 

lookupNode' :: NodeId -> IO Node 
lookupNode' i = lookupNode i >>= \case  
    Just n -> pure n 
    Nothing -> pure em -- xxx 

getCircles :: Node -> IO [[NodeId]]
getCircles peera = mapM (genPath [] peera) (filter f $ crumbs peera)   
    where f c = hops c > 1

genPath :: [NodeId] -> Node -> Crumb -> IO [NodeId]
genPath p n c 
    | length ex == 0 = pure p 
    | hops c == 0 = pure p 
    | otherwise = (liftIO $ lookupNode (target.head $ ex)) >>= \case 
        Just n' -> genPath ((nodeId n):p) n' (nextC n') 
        Nothing -> pure p 
    where 
        ex = filter ((/= (arrow c)).shortId) $ edges n 
        nextC n'' = head $ filter (((==) (hops c - 1) ).hops) $  crumbs n''
 

followCrumb :: Crumb -> IO [NodeId] 
followCrumb c = undefined    

leaveCrumbs :: Handle -> Node -> IO ()  
leaveCrumbs handle n = go (Crumb 0 mempty maxBound "home")  n 
    where 
        go crumb n 
            | hops crumb > 21 = pure ()
        go crumb n@(Node i cx [] p) = do
            h <- readIORef mapref 
            liftIO $ writeIORef mapref $ (H.insert i (Node i cx (crumb : []) p) h) 
            liftIO $ mapM_ gogo cx
            where
                gogo c = (lookupNode (target c)) >>= \case   
                    Just node -> go (crumpdate crumb c) node
                    Nothing -> (liftIO $ loadNode handle (target c)) >>= \node -> 
                        go (crumpdate crumb c) node
        go crumb (Node i c b p) = do
            h <- readIORef mapref
            liftIO $ writeIORef mapref $ (H.insert i (Node i c (crumb : b) p) h)
            pure ()

crumpdate :: Crumb -> Edge -> Crumb 
crumpdate cr e =
    Crumb 
        (hops cr + 1) 
        (Fee ((base.cost) cr + (base.fee) e) (avger ((ppm.cost) cr) ((ppm.fee) e)) )
        (min (neck cr) (sats e))
        (shortId e) 

getSize = do 
    h <- readIORef mapref 
    pure $ H.foldr summarer (Mview 0 0 []) h
    where 
        summarer :: Node -> Mview -> Mview 
        summarer (Node _ ch cr _) (Mview s c h) =
            Mview (s + 1) (c + sum (map sats ch)) (addMin (hops.minimum $ cr) h)  
        addMin d h = case lookup d h of
            Nothing -> insert (d, 1) h
            Just j  -> insert (d, j + 1) (filter ((d /=).fst)  h)  

data Mview = Mview {
      mSize :: Int
    , mCapacity :: Sat 
    , histo :: [(Int, Int)]    
} deriving (Show) 

data Pick = Pick {
      top :: [Node]
    } deriving Show

getCandidates = do 
    h <- readIORef mapref 
    pure $ Pick $ toList $ H.filter inel h 
    where 
        inel :: Node -> Bool
        inel n@(Node i e b _)  
            | (hops.minimum $ b) < 11      = False  
            | length e < 5             = False
            | otherwise                = True

instance Ord Crumb where 
    compare bc rc = compare (hops bc) $ (hops rc)  
instance Monoid Fee where 
    mempty = Fee 0 0 
instance Semigroup Fee where 
    (Fee b p) <> (Fee b' p') = Fee b'' p''
        where 
            b'' = avger b b'
            p'' = avger p p'

avger n n' = (div (n + n') 2)
toNode :: ListChannels -> Node 
toNode (ListChannels x@(a:l)) = 
    let i = source a
    in Node i (map toEdge x) [] Nothing
toNode (ListChannels []) = em
toEdge :: Channel -> Edge 
toEdge c = Edge (destination c) (short_channel_id c) (satoshis c) (
    Fee (base_fee_millisatoshi c) (fee_per_millionth c) ) 

t = "/home/taylor/Projects/storm/t.log"
log' msg = System.IO.appendFile t msg 
