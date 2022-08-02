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

type Sat = Int 
type Msat = Int
type NodeId = String
type ShortId = String
data Fee = Fee {
      base :: Int 
    , ppm :: Int 
    } deriving (Show, Generic, Eq)  
instance ToJSON Fee

em = Node "" [] [] Nothing -- remove need for this
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
      crumber :: Int
    , cost :: Fee 
    , neck :: Sat   
    , arrow :: Edge
    } deriving (Generic, Show, Eq) 
instance ToJSON Crumb

data Edge = Edge {
      target :: NodeId
    , shortId :: ShortId
    , sats :: Sat 
    , fee :: Fee 
    } deriving (Show, Generic, Eq)
instance ToJSON Edge   

loadNode :: Handle -> NodeId -> IO Node
loadNode handle i = do 
    g <- listchannels handle i 
    nodemap <- readIORef mapref 
    case g of 
        (Just (Correct (Res l _))) -> pure $ toNode l  
        otherwise                   -> pure em 

getNode :: Handle -> NodeId -> IO Node   
getNode handle k = do 
    nodemap <- readIORef mapref 
    case H.lookup k nodemap of
        (Just n) -> pure n
        Nothing ->  loadNode handle k  

getNode' :: NodeId -> IO Node 
getNode' k = do
    nodemap <- readIORef mapref 
    case H.lookup k nodemap of
        (Just n) -> pure n
        Nothing -> pure em 

getCircles fulc = undefined --do --
  --  liftIO $ System.IO.appendFile t $ "search crumbs " <> (show.length.crumbs) fulc 
  --  mapM (followCrumb []) $ crumbs fulc

followCrumb p c 
    | crumber c == 0 = do 
        liftIO $ mapM (\s -> getNode' s) p
    | otherwise = do 
        n <- getNode' $ target e
        followCrumb (nxtP n) (nxtCrumb n) 
        where 
           e = arrow c 
           nxtP n = (nodeId n):p 
           nxtCrumb n = head $ sort $ crumbs n
     
leaveCrumbs :: Handle -> Node -> IO ()  
leaveCrumbs handle n = go (Crumb 0 mempty maxBound (Edge "home" "home" 0 mempty) )  n 
    where 
        go crumb _ 
            | crumber crumb > 21 = pure ()
        go crumb n@(Node i cx [] _) = do
            h <- readIORef mapref
            liftIO $ writeIORef mapref (H.insert i (Node i cx (crumb:[]) Nothing) h)
            liftIO $ mapM_ channelGo cx
            where
                channelGo c = do 
                    node <- liftIO $ getNode handle (target c)
                    go (crumpdate crumb c node) node
        go crumb n@(Node i c b _) = do
            h <- readIORef mapref
            liftIO $ writeIORef mapref (H.insert i (Node i c (crumb : b) Nothing) h )

crumpdate :: Crumb -> Edge -> Node -> Crumb 
crumpdate cr e n =
    Crumb 
        (crumber cr + 1) 
        (Fee ((base.cost) cr + (base.fee) e) (avger ((ppm.cost) cr) ((ppm.fee) e)) )
        (min (neck cr) (sats e))
        e 

head' [] = undefined 
head' (x:_) = x

getSize = do 
    h <- readIORef mapref 
    pure $ H.foldr summarer (Mview 0 0 []) h
    where 
        summarer :: Node -> Mview -> Mview 
        summarer (Node _ ch cr _) (Mview s c h) =
            Mview (s + 1) (c + sum (map sats ch)) (addMin (crumber.minimum $ cr) h)  
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
            | crumber (head' b) < 11    = False  
            | length e < 5             = False
            | otherwise                = True

instance Ord Crumb where 
    compare bc rc = compare (crumber bc) $ (crumber rc)  
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
--(*&*) :: NodeId -> [Route] -> Bool
--a *&* ((Route []):ssx) = a *&* ssx
--a *&* ((Route (x:sx)):ssx) = a /= (target x) && a *&* ((Route sx) : ssx)  
--addEdge :: Edge -> Route -> Route 
--addEdge e (Route r) =  Route (e:r) 
--calcTotalFee r amt = go (0::Msat) r amt 
--    where 
--        go :: Msat -> Route -> Msat -> Msat
--        go z (Route [])  _ = z 
--        go z (Route ((Edge _ _ _ (Fee b p)):r)) a = go (z + calcFee b p a) (Route r) a  
--        m :: Int 
--        m = 1000000
--        calcFee :: Msat -> Msat -> Msat -> Msat
--        calcFee base ppm amt = amt * m * ppm `div` (m * m) + base 
t = "/home/taylor/Projects/storm/t.log"
