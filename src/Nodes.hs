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
import System.IO.Unsafe
import Data.Text (Text) 
import Data.Foldable
import qualified Data.Map.Strict as H
type HT k v = H.Map k v
log' = System.IO.appendFile "/home/taylor/Projects/storm/t.txt"
mapref :: IORef (HT NodeId Node) 
mapref = unsafePerformIO $ newIORef H.empty
{-# NOINLINE mapref #-} 

type Base = Int
type Ppm = Int
type Sat = Int 
type Msat = Int
type NodeId = String
type ShortId = String
data Fee = Fee {
      base :: Base 
    , ppm :: Ppm 
    } deriving (Show, Generic, Eq)  
instance ToJSON Fee
newtype Route = Route [Edge] deriving Generic 
instance ToJSON Route 
data Node = Node {
      nodeId :: NodeId
    , edges :: [Edge] 
    , crumbs :: Crumbs
    } deriving (Show)    
data Edge = Edge {
      target :: NodeId
    , shortId :: ShortId
    , sats :: Sat 
    , fee :: Fee 
    } deriving (Show, Generic)
instance ToJSON Edge   
em = Node "" [] []

loadNode :: Handle -> NodeId -> IO Node
loadNode handle i = do 
    g <- listchannels handle i 
    h <- readIORef mapref 
    case g of 
        (Just (Correct (Res l _))) -> 
            case toNode l of 
                (Just node) -> do 
                    liftIO $ log' $ nodeId node
                    liftIO $ writeIORef mapref (H.insert i node h)  
                    pure node 
                Nothing -> do 
                    liftIO $ log' $ i <> "    failde\n"
                    pure em
        otherwise -> pure em 

getNode :: Handle -> NodeId -> IO Node   
getNode handle k = do 
    nodemap <- readIORef mapref 
    case H.lookup k nodemap of
        (Just n) -> pure n
        Nothing ->  loadNode handle k  

data Crumb = Crumb {
      crumber :: Int
    , cost :: Fee 
    , neck :: Sat   
    , arrow :: ShortId 
    } deriving (Generic, Show, Eq) 
instance ToJSON Crumb
type Crumbs = [Crumb]  
leaveCrumbs :: Handle -> Node -> IO ()  
leaveCrumbs handle n = do
    go (Crumb 0 (Fee 0 0) 0 "") n 
    where 
        go crumb _ 
            | crumber crumb > 21 = pure () 
        go crumb n@(Node i cx []) = do 
            h <- readIORef mapref
            liftIO $ writeIORef mapref (H.insert i (Node i cx (crumb:[])) h)
            liftIO $ mapM_ channelGo cx
            where 
                channelGo c = do 
                    node <- liftIO $ getNode handle (target c) 
                    go (crumpdate crumb c) node 
        go crumb n@(Node i c b) = do 
            h <- readIORef mapref
            liftIO $ writeIORef mapref (H.insert i (Node i c (crumb : b)) h)

crumpdate :: Crumb -> Edge -> Crumb 
crumpdate cr e =
    Crumb 
        (crumber cr + 1) 
        (Fee ((base.cost) cr + (base.fee) e) ((ppm.cost) cr + (ppm.fee) e) ) 
        (min (neck cr) (sats e))
        (shortId e)

getSize = do 
    h <- readIORef mapref 
    pure $ H.size h
 
data Pick = Pick {
      top :: [Node]
    } deriving Show

getCandidates = do 
    h <- readIORef mapref 
    pure $ H.foldr picker (Pick []) h 
    where 
        picker :: Node -> Pick -> Pick
        picker n@(Node i e b) (Pick []) = Pick (n : [])   
        picker n@(Node i e b) (Pick top) 
            | length top < 1 = Pick (n : top) 
            | otherwise = Pick $ take 1 $ sort $ n : top

instance Eq Node where 
    (==) (Node i _ _ ) (Node i' _ _ ) = i == i'
instance Ord Node where 
    compare x y 
        | x == y = EQ 
    compare blue@(Node _ cb b) red@(Node _ cr r) = 
        let ncb = length b
            ncr = length r 
            neckdiff = div ((neck $ fold b) - (neck $ fold r)) 1000000 
       in compare (ncb ) (ncr)   
        
instance Ord Crumb where 
    compare bc rc = compare (crumber bc) $ (crumber rc)  
instance Monoid Crumb where 
    mempty = Crumb 0 mempty 0 "" 

instance Semigroup Fee where 
    (Fee b p) <> (Fee b' p') = Fee b'' p''
        where 
            b'' = (div (b + b') 2)
            p'' = (div (p + p') 2)


instance Monoid Edge where 
    mempty = Edge "" "" 0 mempty
instance Monoid Fee where 
    mempty = Fee 0 0 

instance Semigroup Edge where 
    (Edge o i s f) <> (Edge _ _ s' f') = Edge o i (s + s') (f <> f') 

instance Semigroup Crumb where 
    (Crumb c f n a) <> (Crumb c' f' n' a') = Crumb 
        (min c c') 
        (f <> f')  
        ((+) n n')
        a  
 
toNode :: ListChannels -> Maybe Node 
toNode (ListChannels x@(a:_)) = 
    let i = source a
    in Just $ Node i (map toEdge x) []
toNode (ListChannels []) = Nothing 
toEdge c = Edge (destination c) (short_channel_id c) (satoshis c) (
    Fee (base_fee_millisatoshi c) (fee_per_millionth c) ) 
(*&*) :: NodeId -> [Route] -> Bool
a *&* ((Route []):ssx) = a *&* ssx
a *&* ((Route (x:sx)):ssx) = a /= (target x) && a *&* ((Route sx) : ssx)  
addEdge :: Edge -> Route -> Route 
addEdge e (Route r) =  Route (e:r) 
toRoute :: Edge -> Route
toRoute e = Route $ e : [] 

calcTotalFee :: Route -> Msat -> Msat 
calcTotalFee r amt = go (0::Msat) r amt 
        where 
            go :: Msat -> Route -> Msat -> Msat
            go z (Route [])  _ = z 
            go z (Route ((Edge _ _ _ (Fee b p)):r)) a = go (z + calcFee b p a) (Route r) a  
            m :: Int 
            m = 1000000
            calcFee :: Msat -> Msat -> Msat -> Msat
            calcFee base ppm amt = amt * m * ppm `div` (m * m) + base 
