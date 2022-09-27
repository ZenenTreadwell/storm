{-# LANGUAGE 
    DuplicateRecordFields, 
    LambdaCase, 
    DeriveGeneric, 
    OverloadedStrings
#-}
module Cln.Paths where 
import Cln.Types 
import Cln.Client
import Cln.Graph
import System.IO
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query
import Data.Graph.Inductive.Query.BFS
import Data.Graph.Inductive.Internal.RootPath
import Data.Aeson 
import GHC.Generics
import Numeric 
import GHC.IORef
import Data.Function.Flip
import Data.List

data PathInfo = P {
      hops :: Int
    , cost :: Fee
    , neck :: Sat
    , path :: [Channel]
    } deriving (Generic, Show)
instance ToJSON PathInfo where
   toJSON p = object [
          "hops" .= hops p
        , "cost" .= cost p
        , "neck" .= neck p
        , "route" .= createRoute 1000000000 p
        ]

createRoute :: Msat -> PathInfo -> [Route] 
createRoute a c | a > neck c = []  
createRoute a c = foldr (c3 a) [] $ pairUp $ path c 

initP :: [Channel] -> PathInfo 
initP p = P (0) (Fee 0 0) maxBound p 

bftFindPaths :: Node -> Node -> IO [PathInfo]
bftFindPaths n v = do 
    gra <- readIORef graphRef
    case ( match n gra                ,  match v gra) of 
        ( (Just (_, _, _, outy), g) , (Just (inny, _,_,_) , _ ) ) -> 
            bftFP g oo ii
            where oo = filter (\(f, n') -> v /= n') outy --filter direct channel 
                  ii = filter (\(f, n') -> n /= n') inny
        otherwise -> pure []    

bftFP :: Gra -> Adj Channel -> Adj Channel -> IO [PathInfo]
bftFP g outy inny = pure
    $ sort
    $ map summarizePath   
    $ map repackPath
    $ filter (\(_,p,_)-> (length.unLPath) p > 0)
    $ map (\(a,pf,b) -> (a,pf g, b) )
    $ concat
    $ map (finPaths inny) 
    $ map outTree outy

finPaths :: Adj Channel -> (Channel, Gra -> LRTree Channel) -> [ (Channel, Gra -> LPath Channel, Channel) ] 
finPaths i outT = map (fp outT) i
fp :: (Channel, Gra -> LRTree Channel) -> (Channel,Node) -> (Channel, Gra -> LPath Channel, Channel)
fp (f, t) (e, v) = (f, (getLPath v).t , e) 

outTree :: (Channel, Node) -> (Channel, Gra -> LRTree Channel) 
outTree o = (fst o, lbft $ snd o)   

repackPath :: (Channel, LPath Channel, Channel) -> [Channel]
repackPath (f,p,e) = [f] <> ((lp.unLPath) p) <> [e]
lp :: [LNode Channel] -> [Channel]
lp [] = [] 
lp (p:px) = map snd px

summarizePath :: [Channel] -> PathInfo
summarizePath s@(_:sx) = foldr c2 (initP s) sx
c2 :: Channel -> PathInfo -> PathInfo 
c2 e c = P
    (hops c + 1)
    (Fee ( (base.cost) c + base_fee_millisatoshi e )
         ( ((hops c)*((ppm.cost) c) + fee_per_millionth e ) `div` (hops c + 1) ))
    (min (neck c) ((amount_msat::Channel->Msat) e ) )
    (path c)

getCircles :: Node -> IO [PathInfo]
getCircles n = do
    g <- readIORef graphRef
    case match n g of 
        (Just (inny, n',z, outy), g') -> bftFP g' outy inny
        otherwise -> pure [] 

instance Ord Fee where 
    compare a b = compare (base a + ppm a) (base b + ppm b)
instance Eq PathInfo where 
    (==) a b = (base.cost $ a) == (base.cost $ b) && (ppm.cost $ a) == (ppm.cost $ b)
instance Ord PathInfo where 
    compare a b = compare (cost a) (cost b) 

pairUp :: [Channel] -> [(Channel,Channel)] 
pairUp [] = [] 
pairUp (a:[]) = [(a,a)] 
pairUp (a:b) = (a,head b) : pairUp b
c3 :: Msat -> (Channel, Channel)  -> [Route] -> [Route] 
c3 a (cp, c)  r = Route 
    ((destination::Channel->String) cp) 
    ((short_channel_id::Channel->String) cp)
    (getDirect (source cp) ((destination::Channel->String) cp))
    (getAmount a c r)
    (getDelay c r) 
    "tlv" 
    : r 
getDirect :: String -> String -> Int
getDirect a b = if readHex a < readHex b then 0 else 1
getDelay :: Channel -> [Route] -> Int
getDelay e [] = 9
getDelay e (r:_) = (delay::Route->Int) r + (delay::Channel->Int) e         
getAmount :: Msat -> Channel -> [Route] -> Msat
getAmount a e [] = a
getAmount a e r = (+)
    (maximum $ map (amount_msat::Route->Msat) r)
    (base_fee_millisatoshi e + ( div (1000000 * a * (fee_per_millionth e)) (1000000*1000000)))