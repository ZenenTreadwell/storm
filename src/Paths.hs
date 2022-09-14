{-# LANGUAGE 
    DuplicateRecordFields, 
    LambdaCase, 
    DeriveGeneric, 
    OverloadedStrings
#-}


module Paths where 

import Lightningd
import Cli
import Graph
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
        , "route" .= cRoute 1000000 p
        ]

logg = System.IO.appendFile "/home/o/Desktop/logy"

initP :: [Channel] -> PathInfo 
initP p = P (0) (Fee 0 0) maxBound p 

findPaths :: Node -> Node -> IO [PathInfo]
findPaths n v 
    | n == v = pure [] 
findPaths n v = do 
    gra <- readIORef graphRef 
    case (match n gra, 
          match v gra) of 
              ( (Just c@(_, _, _, outy), g) ,
                (Just d@(inny, _,_,_) , _ )   ) -> do
                    -- XXX
                    -- large nodes take hours
                    -- n*m over lesp
                    -- XXX 
                    pure $ sort 
                         $ map (\s -> foldr c2 (initP s) s)  
                         $ map (\(f, l, x) -> [f] ++ (lp (unLPath l)) ++ [x]) 
                         $ concat  
                         $ map (\(x,f) -> map (\o -> (fst o , f (snd o), x)) outy )
                         $ map (\j -> (fst j , flip3 lesp g (snd j))) inny
              otherwise -> pure []



-- much faster
-- misses longer paths with possibly lower fees 
bftFindPaths :: Node -> Node -> IO [PathInfo]
bftFindPaths n v = do 
    g <- readIORef graphRef
    case (match n g) of 
        (Just c@(_, _, _, outy), g) -> do 
            pure 
            $ sort    
            $ map (\s -> foldr c2 (initP s) s)  
            $ map (\(f,p) -> [f] <> ( (reverse.lp.unLPath) p) ) 
            $ map (\(c, n') -> (c, getLPath n' rtree)) outy  
            where 
                rtree = lbft v g 
        otherwise -> pure []    


pp :: [LNode Channel] -> [Channel]
pp [] = [] 
pp p = map snd p  

-- get channels, throw out top
lp :: [LNode Channel] -> [Channel]
lp [] = [] 
lp (p:px) = map snd px  

c2 :: Channel -> PathInfo -> PathInfo
c2 e c = P
    (hops c + 1)
    (Fee
        ( (base.cost) c + base_fee_millisatoshi e )
        ( ((hops c + 1)*((ppm.cost) c) + fee_per_millionth e ) `div` (hops c + 2) ))-- accurate?
    (min (neck c) ((amount_msat::Channel->Msat) e ) )
    (path c)


instance Ord Fee where 
    compare a b = compare (base a + ppm a) (base b + ppm b)

instance Eq PathInfo where 
    (==) a b = (base.cost $ a) == (base.cost $ b) && (ppm.cost $ a) == (ppm.cost $ b)

instance Ord PathInfo where 
    compare a b = compare (cost b) (cost a) 

cRoute :: Msat -> PathInfo -> [Route] 
cRoute a c | a > neck c = [] 
cRoute a c = reverse $ foldr (c3 a) [] $ reverse . path $ c 
c3 :: Msat -> Channel -> [Route] -> [Route] 
c3 a c r = Route 
    (destination c) 
    ((short_channel_id::Channel->String) c)
    (getDirect (source c) (destination c))
    (getAmount a c r)
    (getDelay c r) 
    "tlv" 
    : r 
    where 
        getDirect a b = if readHex a < readHex b then 0 else 1
        getDelay e [] = (delay::(Channel->Int)) e
        getDelay e (r:_) = (delay::(Channel->Int)) e + (delay::(Route->Int)) r         
        getAmount :: Msat -> Channel -> [Route] -> Msat 
        getAmount a _ [] = a
        getAmount _ e (r:_) = (amount_msat::Route->Msat) r + (base_fee_millisatoshi e) -- xxx ppm   


