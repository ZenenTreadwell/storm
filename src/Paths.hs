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
              ( (Just (_, _, _, outy), g) ,
                (Just (inny, _,_,_) , _ )   ) -> do
                    -- XXX n*m over lesp too slow
                    pure $ sort 
                         $ map (\s -> foldr c2 (initP s) s)  
                         $ map (\(f, l, x) -> [f] ++ (lp (unLPath l)) ++ [x]) 
                         $ concat  
                         $ map (\(x,f) -> map (\o -> (fst o , f (snd o), x)) outy )
                         $ map (\j -> (fst j , flip3 lesp g (snd j))) inny
              otherwise -> pure []

bftFindV1 :: Node -> Node -> IO [PathInfo]
bftFindV1 n v = do 
    g <- readIORef graphRef
    case (match n g) of 
        (Just (_, _, _, outy), g) -> do 
            pure 
            $ sort    
            $ map (\s -> foldr c2 (initP s) s)  
            $ map (\(f,p) -> [f] <> ( (reverse.lp.unLPath) p) ) 
            $ map (\(c, n') -> (c, getLPath n' rtree)) outy  
            where 
                rtree = lbft v g 
        otherwise -> pure []    

bftFindPaths :: Node -> Node -> IO [PathInfo]
bftFindPaths n v = do 
    gra <- readIORef graphRef
    case ( match n gra                ,  match v gra) of 
        ( (Just (_, _, _, outy), g) , (Just (inny, _,_,_) , _ ) ) -> do 
            pure
            $ sort    
            $ map (\s -> foldr c2 (initP s) s)  
            $ map (\(f,p, e) -> [f] <> ( (lp.unLPath) p) <> [e] ) 
            $ filter (\(_,p,_)-> (length.unLPath) p > 0)
            $ concat
            $ map (\(e, v') -> map (\(f,t) -> (f, getLPath v' t, e)) rtrees) inny2  
            where 
                rtrees = map (\(f, n') -> (f, lbft n' g)) outy2
                -- direct channel broke it ? 
                outy2 = filter (\(f, n') -> v /= n') outy
                inny2 = filter (\(f, n') -> n /= n') inny
        otherwise -> pure []    

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
    compare a b = compare (cost a) (cost b) 

cRoute :: Msat -> PathInfo -> [Route] 
cRoute a c | a > neck c = [] 
cRoute a c = foldr (c3 a) [] $ path $ c 

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

