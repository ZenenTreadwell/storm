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
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query
import Data.Aeson 
import GHC.Generics
import Numeric 
import GHC.IORef 
import Data.Function.Flip

data PathInfo = P {
      hops :: Int
    , cost :: Fee 
    , neck :: Sat   
    , path :: LPath Channel
    } deriving (Generic, Show) 
instance ToJSON PathInfo where 
   toJSON p = object [
        "hops" .= hops p
        , "cost" .= cost p 
        , "neck" .= neck p
        ]

initPathInfo :: LPath Channel -> PathInfo 
initPathInfo p = P (0) (Fee 0 0) maxBound p 

findPaths :: Node -> Node -> IO [PathInfo]
findPaths n v 
    | n == v = pure [] 
findPaths n v = do 
    gra <- readIORef graphRef 
    case (match n gra, 
          match v gra) of 
              ( (Just c@(_, _, _, outy), g' ) ,
                (Just d@(inny, _,_,_) , _ )   ) -> do
                  pure $ map calcPathInfo
                       -- $ filter (not.null)
                       -- $ map unLPath
                       $ concat 
                       $ map (\f -> map (f.snd) outy)
                       $ map ((flip3 lesp g').snd) inny
              otherwise -> pure []

calcPathInfo :: LPath Channel -> PathInfo
calcPathInfo p = foldr c2 (initPathInfo p) (unLPath p) 

c2 :: (Node, Channel) -> PathInfo -> PathInfo
c2 (_ , e) c = P
    (hops c + 1)
    (Fee
        ( (base.cost) c + base_fee_millisatoshi e )
        ( ((hops c)*((ppm.cost) c) + fee_per_millionth e ) `div` (hops c + 1) ))-- accurate?
    (min (neck c) (satoshis e) )
    (path c)

--CalcRoute :: Msat -> PathInfo -> [Route] 
--CalcRoute a c | a > neck c = [] 
--CalcRoute a c = 
--    let p' = reverse . path $ c 
--    in reverse $ foldyish [] p'
--    where 
--          foldyish r [] = r 
--          foldyish r (x:[]) = r 
--          foldyish r ((xn, xe):y@(yn, ye):z) = Route 
--              (nodeid yn) (short_channel_id ye)
--              (getDirect (nodeid xn) (nodeid yn))
--              (getAmount a r ye)
--              ("xxxmsat") 
--              (getDelay r ye) 
--              "tlv" 
--              : r
--          getDirect a b = if readHex a < readHex b then 0 else 1 -- right? 
--          getDelay [] e = (delay::(Channel->Int)) e 
--          getDelay (r:_) e = (delay::(Channel->Int)) e + (delay::(Route->Int)) r -- rly ghc         
--          getAmount a [] _ = a
--          getAmount _ (r:_) e = a 
--
