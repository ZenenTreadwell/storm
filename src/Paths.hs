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
    , path :: [LNode Channel]
    } deriving (Generic, Show) 
instance ToJSON PathInfo where 
   toJSON p = object [
        "hops" .= hops p
        , "cost" .= cost p 
        , "neck" .= neck p
        , "route" .= cRoute 700700 p
        ]


logg = System.IO.appendFile "/home/o/Desktop/logy"

initPathInfo :: [LNode Channel] -> PathInfo 
initPathInfo p = P (0) (Fee 0 0) maxBound p 


-- pathfindingrules


findPaths :: Node -> Node -> IO [PathInfo]
findPaths n v 
    | n == v = pure [] 
findPaths n v = do 
    gra <- readIORef graphRef 
    case (match n gra, 
          match v gra) of 
              ( (Just c@(_, _, _, outy), g) ,
                (Just d@(inny, _,_,_) , _ )   ) -> do
                       pure
                       -- $ sort
                       $ map calcPathInfo
                       -- $map (attachOut outy) 
                       -- $map (attachIn inny) 
                       -- $ filter (\s -> length s /= 0 ) 
                       $ map unLPath
                       $ concat -- a 
                       -- $ [[LPath]]
                       $ map (\f -> map (f.snd) outy) 
                       $ map ((flip3 lesp g).snd) inny
                       where 
                           attachIn [] c = c 
                           attachIn (x:xs) c = if ( (source.head $ c) == (destination.fst $ x) ) 
                               then (fst x) : c 
                               else attachIn xs c
                           attachOut [] c = c 
                           attachOut (x:xs) c = if ((destination.last $ c) == (source.fst $ x) ) 
                               then (++) c [fst x]  
                               else attachOut xs c    
              otherwise -> pure []

calcPathInfo :: [LNode Channel] -> PathInfo
calcPathInfo p = foldr c2 (initPathInfo p) (map snd p) 

instance Ord Fee where 
    compare a b = compare (base a + ppm a) (base b + ppm b)

instance Eq PathInfo where 
    (==) a b = (base.cost $ a) == (base.cost $ b) && (ppm.cost $ a) == (ppm.cost $ b)

instance Ord PathInfo where 
    compare a b = compare (cost a) (cost b) 

c2 :: Channel -> PathInfo -> PathInfo
c2 e c = P
    (hops c + 1)
    (Fee
        ( (base.cost) c + base_fee_millisatoshi e )
        ( ((hops c + 1)*((ppm.cost) c) + fee_per_millionth e ) `div` (hops c + 2) ))-- accurate?
    (min (neck c) ((amount_msat::Channel->Msat) e ) )
    (path c)


c3 :: Msat -> LNode Channel -> [Route] -> [Route] 
c3 a (n, c) r = Route 
    (source c) 
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
        getAmount _ e (r:_) = (amount_msat::Route->Msat) r - (base_fee_millisatoshi e) -- xxx ppm   

cRoute :: Msat -> PathInfo -> [Route] 
cRoute a c | a > neck c = [] 
cRoute a c = reverse $ foldr (c3 a) [] $ reverse . path $ c 


fromRoute :: [Route] -> PathInfo 
fromRoute = undefined
