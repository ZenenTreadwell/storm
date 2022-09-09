{-# LANGUAGE 
    DuplicateRecordFields, 
    LambdaCase, 
    DeriveGeneric, 
    OverloadedStrings
#-}

module Graph where 

import Numeric 
import Lightningd 
import Cli
import Jspec
import Data.Aeson 
import Control.Monad
import System.IO 
import System.IO.Unsafe
import GHC.IORef
import GHC.Generics
import Control.Monad.IO.Class
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query
import Data.Graph.Inductive.Tree
import GHC.Real
import Data.List
import Data.Maybe 
import Data.Char
import Data.Function.Flip

graphRef :: IORef Gra
graphRef = unsafePerformIO $ newIORef empty 

type Edge' = Channel
type Node' = NodeInfo
type Gra = Gr Node' Edge'
type Cxt = Context Node' Edge'

getNodeInt :: String -> Node
getNodeInt s = case readHex.filter isHexDigit $ s of 
    ([]) -> 0 
    (x:_)-> fst x 

toLNode' :: NodeInfo -> LNode NodeInfo 
toLNode' ni = ( (getNodeInt.nodeid) ni , ni)

toLEdge' :: Channel -> LEdge Channel
toLEdge' c = ( (getNodeInt.source) c  , (getNodeInt.destination) c, c )

mockInfo s = NodeInfo s Nothing Nothing Nothing Nothing Nothing Nothing 

loadGraph :: IO ()
loadGraph = (allchannels) >>= \case 
    (Just (Correct (Res listchannels _))) -> 
           liftIO $ writeIORef graphRef $ mkGraph (map toLNode' nx) (map toLEdge' cx) 
           where cx = (channels::ListChannels->[Channel]) listchannels 
                 nx = map mockInfo $ nub $ (map source cx) <> (map destination cx)   
    otherwise -> pure () 






    -- wanted to load node info but rpc req fail ?? XXX
    --(allnodes) >>= \case --  XXX INVALID
      --  (Just (Correct (Res listnodes _))) -> do 


-- loadBfs :: String -> Int -> IO () 
--        gra <- pure $ foldr buildg empty listChan
--        liftIO $ writeIORef graphRef $ gra
--        where
--              listChan = (channels :: ListChannels -> [Channel] ) a'   
--              buildg :: Channel -> Gra -> Gra
--              buildg c g = 
--                let a = source c
--                    b = destination c 
--                in case (gelem (getNodeInt a) g, gelem (getNodeInt b) g) of 
--                  (True, True) -> insEdge e g
--                  (False, True) -> insEdge e (insNode (fromId a) g)
--                  (True, False) -> insEdge e (insNode (fromId b) g) 
--                  (False, False) -> insEdge e (insNode (fromId a) (insNode (fromId b) g)) 
--    otherwise -> pure ()   


type Path' = [(Edge', Node')]

data PathInfo = C {
      hops :: Int
    , cost :: Fee 
    , neck :: Sat   
    , path :: Path'
    } deriving (Generic, Show) 
instance ToJSON PathInfo 

findCircles :: Node -> IO [PathInfo]
findCircles n = do 
    g <- readIORef graphRef
    case match n g of
        (Just home, g') -> pure []

findPaths :: Node -> Node -> IO [PathInfo]
findPaths n v 
    | n == v = findCircles n
findPaths n v = do 
    gra <- readIORef graphRef 
    case (match n gra, 
          match v gra) of 
              ( (Just c@(_, _, _, outy), g' ) ,
                (Just d@(inny, _,_,_) , _ )   ) -> do 
                  pure $ map calcPathInfo                                -- Path' -> PathInfo
                       $ map (\p -> reProcess gra ([n] <> p <> [v]) ) -- Path -> Path' 
                       $ filter (not.null) 
                       $ concat                                       -- [[Path]] -> [Path] 
                       $ map (\f -> map (f.snd) outy)                 -- Node-> [Path]  
                       $ map ((flip3 esp g').snd) inny                -- Node -> (Node -> Path)
              otherwise -> pure []   

reProcess :: Gra -> Path -> Path'
reProcess _ [] = [] 
reProcess _ (x : []) = [] 
reProcess g (x:y:z) = (doge g x y, gode g x) : reProcess g z
    where 
        gode g x = fromJust $ lab g x
        doge g x y = snd $ head $ filter ((== y).fst) $ lsuc' $ context g x 

calcPathInfo :: Path' -> PathInfo
calcPathInfo p' = foldr c2 (initPathInfo p') p' 

c2 :: (Edge', Node') -> PathInfo -> PathInfo 
c2 (e, n) c = C 
    (hops c + 1) 
    (Fee 
        ((base.cost) c + base_fee_millisatoshi e ) 
        ( ((hops c)*((ppm.cost) c) + fee_per_millionth e ) `div` (hops c + 1) )) -- accurate?
    (min (neck c) (satoshis e) ) 
    (path c)    

calcRoute :: Msat -> PathInfo -> [Route] 
calcRoute a c | a > neck c = [] 
calcRoute a c = 
    let p' = reverse . path $ c 
    in reverse $ foldyish [] p'
    where 
          foldyish :: [Route] -> Path' -> [Route] 
          foldyish r [] = r 
          foldyish r (x:[]) = r 
          foldyish r ((xe, xn):y@(ye, yn):z) = Route 
              (nodeid yn) (short_channel_id ye)
              (getDirect (nodeid xn) (nodeid yn))
              (getAmount a r ye)
              ("xxxmsat") 
              (getDelay r ye) 
              "tlv" 
              : r

          getDirect a b = if readHex a < readHex b then 0 else 1 -- right? 
          getDelay [] e = (delay::(Channel->Int)) e 
          getDelay (r:_) e = (delay::(Channel->Int)) e + (delay::(Route->Int)) r -- rly ghc         
          getAmount a [] _ = a
          getAmount _ (r:_) e = a 


calcCapacity :: Gra -> Sat -> Sat
calcCapacity g a 
    | isEmpty g = a 
    | otherwise = case matchAny g of 
        (c, g') -> calcCapacity g' $ a + (sum $ map (satoshis.fst) 
                                              $ nubBy (\x y -> (short_channel_id.fst) x == (short_channel_id.fst) y) 
                                              $ lneighbors' c) 

deadends :: Context Node' Edge' -> Maybe (Context Node' Edge') 
deadends c =
    let inny = (indeg' c) == 0 
        outy = (outdeg' c) == 0
    in if (inny || outy)   
        then Nothing 
        else Just c

initPathInfo :: Path' -> PathInfo 
initPathInfo p = C (0) (Fee 0 0) maxBound p 

