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

type Gra = Gr Node' Edge'

graphRef :: IORef Gra
graphRef = unsafePerformIO $ newIORef empty 

data Node' = N {
      pubkey :: Maybe String
    , alias' :: Maybe String
    , crumb :: Maybe [Crumb] }

fromId :: String -> LNode Node' 
fromId s = (getNodeInt s, N (Just s) Nothing Nothing) 

data Edge' = E { 
      short :: String 
    , collat :: Sat
    , fees :: Fee
    --, delay  
    } deriving (Generic, Show, Eq)
instance ToJSON Edge' 

toEdge' :: Channel -> LEdge Edge' 
toEdge' c = (
      (getNodeInt $ source c)
    , (getNodeInt $ destination c),
      ((E  
        (short_channel_id c) 
        (satoshis c) 
        (Fee (base_fee_millisatoshi c) (fee_per_millionth c))  
       ))
    ) 
   
toNode' :: NodeInfo -> LNode Node'
toNode' n = ((getNodeInt $ nodeid n), N (Just $ nodeid n) (ali n) Nothing )
    where ali = alias :: NodeInfo -> Maybe String

toNode'' :: String -> LNode Node'
toNode'' s = (getNodeInt s, N (Just s) Nothing Nothing )  

findLargest gra = ufold islargest (0, 0) gra 

islargest nc c = if (length.suc') nc > fst c then ( (length.suc') nc , node' nc )  
                                            else c 
--removeLargest gra = case match (findLargest gra) gra of 
  --  (Just c, g) -> g 
  --  (Nothing, _) -> empty


segponants gra stop = undefined

getNodeInt :: String -> Node
getNodeInt = fst.head.readHex.filter isHexDigit 

deadends :: Context Node' Edge' -> Maybe (Context Node' Edge') 
deadends c =
    let inny = indeg' c == 0 
        outy = outdeg' c == 0
    in if inny || outy  
        then Nothing 
        else Just c

loadGraph :: IO ()
loadGraph = (allchannels) >>= \case 
    (Just (Correct (Res a' _))) -> do 
        -- why does this use so much memory (++8GB++) 
        gra <- pure $ foldr buildg empty listChan
        liftIO $ writeIORef graphRef $ gra
        where
              listChan = (channels :: ListChannels -> [Channel] ) a'   
              buildg :: Channel -> Gra -> Gra
              buildg c g = 
                let e = toEdge' c    
                    a = source c
                    b = destination c 
                in case (gelem (getNodeInt a) g, gelem (getNodeInt b) g) of 
                  (True, True) -> insEdge e g
                  (False, True) -> insEdge e (insNode (fromId a) g)
                  (True, False) -> insEdge e (insNode (fromId b) g) 
                  (False, False) -> insEdge e (insNode (fromId a) (insNode (fromId b) g)) 
    otherwise -> pure ()   

findCircles :: Node -> IO [Crumb]
findCircles n = do 
    g <- readIORef graphRef
    case match n g of
        (Just home, g') -> pure []

findPaths :: Node -> Node -> IO [Crumb]
findPaths n v 
    | n == v = findCircles n
findPaths n v = do 
    gra <- readIORef graphRef 
    case (match n gra, 
          match v gra) of 
              ( (Just c@(_, _, _, outy), gra' ) ,
                (Just d@(inny, _,_,_) , _ )     ) -> do 
                  pure $ concat $ map (findPath gra' v inny) outy
              otherwise -> pure []   
    where   
        --findPath :: Gra -> Node -> LEdge Edge' -> Crumb 
        findPath  = undefined -- g v' inny (_, n', e) = foldr crumpdate (initCrumb e) $ concat $ map (\(q,_) -> reProcess g $ esp n' q g) inny 


reProcess :: Gra -> Path -> [(Edge', Node')]
reProcess _ [] = [] 
reProcess _ (x : []) = [] 
reProcess g (x:y:z) = (doge g x y, gode g x) : reProcess g z

gode g x = fromJust $ lab g x
doge g x y = snd $ head $ filter ((== y).fst) $ lsuc' $ context g x 

calcCapacity :: Gra -> Sat -> Sat
calcCapacity g a 
    | isEmpty g = a 
    | otherwise = case matchAny g of 
        (c, g') -> calcCapacity g' $ a + (sum $ map (collat.fst) 
                                              $ nubBy (\a b -> (short.fst) a == (short.fst) b) 
                                              $ lneighbors' c) 


data Crumb = C {
      hops :: Int
    , cost :: Fee 
    , neck :: Sat   
    , arrow :: [Route]
    } deriving (Generic, Show, Eq) 
instance ToJSON Crumb where 
    toJSON cr = object [
        ("route" .= (arrow cr) )  
       ]

toRoute :: (Edge', Node') -> Route
toRoute (e,n) = undefined

initCrumb :: (Edge', Node') -> Crumb 
initCrumb e = C (1) (fees $ fst e) (collat $ fst e) (toRoute e : [])

crumpdate :: (Edge', Node') -> Crumb -> Crumb 
crumpdate e cr = C 
    (hops cr + 1) 
    (Fee 
        ((base.cost) cr + (base.fees) (fst e) ) 
        ((((ppm.cost) cr) + ((ppm.fees) $ fst e)) `div` 2)) -- not accurate overweights newest
    (min (neck cr) (collat $ fst e))
    (toRoute e : (arrow cr))


