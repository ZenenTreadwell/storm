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

--- XXX
graphRef :: IORef (Gra)
graphRef = unsafePerformIO $ newIORef empty 
{-# NOINLINE graphRef #-}

data Node' = N {
      pubkey :: Maybe String
    , alias' :: Maybe String
    , crumb :: Maybe [Crumb] }
em = N Nothing Nothing Nothing


data Edge' = E { 
      short :: String 
    , collat :: Sat
    , fees :: Fee   
    , mysat :: Maybe Sat 
    , htlcs :: Maybe [Int] }
    deriving (Generic, Show, Eq)
instance ToJSON Edge' 

toEdge' :: Channel -> LEdge Edge' 
toEdge' c = (
    (getNodeInt $ source c), 
    (getNodeInt $ destination c),
    (E  
        (short_channel_id c) 
        (satoshis c) 
        (Fee (base_fee_millisatoshi c) (fee_per_millionth c))  
        Nothing 
        Nothing
    )) 
    
toNode' :: NodeInfo -> LNode Node'
toNode' n = ((getNodeInt $ nodeid n), N (Just $ nodeid n) (ali n) Nothing )
    where ali = alias :: NodeInfo -> Maybe String

getNodeInt :: String -> Node
getNodeInt = fst.head.readHex.filter isHexDigit 

loadGraph :: Handle -> IO (Gra)
loadGraph h = (allchannels h) >>= \case 
    (Just (Correct (Res a' _))) -> do 
        gra <- pure $ foldr insertEdge empty a''
        liftIO $ writeIORef graphRef $ gra
        pure gra
        where
              insertEdge :: LEdge Edge' -> Gra -> Gra  
              insertEdge e@(x,y,z) b = case (gelem y b, gelem x b) of 
                  (True, True) -> insEdge e b
                  (False, True) -> insEdge e (insertEmpty y b)
                  (True, False) -> insEdge e (insertEmpty x b) 
                  (False, False) -> insEdge e (insertEmpty x (insertEmpty y b))  
              insertEmpty :: Node -> Gra -> Gra 
              insertEmpty n g = insNode (n, em) g
              a'' = map toEdge' $ (channels :: ListChannels -> [Channel] ) a'
    otherwise -> pure empty  

findPaths :: Node -> Node -> IO [Crumb]
findPaths n v = do 
    gra <- readIORef graphRef 
    case match n gra of 
        (Just c, gra') -> do 
            pure $ map (findPath gra' v) $ out' c
        (Nothing, _) -> pure []   
    where   
        findPath :: Gra -> Node -> LEdge Edge' -> Crumb 
        findPath g v' (_, n', e) = foldr crumpdate (initCrumb e) $ ero g $ esp n' v' g 
        ero :: Gra -> Path -> [Edge'] 
        ero _ [] = []
        ero _ (x:[]) = []
        ero g (x:y:xyz) = lookupEdge g (x, y) : (ero g xyz) 
        lookupEdge :: Gra -> Edge -> Edge' 
        lookupEdge g e = snd $ head $ filter (\s -> fst s == snd e) $ lsuc' $ context g (fst e)  

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
    , arrow :: [Edge']
    } deriving (Generic, Show, Eq) 
instance ToJSON Crumb where 
    toJSON cr = object [
          "hops" .= hops cr 
        , "totalfee" .= cost cr
        , "maximum" .= neck cr ]

initCrumb :: Edge' -> Crumb 
initCrumb e = C (1) (fees e) (collat e) (e : [])

crumpdate :: Edge' -> Crumb -> Crumb 
crumpdate e cr = C 
    (hops cr + 1) 
    (Fee 
        ((base.cost) cr + (base.fees) e) 
        ((((ppm.cost) cr) + ((ppm.fees) e)) `div` 2)) -- not accurate overweights newest
    (min (neck cr) (collat e))
    (e : (arrow cr))


