{-# LANGUAGE 
    DuplicateRecordFields, 
    LambdaCase, 
    DeriveGeneric
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

graphRef :: IORef (Gr Node' Edge')
graphRef = unsafePerformIO $ newIORef empty 
{-# NOINLINE graphRef #-}

data Node' = N {
      alias' :: Maybe String
    , crumb :: Maybe Crumb }

data Edge' = E { 
      short :: String 
    , collat :: Sat
    , fees :: Fee   
    , mysat :: Maybe Sat 
    , htlcs :: [Int] }
    -- home :: Node 

toEdge' :: Channel -> LEdge Edge' 
toEdge' c = (
    (fst.head $ readHex $ source c),
    (fst.head $ readHex $ destination c),
    (E  
        (short_channel_id c) 
        (satoshis c) 
        (Fee (base_fee_millisatoshi c) (fee_per_millionth c)) 
        Nothing 
        ([]) ))
    
toNode' :: NodeInfo -> LNode Node'
toNode' n = ((fst.head $ readHex $ nodeid n), N (ali n) Nothing)
    where ali = alias :: NodeInfo -> Maybe String

loadGraph h = (allchannels h) >>= \case 
    (Just (Correct (Res a' _))) -> do 
        gra <- pure $ mkGraph (nodesFromChannels a'') (map toEdge' a'')
        liftIO $ writeIORef graphRef $ gra
        pure gra
        where a'' :: [Channel]
              a'' = channels a'
              nodesFromChannels :: [Channel] -> [LNode Node']
              nodesFromChannels cx = foldr allnodr [] cx
              allnodr :: Channel -> [LNode Node'] -> [LNode Node']
              allnodr c nx = (fst.head $ readHex $ destination c, N Nothing Nothing) 
                             : (fst.head $ readHex $ source c, N Nothing Nothing) 
                             : nx
    otherwise -> pure empty  


-- Use label on node to never revisit (same solution as Nodes) 
-- whole point of fgl was inductive def not node marking 



-- dfs? wth is taking so long? -- nothing prevents researching e
findCircles :: Node -> IO [Crumb]
findCircles n = do
    g <- readIORef graphRef
    pure $ gogo g [homeCrumb n] [] 
    where
        gogo :: Gr Node' Edge' -> [ Crumb ] -> [ Crumb ] -> [ Crumb ]
        gogo g [] fin = fin 
        gogo g (x:xs) fin 
            | (length.path) x > 5 = gogo g xs fin
            | (last.path) x == (head.path) x && (length.path) x > 1 = gogo g xs (x:fin) 
            | otherwise = gogo g ( (++) xtsd xs) fin 
            where 
                (incoming, n', _, outgoing) = context g ((head.path) x) 
                xtsd :: [Crumb] 
                xtsd = catMaybes $ map (crumpdate x) (nubBy unuq outgoing)
                unuq (_, n1) (_, n2) = n1 == n2 

-- use shortest path tree?
--findCircles n = (readIORef graphRef) >>= \g ->  
--    let tree = spTree n (emap fff g)
--    in pure $ ufold ff [] g  
--    where -- .. ff :: Context Node' Edge' -> [LPath]
--        
--        ff = undefined
--        fff :: Edge' -> Integer 
--        fff e = toInteger (collat e)     
    
-- use labels on graph?
--findCircles n = do 
--    g <- readIORef graphRef -- xxx
--    pure $ gogo g (C 0 mempty maxBound []) (context g n)    
--    where
--        gogo g cr c@(_, i, _, outgoing)
--                | i == n = crumbs cr  
--                | hops cr > 20 = []    
--                | otherwise =  pure $ join $ map (ff g) outgoing
--                where 
--                    ff g le@(e, n2) = gogo g (crumpdate cr i le) (context g n2)
--
data Crumb = C {
      hops :: Int 
    , totalfees :: Fee 
    , neck :: Sat 
    , path :: Path  
    } deriving (Show, Generic) 
instance ToJSON Crumb

homeCrumb n = C 0 (Fee 0 0) maxBound [n] 

crumpdate :: Crumb -> (Edge', Node) -> Maybe Crumb 
crumpdate cr (e, n2) = 
    case elemIndex n2 p of
        Just ind -> if ( ind == (length p) - 1 )   
            then ud 
            else Nothing -- no loop for you 
        Nothing -> ud
     where 
        p = path cr
        ud = Just $ C 
            (hops cr + 1) 
            (Fee ((base.totalfees) cr + (base.fees) e) 0) -- ppm xxx 
            (min (neck cr) (collat e))
            (n2 : p)


t = "/home/taylor/Projects/storm/t.log"
log' msg = System.IO.appendFile t msg 



































