{-# LANGUAGE 
    DuplicateRecordFields, 
    LambdaCase
#-}

module Graph where 

import Numeric 
import Lightningd 
import Cli
import Jspec

import System.IO 
import System.IO.Unsafe
import GHC.IORef

import Control.Monad.IO.Class

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query
import Data.Graph.Inductive.Tree

graphRef :: IORef (Gr Node' Edge')
graphRef = unsafePerformIO $ newIORef empty 
{-# NOINLINE graphRef #-}

data Node' = N { alias :: Maybe String }
data Edge' = E { 
                   short :: String 
                 , capacity :: Sat
                 , fees :: Fee   
               }

toEdge' :: Channel -> LEdge Edge' 
toEdge' c = (
    (fst.head $ readHex $ source c),
    (fst.head $ readHex $ destination c),
    (E (short_channel_id c) (satoshis c) 
        $ Fee (base_fee_millisatoshi c) (fee_per_millionth c)))

toNode' :: NodeInfo -> LNode Node'
toNode' n = ((fst.head $ readHex $ nodeid n), N ((alias :: NodeInfo -> Maybe String) n))

-- loadGraph :: Handle -> IO 
-- loadGraph h = do 
    -- x <- allnodes h InvalidReq??
    -- a <- allchannels h 
    --case (a, x) of 
    --    ( (Just (Correct (Res a' _))) , (Just (Correct (Res x' _))) ) -> do
    --        gra <- pure $ mkGraph (map toNode' (eNodes x')) (map toEdge' (channels a')) 
    --        liftIO $ writeIORef graphRef $ gra
    --        pure gra
    --    (aa, xx) -> do 
    --        liftIO $ log' $ show xx -- how the heck is listnodes invalid
    --        pure empty

loadGraph h = (allchannels h) >>= \case 
    (Just (Correct (Res a' _))) -> do 
        gra <- pure $ mkGraph (nodesFromChannels a'') (map toEdge' a'')
        liftIO $ writeIORef graphRef $ gra
        pure gra 
        where a'' = channels a'
    otherwise -> pure empty  


nodesFromChannels :: [Channel] -> [LNode Node']
nodesFromChannels cx = foldr allnodr [] cx

allnodr :: Channel -> [LNode Node'] -> [LNode Node']
allnodr c nx = (fst.head $ readHex $ destination c, N Nothing) : (fst.head $ readHex $ source c, N Nothing) : nx



t = "/home/taylor/Projects/storm/t.log"
log' msg = System.IO.appendFile t msg 
