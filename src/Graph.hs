
{-# LANGUAGE 
    DuplicateRecordFields, 
    LambdaCase, 
    DeriveGeneric, 
    OverloadedStrings
#-}
module Graph where 

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
import Numeric 

graphRef :: IORef Gra
graphRef = unsafePerformIO $ newIORef empty 

type Gra = Gr NodeInfo Channel
type Cxt = Context NodeInfo Channel

getNodeInt :: String -> Node
getNodeInt s = case readHex.filter isHexDigit $ s of 
    ([]) -> 0 
    (x:_)-> fst x 

toLNode :: NodeInfo -> LNode NodeInfo 
toLNode ni = ( (getNodeInt.nodeid) ni , ni)

toLEdge' :: Channel -> LEdge Channel
toLEdge' c = ( (getNodeInt.source) c  , (getNodeInt.destination) c, c )

loadGraph :: IO ()
loadGraph = (allchannels) >>= \case 
    (Just (Correct (Res listchannels _))) -> (allnodes) >>= \case 
        (Just (Correct (Res listnodes _))) -> do 
            liftIO $ writeIORef graphRef $ mkGraph (map toLNode nx) (map toLEdge' cx) 
            where cx = (channels::ListChannels->[Channel]) listchannels 
                  nx = (_nodes :: ListNodes -> [NodeInfo]) listnodes
        otherwise -> pure () 
    otherwise -> pure () 

--mockInfo :: String -> NodeInfo
--mockInfo s = NodeInfo s Nothing Nothing Nothing Nothing Nothing Nothing 
--
--mockInto :: String -> LNode NodeInfo  
--mockInto = toLNode.mockInfo
--
--loadBfs :: Int -> String -> IO () 
--loadBfs d ns 
--    | d == 0 = pure () 
--    | otherwise = do 
--       gra <- liftIO $ readIORef graphRef 
--       (channelsbysource ns) >>= \case 
--           (Just (Correct (Res listchannels _))) -> do 
--                liftIO $ writeIORef graphRef $ foldr buildg gra lc
--                x <- mapM (loadBfs (d-1)) $ map destination lc 
--                pure () 
--                where
--                    lc = (channels :: ListChannels -> [Channel] ) listchannels
--           otherwise -> pure ()  
--
--buildg :: Channel -> Gra -> Gra
--buildg c g = 
--    let a = source c
--        b = destination c 
--        e = toLEdge' c
--    in case (gelem (getNodeInt a) g, gelem (getNodeInt b) g) of 
--        (True, True) -> insEdge e g
--        (False, True) -> insEdge e (insNode (mockInto a) g)
--        (True, False) -> insEdge e (insNode (mockInto b) g) 
--        (False, False) -> insEdge e (insNode (mockInto a) (insNode (mockInto b) g)) 

calcCapacity :: Gra -> Sat -> Sat
calcCapacity g a 
    | isEmpty g = a 
    | otherwise = case matchAny g of 
        (c, g') -> calcCapacity g' $ a + (sum 
            $ map ((amount_msat::Channel->Msat).fst) 
            $ nubBy (\x y -> (==) 
                (((short_channel_id::Channel->String).fst) x)     
                (((short_channel_id::Channel->String).fst) y)  ) 
            $ lneighbors' c) 

deadends :: Cxt -> Maybe Cxt  
deadends c =
    let inny = (indeg' c) == 0 
        outy = (outdeg' c) == 0
    in if (inny || outy)   
        then Nothing 
        else Just c

