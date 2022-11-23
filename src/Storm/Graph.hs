
{-# LANGUAGE 
    DuplicateRecordFields, 
    LambdaCase, 
    DeriveGeneric, 
    OverloadedStrings
#-}
module Storm.Graph where 
import Cln.Types 
import Cln.Client
import Cln.Conduit
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


type Gra = Gr NodeInfo Channel
type Cxt = Context NodeInfo Channel
type MCxt = MContext NodeInfo Channel
type Dcp = (MCxt, Gra) 

graphRef :: IORef Gra
graphRef = unsafePerformIO $ newIORef empty 

loadGraph :: IO ()
loadGraph = (allchannels) >>= \case 
    (Just (Correct (Res listchannels _))) -> (allnodes) >>= \case 
        (Just (Correct (Res listnodes _))) -> do 
            liftIO $ writeIORef graphRef
                   $ mkGraph (map toLNode nx) (map toLEdge' cx)
                   where cx = (channels::ListChannels->[Channel]) listchannels 
                         nx = (_nodes :: ListNodes -> [NodeInfo]) listnodes
                         toLNode ni = ( (getNodeInt.nodeid) ni , ni)
                         toLEdge' c = (
                           ( getNodeInt.source) c
                           , (getNodeInt.(destination::Channel->String)) c
                           , c
                           )
        otherwise -> pure () 
    otherwise -> pure () 

getNodeInt :: String -> Node
getNodeInt s = case readHex.filter isHexDigit $ s of 
    ([]) -> 0 
    (x:_)-> fst x 
