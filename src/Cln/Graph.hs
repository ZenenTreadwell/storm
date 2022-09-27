
{-# LANGUAGE 
    DuplicateRecordFields, 
    LambdaCase, 
    DeriveGeneric, 
    OverloadedStrings
#-}
module Cln.Graph where 
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

graphRef :: IORef Gra
graphRef = unsafePerformIO $ newIORef empty 


loadGraph :: IO ()
loadGraph = (allchannels) >>= \case 
    (Just (Correct (Res listchannels _))) -> (allnodes) >>= \case 
        (Just (Correct (Res listnodes _))) -> do 
            liftIO $ writeIORef graphRef
                   $ (\g -> (flip subgraph g) . head $ components g )
                   $ (\g -> foldr (delEdge.toEdge) g $ filter (feeTooHigh.edgeLabel) (labEdges g) ) 
                   $ gfiltermap traps
                   $ mkGraph (map toLNode nx) (map toLEdge' cx)
                   where cx = (channels::ListChannels->[Channel]) listchannels 
                         nx = (_nodes :: ListNodes -> [NodeInfo]) listnodes
        otherwise -> pure () 
    otherwise -> pure () 

getNodeInt :: String -> Node
getNodeInt s = case readHex.filter isHexDigit $ s of 
    ([]) -> 0 
    (x:_)-> fst x 

toLNode :: NodeInfo -> LNode NodeInfo 
toLNode ni = ( (getNodeInt.nodeid) ni , ni)

toLEdge' :: Channel -> LEdge Channel
toLEdge' c = (
      (getNodeInt.source) c
    , (getNodeInt.(destination::Channel->String)) c
    , c )

feeTooHigh :: Channel -> Bool 
feeTooHigh c = (||) 
    (base_fee_millisatoshi c > 111111) 
    (fee_per_millionth c > 11111)

traps :: Cxt -> Maybe Cxt  
traps c =
    let inny = (indeg' c) == 0 
        outy = (outdeg' c) == 0
    in if (inny || outy)   
        then Nothing 
        else Just c

