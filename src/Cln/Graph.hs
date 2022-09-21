
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
toLEdge' c = (
      (getNodeInt.source) c
    , (getNodeInt.(destination::Channel->String)) c
    , c )

loadGraph :: IO ()
loadGraph = (allchannels) >>= \case 
    (Just (Correct (Res listchannels _))) -> (allnodes) >>= \case 
        (Just (Correct (Res listnodes _))) -> do 
            liftIO $ writeIORef graphRef
    
                   $ gfiltermap traps
                   $ mkGraph (map toLNode nx) (map toLEdge' cx)
                   where cx = (channels::ListChannels->[Channel]) listchannels 
                         nx = (_nodes :: ListNodes -> [NodeInfo]) listnodes
        otherwise -> pure () 
    otherwise -> pure () 

capacity :: Gra -> Sat -> Sat
capacity g a 
    | isEmpty g = a 
    | otherwise = case matchAny g of 
        (c, g') -> capacity g' $ a + (sum 
            $ map ((amount_msat::Channel->Msat).fst) 
            $ nubBy (\x y -> (==) 
                (((short_channel_id::Channel->String).fst) x)     
                (((short_channel_id::Channel->String).fst) y)  ) 
            $ lneighbors' c) 

traps :: Cxt -> Maybe Cxt  
traps c =
    let inny = (indeg' c) == 0 
        outy = (outdeg' c) == 0
    in if (inny || outy)   
        then Nothing 
        else Just c

destiny :: Channel -> String 
destiny = destination
