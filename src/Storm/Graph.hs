
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
import Cln.Plugin 

type Gra = Gr NodeInfo Channel
type Cxt = Context NodeInfo Channel
type MCxt = MContext NodeInfo Channel
type Dcp = (MCxt, Gra) 

-- logy m = liftIO $ System.IO.appendFile "/home/o/.ao/storm" $ (show m) <> "\n"

loadGraph h = undefined 
    


 --                  $ mkGraph (map toLNode nx) (map toLEdge' cx)
 --                  where cx = (channels::ListChannels->[Channel]) listchannels 
--                         nx = (_nodes :: ListNodes -> [NodeInfo]) listnodes
--                         toLNode ni = ( (getNodeInt.nodeid) ni , ni)

getNodeInt :: String -> Node
getNodeInt s = case readHex.filter isHexDigit $ s of 
    ([]) -> 0 
    (x:_)-> fst x 
