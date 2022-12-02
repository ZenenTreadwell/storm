{-# LANGUAGE 
    DuplicateRecordFields, 
    LambdaCase, 
    DeriveGeneric, 
    OverloadedStrings
#-}
module Storm.Graph where 
import Numeric 
import Cln.Types 
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Char 

type Gra = Gr NodeInfo Channel
type Cxt = Context NodeInfo Channel
type MCxt = MContext NodeInfo Channel
type Dcp = (MCxt, Gra) 

loadGraph :: ListNodes -> ListChannels -> Gra 
loadGraph n c = mkGraph (map toLNode nx) (map toLEdge' cx)
    where 
        cx = (channels::ListChannels->[Channel]) c
        nx = (_nodes :: ListNodes -> [NodeInfo]) n 
        toLNode ni = ( (getNodeInt.nodeid) ni , ni)
        toLEdge' c = (
            ( getNodeInt.source) c
          , (getNodeInt.(destination::Channel->String)) c
          , c
          )

getNodeInt :: String -> Node
getNodeInt s = case readHex.filter isHexDigit $ s of 
    ([]) -> 0 
    (x:_)-> fst x
