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

import Data.Graph.Inductive.PatriciaTree
--import Data.Graph.Inductive.Tree
import Data.Char 

type Gra = Gr NodeInfo Channel
type Cxt = Context NodeInfo Channel
type MCxt = MContext NodeInfo Channel
type Dcp = (MCxt, Gra) 

-- Commenting this out for now; I'm not sure how to debug it and
-- I think I'll learn when I need to

-- loadGraph :: ListNodes -> ListChannels -> Gra 
-- loadGraph n c = mkGraph (map toLNode nx) (map toLEdge' cx)
--     where 
--         cx = c.channels
--         nx = n._nodes
--         toLNode ni = ((getNodeInt ni.nodeid), ni)
--         toLEdge' c = (
--             getNodeInt c.source
--           , getNodeInt c.destination
--           , c
--           )

getNodeInt :: String -> Node
getNodeInt s = case readHex . filter isHexDigit $ s of 
    ([]) -> 0 
    (x:_)-> fst x
