{-# LANGUAGE 
    DuplicateRecordFields, 
    LambdaCase, 
    DeriveGeneric, 
    OverloadedStrings
#-}

module Rebalance where 

import Data.Graph.Inductive.Graph
import GHC.IORef

import Lightningd
import Cli
import Graph
import Jspec

rebalance :: IO () 
rebalance = (listFunds) >>= \case 
    (Just (Correct (Res funds _))) -> getinfo >>= \case 
        (Just (Correct (Res info  _))) -> do 
            g <- readIORef graphRef
            case match (getNodeInt $ __id info) g of
                (Just (inny, n',z, outy), g') -> pure () 
                    
                    -- sort req in vs req out
                    -- find lowest fee balance options
                    -- attempt rebalances
                    -- wait
    otherwise -> pure ()  
        
    
