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

import Data.Ratio
import Data.List 

rebalance :: IO () 
rebalance = (listfunds) >>= \case 
    (Just (Correct (Res funds _))) -> getinfo >>= \case 
        (Just (Correct (Res info  _))) -> do 
            g <- readIORef graphRef
            case match (getNodeInt $ __id info) g of
                (Just (inny, n',z, outy), g') -> listfunds >>= \case 
                    (Just (Correct (Res wallet _))) -> do 
                        -- bft from high to low 
                        pure
                        $ pots 
                        $ (channels::ListFunds->[LFChannel]) wallet
                    otherwise -> pure () 
                otherwise -> pure () 
    otherwise -> pure ()  
        
--pots :: [LFChannel] -> (a,b,c,d) -> () 
pots = undefined
    
