{-# LANGUAGE
    DuplicateRecordFields, 
    LambdaCase, 
    DeriveGeneric, 
    OverloadedStrings
#-}

module Rebalance where 

import GHC.IORef
import Data.Graph.Inductive.Graph
import Data.Ratio
import Data.List 
import Data.Maybe

import Lightningd
import Cli
import Graph
import Paths
import Jspec


rebalance :: IO [PathInfo] 
rebalance = (listfunds) >>= \case 
    (Just (Correct (Res funds _))) -> getinfo >>= \case 
        (Just (Correct (Res info  _))) -> do 
            g <- readIORef graphRef
            case match (getNodeInt $ __id info) g of
                (Just (inny, n',z, outy), g') -> listfunds >>= \case
                    (Just (Correct (Res wallet _))) -> do 
                        (a,b,_,d,e) <- pure $ pots 
                                            $ filter (isJust.sci) 
                                            $ chlf wallet
                        (outy', inny') <- pure (matchChannels outy e, 
                                                matchChannels inny a)
                        bftFP g' inny' outy'
                    otherwise -> pure [] 
                otherwise -> pure [] 
    otherwise -> pure []  



matchChannels :: Adj Channel -> A -> Adj Channel 
matchChannels adj cx = filter ((flip elem cx).cci.fst) adj

pots :: [LFChannel] -> (A,A,A,A,A)
pots a = foldr p2 ([],[],[],[],[]) $ map ratiod a

type A = [String]
p2 :: (String, Ratio Msat) -> (A,A,A,A,A) -> (A,A,A,A,A) 
p2 (sid, b) (emp, midemp, goldi, midful, ful) 
    | b < 0.2   = (sid:emp, midemp, goldi, midful, ful)
    | b < 0.45   = (emp, sid:midemp, goldi, midful, ful)
    | b < 0.55   = (emp, midemp, sid:goldi, midful, ful)
    | b < 0.8   = (emp, midemp, goldi, sid:midful, ful)
    | otherwise = (emp, midemp, goldi, midful, sid:ful)

ratiod :: LFChannel -> (String, Ratio Msat)
ratiod lfc = (
      (fromJust.sci) lfc
    , our_amount_msat lfc % (amount_msat::LFChannel->Msat) lfc 
    ) 

-- DuplicateRecordFields requires even if arg type is known :(
sci :: LFChannel -> Maybe String 
sci = short_channel_id 
cci :: Channel -> String
cci = short_channel_id 
chlf :: ListFunds->[LFChannel]
chlf = channels 
