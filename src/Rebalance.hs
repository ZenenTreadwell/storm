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
                        $ trybalance inny outy   
                        $ pots
                        $ filter (isJust.sci)
                        $ (channels::ListFunds->[LFChannel]) wallet
                    otherwise -> pure () 
                otherwise -> pure () 
    otherwise -> pure ()  





trybalance inny outy (a,b,c,d,e) = 
    let
    b1 = fromTo e a
    b2 = fromTo d a  
    b3 = fromTo e b
    b4 = fromTo d b 
    in undefined

fromTo a b = undefined 
 --   inny' = matchChannels inny (a) 
 --   outy' = matchChannels outy (e)  

matchChannels :: Adj Channel -> A -> Adj Channel 
matchChannels adj cx = filter ((flip elem cx).cci.fst) adj

pots :: [LFChannel] -> (A,A,A,A,A)
pots a = foldr p2 ([],[],[],[],[]) $ map ratiod a

type A = [String] -- shortids 
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

sci :: LFChannel -> Maybe String 
sci = short_channel_id 
cci :: Channel -> String
cci = short_channel_id 
