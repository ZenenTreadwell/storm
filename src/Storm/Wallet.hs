
{-# LANGUAGE 
      LambdaCase
    , OverloadedStrings 
    , DuplicateRecordFields
#-}
module Storm.Wallet where 

import Cln.Client
import Cln.Types 
import Cln.Conduit
import Storm.Graph
import Storm.Balance 
-- import Storm.Types 
import Data.Text.Format.Numbers
import Data.Aeson
import Data.Aeson.Key
import Data.Maybe
import Data.Graph.Inductive.Graph
import Data.Ratio 

summarizeFunds :: ListFunds -> Value 
summarizeFunds j = object [
      "chain withdraw or channel(s)" .= (prettyI (Just ',') 
        $ (`div` 1000) $ sum  $ map  (.amount_msat) (outputs j))
    , "lightning pay | invoice" .= (
                  (prettyI (Just ',') ourTote) <> " | " <> (prettyI (Just ',') (tote - ourTote) )   
    ),"limbo" .=  (object $ map (\(s',i')-> ( (fromString s') .= (prettyI (Just ',') (div i' 1000)))) 
                                      $ filter (\x -> (fst x) /= ("CHANNELD_NORMAL"::String))
                                      $ foldr channelBreakdown [] c' )
    , "x balances" .= (countPots $ pots $ filter (isJust . sci) c') 
    ]                
    where tote = (`div`1000) . sum $ map (.amount_msat) normies
          ourTote = (`div` 1000) . sum $ map our_amount_msat normies
          normies = filter (\c -> __state c == "CHANNELD_NORMAL") $ c'
          c' = j.channels
          channelBreakdown :: LFChannel -> [(String, Int)] -> [(String, Int)] 
          channelBreakdown x a = case lookup (__state x) a of 
              Just cur -> (__state  x, cur + (our_amount_msat :: LFChannel -> Msat) x) : 
                          (filter ((/= (__state x)) . fst) a)  
              Nothing -> (__state  x, x.our_amount_msat) : a

countPots (a,b,c,d,e) = object [
      "a. depleted" .= length a
    , "b. mid-low" .= length b 
    , "c. balanced" .= length c 
    , "d. mid-high" .= length d
    , "e. full" .= length e  
    ]

type Pots = ([String], [String],[String],[String],[String])

pots a = foldr p2 ([],[],[],[],[]) $ map ratiod a
p2 :: (String, Ratio Msat) -> Pots -> Pots 
p2 (sid, b) (emp, midemp, goldi, midful, ful)
    | b < 0.2   = (sid:emp, midemp, goldi, midful, ful)
    | b < 0.45   = (emp, sid:midemp, goldi, midful, ful)
    | b < 0.55   = (emp, midemp, sid:goldi, midful, ful)
    | b < 0.8   = (emp, midemp, goldi, sid:midful, ful)
    | otherwise = (emp, midemp, goldi, midful, sid:ful)
ratiod :: LFChannel -> (String, Ratio Msat)
ratiod lfc = (
      (fromJust . sci) lfc
    , our_amount_msat lfc % lfc.amount_msat
    )

sci :: LFChannel -> Maybe String
sci = (.short_channel_id)
