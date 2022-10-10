
{-# LANGUAGE 
      LambdaCase
    , OverloadedStrings 
    , DuplicateRecordFields
#-}
module Cln.Wallet where 

import Cln.Client
import Cln.Types 
import Cln.Conduit
import Cln.Graph
import Cln.Balance 
import Data.Text.Format.Numbers
import Data.Aeson
import Data.Aeson.Key
import Data.Maybe
import Data.Graph.Inductive.Graph
import Control.Monad.IO.Class

wallet :: IO Value
wallet = do     
    lf <- liftIO $ listfunds  
    case lf of 
        (Just (Correct (Res j _))) -> pure $ summarizeFunds j   
        (Just x) -> pure $ object ["decode err" .= (show x)] 
        otherwise -> pure $ object [] 

summarizeFunds :: ListFunds -> Value 
summarizeFunds j = object [
      "chain withdraw or channel(s)" .= (prettyI (Just ',') 
        $ (`div` 1000) $ sum  $ map  (amount_msat :: LFOutput -> Msat) (outputs j))
    , "lightning pay | invoice" .= (
                  (prettyI (Just ',') ourTote) <> " | " <> (prettyI (Just ',') (tote - ourTote) )   
    ),"limbo" .=  (object $ map (\(s',i')-> ( (fromString s') .= (prettyI (Just ',') (div i' 1000)))) 
                                      $ filter (\x -> (fst x) /= ("CHANNELD_NORMAL"::String))
                                      $ foldr channelBreakdown [] c' )
    , "x balances" .= (countPots $ pots $ filter (isJust.sci) c') 
    ]                
    where tote = (`div`1000).sum $ map (amount_msat::LFChannel->Msat) normies
          ourTote = (`div`1000).sum $ map our_amount_msat normies
          normies = filter (\c -> __state c == "CHANNELD_NORMAL") $ c'
          c' = (channels :: ListFunds -> [LFChannel]) j
          channelBreakdown :: LFChannel -> [(String, Int)] -> [(String, Int)] 
          channelBreakdown x a = case lookup (__state x) a of 
              Just cur -> (__state  x, cur + (our_amount_msat :: LFChannel -> Msat) x) : 
                          (filter ((/= (__state x)).fst) a)  
              Nothing -> (__state  x, (our_amount_msat :: LFChannel -> Msat) x) : a

countPots (a,b,c,d,e) = object [
      "a. depleted" .= length a
    , "b. mid-low" .= length b 
    , "c. balanced" .= length c 
    , "d. mid-high" .= length d
    , "e. full" .= length e  
    ]

capacity :: Gra -> Msat -> Msat 
capacity g t
    | isEmpty g = t
    | otherwise = case matchAny g of
        (n, g') -> capacity g' $ t + ( sum 
            . (map (amount_msat::Channel -> Msat)) 
            . (map snd)  
            . lsuc' -- (outchannels) 
            $ n 
            )     

