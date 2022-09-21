{-# LANGUAGE
    DuplicateRecordFields, 
    LambdaCase, 
    DeriveGeneric, 
    OverloadedStrings
#-}

module Rebalance where 
import System.Random
import System.IO 
import GHC.IORef
import Data.Graph.Inductive.Graph
import Data.Ratio
import Data.List 
import Data.Maybe
import Data.Aeson
import Cln.Types
import Cln.Client
import Cln.Graph
import Cln.Conduit
import Cln.Paths

logg m = System.IO.appendFile "/home/o/Desktop/logy" $ m <> "\n"

rebalance :: IO ()  
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
                        p <- bftFP g' (matchChannels outy e) (matchChannels inny a)
                        tryRebalance 1000 p 
                    otherwise -> pure () 
                otherwise -> pure () 
    otherwise -> pure ()  

tryRebalance :: Msat -> [PathInfo] -> IO () 
tryRebalance _ [] = pure ()
tryRebalance z _ | z <= 0 = pure ()
tryRebalance z (p:px) =
    let  
        f = head.path $ p
        e = last.path $ p
        sf = camt f
        se = camt e
        amt = min (div (min sf se) 3 ) (neck p `div` 3)
        r = cRoute amt p
        samt = ramt.head $ r
        eamt = ramt.last $ r 
        fee = samt - eamt 
        msg = "move " <> (show amt) <> " with fee " <> (show fee) <> "msat"
    in do 
        logg msg
        ui <- randomIO       
        payto <- b11invoice eamt ((show (ui::Int))::String) msg 
        case payto of 
            (Just (Correct (Res invo  _))) -> do
                logg $ ((bolt11::Invoice->String) invo) 
                
                tryRebalance (z-fee) px
                payy <- sendpay r ((payment_hash::Invoice->String) invo) 
                                ((payment_secret::Invoice->String) invo)  
                case payy of 
                    (Just (Correct (Res povo  _))) -> do 
                        logg $ show.encode $ povo 
                        pure () 
                    (Just x) -> do 
                        logg $ show $ x
                        pure () 
                    otherwise -> pure () 
            otherwise -> pure () 

matchChannels :: Adj Channel -> A -> Adj Channel 
matchChannels adj cx = take 33 $ filter ((flip elem cx).cci.fst) adj

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

sci :: LFChannel -> Maybe String 
sci = short_channel_id 
cci :: Channel -> String
cci = short_channel_id
camt :: Channel -> Msat 
camt = amount_msat 
ramt :: Route -> Msat 
ramt = amount_msat 
chlf :: ListFunds->[LFChannel]
chlf = channels 
