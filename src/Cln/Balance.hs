{-# LANGUAGE
    DuplicateRecordFields, 
    LambdaCase, 
    DeriveGeneric, 
    OverloadedStrings
#-}

module Cln.Balance where 
import System.Random
import System.IO 
import System.IO.Unsafe
import Control.Monad.IO.Class
import GHC.IORef
import Control.Concurrent
import Data.Graph.Inductive.Graph
import Data.Ratio
import Data.List 
import Data.Maybe
import Data.Aeson
import qualified Data.Text as T
import Cln.Types
import Cln.Client
import Cln.Graph
import Cln.Conduit
import Cln.Paths

type Circle = (Activity, PathInfo) 
type Activity = [ WaitSendPay ]  

circleRef :: IORef [Circle] 
circleRef = unsafePerformIO $ newIORef [] 
 
genCircles :: IO () 
genCircles = getinfo >>= \case
    (Just (Correct (Res info  _))) -> do 
         g <- liftIO $ readIORef graphRef
         case match (getNodeInt $ __id info) g of 
            (Just (inny, n',z, outy), g') -> listfunds >>= \case 
                (Just (Correct (Res wallet _))) -> do
                    (a,b,_,d,e) <- pure $ pots
                                        $ filter (isJust.sci)
                                        $ chlf wallet
                    p <- bftFP g' (matchChannels outy e) (matchChannels inny a)
                    liftIO $ writeIORef circleRef $ map ((,) []) p

rebalance :: Msat -> IO [Maybe ListSendPays]  
rebalance max = do 
    p <- liftIO $ readIORef circleRef
    trySends <- mapM payRoute $ getSome max [] $ map snd $ p 
    tryChecks <- mapM checkHash trySends
    pure tryChecks

getSome :: Msat -> [[Route]] -> [PathInfo] -> [[Route]] 
getSome _ _ [] = [] 
getSome max q (p:px) = 
    let f = head.path $ p
        e = last.path $ p
        sf = camt f
        se = camt e
        amt = min (div (min sf se) 3 ) (neck p `div` 3)
        r = createRoute amt p
        famt = ramt.head $ r
        eamt = ramt.last $ r 
        fee = famt - eamt
    in if (max > fee) then getSome (max - fee) (r : q) px
                      else q  

payRoute :: [Route] -> IO String
payRoute r =  
    let edest = (___id::Route->String).last $ r
        famt = ramt.head $ r
        eamt = ramt.last $ r 
        fee = famt - eamt
        msg = "move " <> (show famt) <> " with " <> (show fee) <> "msat fee (" <> edest <> ")"
    in do 
        ui <- randomIO       
        ( b11invoice eamt (show (ui::Int)) msg ) >>= \case  
            (Just (Correct (Res invoice  _))) -> 
                ( sendpay r phsh ((payment_secret::Invoice->String) invoice) ) >>= \case 
                    (Just (Correct (Res povo  _))) -> pure phsh
                    otherwise -> pure phsh
                where phsh = (payment_hash::Invoice->String) invoice
            otherwise -> pure ""  

checkHash :: String -> IO (Maybe ListSendPays ) 
checkHash phsh = (waitsendpay phsh) >>= \case 
    otherwise -> (listsendpays phsh) >>= \case   
        (Just (Correct (Res w _))) -> pure $ Just w 
        otherwise -> pure Nothing 

matchChannels :: Adj Channel -> A -> Adj Channel 
matchChannels adj cx =  filter ((flip elem cx).cci.fst) adj

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
