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
import Control.Monad.State
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

type Circle = (Attempts, PathInfo) 
type Attempts = [ ListSendPays ]  

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

rebalance :: Msat -> IO ()   
rebalance max = do 
    p <- liftIO $ readIORef circleRef
    c <- evalStateT (mapM payPath p) max   
    liftIO $ writeIORef circleRef c

getFee :: Maybe ListSendPays -> Int
getFee _ = 20000

payPath :: Circle -> StateT Msat IO Circle
payPath c@(a,p) = do 
    max <- get
    if fee < max && isCircle then do 
        ui <- liftIO randomIO       
        ir <- liftIO $ b11invoice eamt (show (ui::Int)) msg
        case ir of    
            (Just (Correct (Res invoice  _))) -> 
                let
                    phsh = (payment_hash::Invoice->String) invoice
                    psct = (payment_secret::Invoice->String) invoice
                in do
                    liftIO $ sendpay r phsh $ psct
                    ch <- liftIO $ checkHash $ phsh
                    case ch of 
                        (Just w) -> do
                            liftIO $ System.IO.appendFile "/home/o/Desktop/loguy" $ show (max,w) <> "\n" 
                            if (checkSettled $ payments w) 
                                then put (max-fee)
                                else pure () 
                            pure (w:a ,p) 
                        otherwise -> pure c
            otherwise -> pure c  
    else pure c
    
    where
        f = head.path $ p
        e = last.path $ p
        sf = camt f
        se = camt e
        size = min (div (min sf se) 3 ) (neck p `div` 3) 
        r = createRoute size p     
        edest = (___id::Route->String).last $ r
        eamt = ramt.last $ r
        isCircle = (==) edest ((source::Channel->String) f)
        fee = routeFee r
        msg = (show fee) <> "msat fee (" <> edest <> ")"


checkSettled :: [LPPayment] -> Bool 
checkSettled [] = False 
checkSettled (x:_) = (==) "settled" ((status::LPPayment->String) x) 

routeFee :: [Route] -> Msat
routeFee r = famt - eamt
    where 
        famt = ramt.head $ r
        eamt = ramt.last $ r 

checkHash :: String -> IO (Maybe ListSendPays) 
checkHash h = (waitsendpay h) >>= \case 
    otherwise -> (listsendpays h) >>= \case   
        (Just (Correct (Res w _))) -> pure $ Just w 
        otherwise -> pure Nothing 

type A = [String]
matchChannels :: Adj Channel -> A -> Adj Channel 
matchChannels adj cx = filter ((flip elem cx).cci.fst) adj
pots :: [LFChannel] -> (A,A,A,A,A)
pots a = foldr p2 ([],[],[],[],[]) $ map ratiod a

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
