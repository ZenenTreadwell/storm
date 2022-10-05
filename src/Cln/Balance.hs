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
type Attempts = [ Maybe ListSendPays ]  

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
    c <- evalStateT (mapM attempt p) max  
    liftIO $ writeIORef circleRef c
        
attempt :: Circle -> StateT Msat IO Circle 
attempt c@(a, p) = do 
    max <- get
    hash <- liftIO $ payRoute max p
    msp <- liftIO $ checkHash hash
    case msp of 
        Nothing -> pure c
        sp -> do 
            put $ max - 20000 
            pure ((sp:a), p)     

getFee :: Maybe ListSendPays -> Int
getFee _ = 20000

payRoute :: Msat -> PathInfo -> IO String
payRoute max p =  
    let f = head.path $ p
        e = last.path $ p
        sf = camt f
        se = camt e
        size = min (div (min sf se) 3 ) (neck p `div` 3) 
        r = createRoute size p     
        edest = (___id::Route->String).last $ r
        isCircle = (==) edest ((source::Channel->String) f)
        famt = ramt.head $ r
        eamt = ramt.last $ r 
        fee = famt - eamt
        msg = (show fee) <> "msat fee (" <> edest <> ")"
    in if fee < max && isCircle then do 
        ui <- randomIO       
        ( b11invoice eamt (show (ui::Int)) msg ) >>= \case  
            (Just (Correct (Res invoice  _))) -> 
                ( sendpay r phsh ((payment_secret::Invoice->String) invoice) ) >>= \case 
                    (Just (Correct (Res povo  _))) -> pure phsh
                    otherwise -> pure phsh
                where phsh = (payment_hash::Invoice->String) invoice
            otherwise -> pure ""  
        else pure ""

checkHash :: String -> IO (Maybe ListSendPays) 
checkHash phsh = (waitsendpay phsh) >>= \case 
    otherwise -> (listsendpays phsh) >>= \case   
        (Just (Correct (Res w _))) -> pure $ Just w 
        otherwise -> pure Nothing 

type A = [String]
matchChannels :: Adj Channel -> A -> Adj Channel 
matchChannels adj cx = take 5 $ filter ((flip elem cx).cci.fst) adj
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
