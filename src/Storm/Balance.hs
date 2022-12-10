{-# LANGUAGE
    DuplicateRecordFields, 
    LambdaCase, 
    DeriveGeneric, 
    OverloadedStrings, 
    BangPatterns
#-}

module Storm.Balance where 
import System.Random
import System.IO 

import Control.Concurrent
import Control.Concurrent.Async 
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Graph.Inductive.Graph
import Data.Ratio
import Data.List 
import Data.Maybe
import Data.Aeson

import qualified Data.Sequence as Q
import Data.Sequence( Seq(..) , (<|) , (|>) , (><) ) 
import qualified Data.Text as T

import Cln.Types
import Cln.Client
import Cln.Conduit

import Storm.Graph
import Storm.Paths
import Storm.Search 
import Storm.Types 

type Circle = (Attempts, PathInfo) 
type Attempts = [ ListSendPays ]  

loadCircles :: Gra -> Node -> [Acc] -> IO [Ref]
loadCircles !g me a =
    let outs :: [Node]
        outs = map snd $ filter (\(r, _) -> r > 0.7) a
        spoon :: Int -> Chan Ref -> Node -> IO () 
        spoon q c oot = runReaderT (evalStateT loop (q, Empty, c)) (g, oot, me)
    in do
        c <- newChan 
        threads <- mapConcurrently (\o ->
            let   oo = suc g me 
                  le = dropWhile (not.(== o)) oo
                  la = length le
                  lo = length oo
                  q  = lo - la 
            in forkIO (spoon q c o) 
            ) outs
        threadDelay $ (10 ^ 6) * 3
        mapM killThread threads
        xd <- getChanContents c -- hung (but threads work & get killed) 
                                -- chan must be empty but why
        pure xd 


loop :: StateT (Int, Ref, Chan Ref) Search () 
loop = do 
    me <- liftIO $ myThreadId
    (q, r , c) <- get
    (_, r') <- lift $ search r
    liftIO $ writeChan c (q <| r') 
    liftIO $ System.IO.appendFile ("/home/o/.ao/" <> (show me)) $ "to chan " <> show (q <| r') 
    put (q, increment.chop $ r', c) 
    loop 

















-- 
--genCircles :: IO () 
--genCircles = getinfo >>= \case
--    (Just (Correct (Res info  _))) -> do 
--         g <- liftIO $ readIORef graphRef
--         case match (getNodeInt $ __id info) g of 
--            (Just (inny, n',z, outy), g') -> listfunds >>= \case 
--                (Just (Correct (Res wallet _))) -> do
--                    (a,b,_,d,e) <- pure $ pots
--                                        $ filter (isJust.sci)
--                                        $ chlf wallet
--                    p <- _ 
--                        (foldr (append (LP [])) Q.empty (matchChannels outy e) , [])
--                    liftIO $ writeIORef circleRef $ map ((,) []) (sort $ buildPaths p) 
--
--rebalance :: Msat -> IO ()   
--rebalance max = do 
--    p <- liftIO $ readIORef circleRef
--    c <- evalStateT (mapM payPath p) max   
--    liftIO $ writeIORef circleRef c
--
--payPath :: Circle -> StateT Msat IO Circle
--payPath c@(a,p) = do 
--    max <- get
--    if fee < max && isCircle then do 
--        ui <- liftIO randomIO       
--        ir <- liftIO $ b11invoice eamt (show (ui::Int)) msg
--        case ir of    
--            (Just (Correct (Res invoice  _))) -> 
--                let
--                    ph = (payment_hash::Invoice->String) invoice
--                    ps = (payment_secret::Invoice->String) invoice
--                in do
--                    liftIO $ sendpay r ph ps
--                    ch <- liftIO $ checkHash ph
--                    case ch of 
--                        (Just w) -> do
--                            if (checkSettled $ payments w)
--                                then put (max-fee)
--                                else pure ()
--                            pure (w:a ,p)
--                        otherwise -> pure c
--            otherwise -> pure c  
--    else pure c
--    where
--        r = undefined    
--        f = undefined
--        e = undefined 
--        getSize = amount_msat :: Channel -> Msat
--        sf = getSize f
--        se = getSize e
--        size = min (div (min sf se) 3 ) (neck p `div` 3) 
--        edest = (___id::Route->String).last $ r
--        eamt = ramt.last $ r
--        isCircle = (==) edest ((source::Channel->String) f)
--        fee = routeFee r
--        msg = (show fee) <> "msat fee (" <> edest <> ")"
--
--
--checkSettled :: [LPPayment] -> Bool 
--checkSettled [] = False 
--checkSettled (x:_) = (==) "settled" ((status::LPPayment->String) x) 
--
--routeFee :: [Route] -> Msat
--routeFee r = famt - eamt
--    where 
--        famt = ramt.head $ r
--        eamt = ramt.last $ r 
--
--checkHash :: String -> IO (Maybe ListSendPays) 
--checkHash h = (waitsendpay h) >>= \case 
--    otherwise -> (listsendpays h) >>= \case   
--        (Just (Correct (Res w _))) -> pure $ Just w 
--        otherwise -> pure Nothing 
--
--type A = [String]
--matchChannels :: Adj Channel -> A -> Adj Channel 
--matchChannels adj cx = filter ((flip elem cx).cci.fst) adj
--pots :: [LFChannel] -> (A,A,A,A,A)
--pots a = foldr p2 ([],[],[],[],[]) $ map ratiod a
--
--p2 :: (String, Ratio Msat) -> (A,A,A,A,A) -> (A,A,A,A,A) 
--p2 (sid, b) (emp, midemp, goldi, midful, ful)
--    | b < 0.2   = (sid:emp, midemp, goldi, midful, ful)
--    | b < 0.45   = (emp, sid:midemp, goldi, midful, ful)
--    | b < 0.55   = (emp, midemp, sid:goldi, midful, ful)
--    | b < 0.8   = (emp, midemp, goldi, sid:midful, ful)
--    | otherwise = (emp, midemp, goldi, midful, sid:ful)
--
--ratiod :: LFChannel -> (String, Ratio Msat)
--ratiod lfc = (
--      (fromJust.sci) lfc
--    , our_amount_msat lfc % (amount_msat::LFChannel->Msat) lfc
--    )
--
--sci :: LFChannel -> Maybe String
--sci = short_channel_id
--cci :: Channel -> String
--cci = short_channel_id
--ramt :: Route -> Msat
--ramt = amount_msat
--chlf :: ListFunds->[LFChannel]
--chlf = channels
