{-# LANGUAGE 
      LambdaCase
    , OverloadedStrings 
    , DuplicateRecordFields
#-}

module Cln.Plugin where 

import Cln.Conduit
import Cln.Balance
import Cln.Manifest (manifest) 
import Cln.Types 
import Cln.Client 
import Cln.Graph 
import Cln.Paths
import Cln.Wallet
import System.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.Class 
import Control.Monad.Trans.State 
import Control.Concurrent (threadDelay)
import GHC.IORef 
import System.IO.Unsafe
import Network.Socket
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Aeson 
import Data.Aeson.Types ( parseMaybe )
import Data.Aeson.Lens
import Data.Aeson.Key
import Control.Lens hiding ((.=))
import Data.Text (Text, pack)  
import Data.Graph.Inductive.Graph 
import Data.Graph.Inductive.Query
import Control.Monad ((>>=))
import Data.Maybe
import Data.Conduit
import Data.Foldable 
import Data.List 
import Data.Char 
import Data.Text.Format.Numbers
import Numeric

plug :: ConduitT (Fin (Req Value)) S.ByteString IO () 
plug = a .| b .| c 

a :: (Monad n) => ConduitT (Fin (Req Value)) (Either (Res Value) (Maybe Id, Method, Params))  n () 
a = await >>= maybe mempty (\case  
    Correct v -> yield $ Right (getReqId v, getMethod v, getParams v) 
    InvalidReq -> yield $ Left $ Derp ("Request Error"::Text) Nothing  
    ParseErr -> yield $ Left $ Derp ("Parser Err"::Text) Nothing )

b :: ConduitT (Either (Res Value) (Maybe Id, Method, Params)) (Res Value) IO () 
b = evalStateT l Nothing
    where 
    l = lift await >>= monad 
    monad (Just( (Left r))) = lift $ yield r  
    monad (Just (Right (Nothing, m, p) )) = notifications m p
    monad (Just (Right (Just i, m, p))) = hooks i m p
    monad _ = pure ()

c :: ConduitT (Res Value) S.ByteString IO () 
c = await >>= maybe mempty (\v -> yield $ L.toStrict $ encode v) 

notifications m p = case m of 
    "channel_opened"         -> pure ()    
    "channel_state_changed"  -> pure ()    
    "connect"                -> pure ()     
    "disconnect"             -> pure ()    
    "invoice_payment"        -> case ((fromJSON p) :: Result InvoicePayment ) of
        Success (a)             -> do 
            liftIO $ System.IO.appendFile "/home/o/Desktop/loguy" $ show a <> "\n"
            pure () -- recordFwd 
        Error x                 -> pure ()                       
    "invoice_creation"       -> pure ()    
    "warning"                -> pure ()    
    "forward_event"          -> pure ()    
    "sendpay_success"        -> pure ()    
    "sendpay_failure"        -> pure ()    
    "coin_movement"          -> case ((fromJSON p) :: Result CoinMovement ) of 
        Success (a)             -> pure () -- recordFwd 
        Error x                 -> pure ()                       
    "balance_snapshot"       -> pure ()
    "openchannel_peer_sigs"  -> pure ()    
    "shutdown"               -> pure ()    
    otherwise                -> pure ()       

hooks i m p = 
  let rc = lift $ yield $ Res (object [ "result" .= ("continue"::Text) ]) i 
  in case m of
    "init" -> case fromJSON p :: Result Init of 
        (Success x) ->  do 
            liftIO $ connectCln
                   $ ((lightning5dir::InitConfig -> String).configuration $ x)  
                   <> "/" <> (rpc5file.configuration $ x)
            rc
        (Error q) -> rc
    "getmanifest" -> lift $ yield $ Res manifest i 
      
      -- HOOK
    "peer_connected"        -> rc 
    "commitment_revocation" -> rc
    "invoice_payment"       -> rc 
    "openchannel"           -> rc  
    "openchannel2"          -> rc 
    "openchannel2_changed"  -> rc 
    "openchannel2_sign"     -> rc 
    "rbf_channel"           -> rc 
    "htlc_accepted"         -> rc 
    "rpc_command"           -> rc  
    "onion_message_blinded" -> rc 
    "onion_message_ourpath" -> rc 

    -- BCLI   
    "getchaininfo" -> rc
    "estimatefees" -> rc
    "getrawblockbyheight" -> rc
    "getutxout" -> rc
    "sendrawtransaction" -> rc

    -- GRAPH 
    "stormload" -> do 
          liftIO loadGraph  
          -- liftIO genCircles 
          g <- liftIO $ readIORef graphRef
          -- p <- liftIO $ readIORef circleRef  
          lift $ yield $ Res (object [
                  "nodes loaded" .= order g
                -- , "balancing paths" .= length p
              ]) i
    "stormnetwork" -> do 
          g <- liftIO $ readIORef graphRef
          lift $ yield $ Res (object [
                "nodes" .= order g 
              , "edges" .= size g
              , "capacity" .= (prettyI (Just ',') $ capacity g 0 )
              , "x levels" .= (object
                  $ map (\(l,c)-> (fromString.show $ l) .= c )
                  $ foldr lvls [] (level (node'.fst.matchAny $ g) g)
              )]) i
              where  lvls :: (Node, Int) -> [(Int, Int)] -> [(Int, Int)]
                     lvls (m, i) q = case lookup i q of 
                        Nothing -> (i, 1) : q 
                        Just x -> (i, x + 1) : filter ((/= i).fst) q 
    "stormrebalance" -> do 
          liftIO $ rebalance 89000
          c <- liftIO $ readIORef circleRef
          lift $ yield $ Res (toJSON (map (toJSON.snd) c)) i
    "stormpaths" -> do 
          paths <- liftIO $ findPaths x y  
          lift $ yield $ Res (toJSON paths) i 
          where 
              x = getNodeInt $ getNodeArg 0 p
              y = getNodeInt $ getNodeArg 1 p 
              getNodeArg i v = case v ^? nth i . _String of
                  (Just b) -> filter isHexDigit (show b) 
                  Nothing -> ""
    -- UTIL 
    "stormwallet" -> do 
          w <- liftIO wallet
          lift $ yield $ Res w i 


