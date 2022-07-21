{-# LANGUAGE 
    LambdaCase,
    OverloadedStrings
#-}
module Plugin where 

import Jspec
import Manifest (manifest) 
import Lightningd 
import Cli
import Nodes
import System.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.Class 
import Control.Monad.Trans.State 
import GHC.IORef 
import System.IO.Unsafe
import Network.Socket
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Aeson 
import Data.Aeson.Types ( parseMaybe )
import Data.Text  
import Control.Monad ((>>=))
import Data.Maybe
import Data.Conduit

plug :: ConduitT (Fin (Req Value)) S.ByteString IO () 
plug = a .| b .| c 

type Method = Text 
type Params = Value
type Id = Value

ioref :: IORef (Handle) 
ioref = unsafePerformIO $ newIORef stderr
{-# NOINLINE ioref #-} 

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
    monad (Just (Right (Nothing, m, p) )) = do 
        -- NOTIFICATION NO YIELD  a
        case m of 
            "channel_opened"         -> pure ()    
            "channel_state_changed"  -> pure ()    
            "connect"                -> pure ()     
            "disconnect"             -> pure ()    
            "invoice_payment"        -> pure ()    
            "invoice_creation"       -> pure ()    
            "warning"                -> pure ()    
            "forward_event"          -> pure ()    
            "sendpay_success"        -> pure ()    
            "sendpay_failure"        -> pure ()    
            "coin_movement"          -> pure ()    
            "balance_snapshot"       -> pure ()    
            "openchannel_peer_sigs"  -> pure ()    
            "shutdown"               -> pure ()    
            otherwise                -> pure ()       
    monad (Just (Right (Just i, m, p))) = 
        let rc = lift $ yield $ Res re i 
        in do 
            case m of
                -- INITIALIZATION 
                "init" -> do 
                    h <- liftIO $ connectSocket (fromInit p)
                    liftIO $ writeIORef ioref h 
                    rc 
                "getmanifest" -> lift $ yield $ Res manifest i
                -- HOOK MUST YIELD
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
                -- CUSTOM RPC
                "stormcircle" -> rc
                "stormsize" -> do 
                    mb <- liftIO $ getSize 
                    lift $ yield $ Res (msg $ pack $ show  mb ) i
                "stormcandidates" -> do 
                    c <- liftIO $ getCandidates  
                    lift $ yield $ Res (msg $ pack $ show $ top c) i 
                "stormload" -> do 
                    handle <- liftIO $ readIORef ioref 
                    w <- liftIO $ getinfo handle
                    case w of
                        (Just (Correct (Res g _))) -> do 
                            xl <- liftIO $ listchannels handle (__id g) 
                            case xl of 
                                (Just (Correct (Res xl' _))) -> do
                                    case toNode xl' of 
                                        Just n -> do 
                                            x <- liftIO $ leaveCrumbs handle n
                                            lift $ yield $ Res re i
                                        Nothing -> rc
                                a -> lift $ yield $ Res (msg $ (pack $ show g)) i  
                        a -> rc 
                "stormdeploy" -> rc 
                "stormrebalance" -> rc
                "stormbot" -> rc 
                -- BCLI OVERIDE (not enabled) 
                "getchaininfo" -> rc 
                "estimatefees" -> rc
                "getrawblockbyheight" -> rc 
                "getutxout" -> rc 
                "sendrawtransaction" -> rc  
                otherwise ->  pure ()
    monad _ = pure () 

c :: ConduitT (Res Value) S.ByteString IO () 
c = await >>= maybe mempty (\v -> yield $ L.toStrict $ encode v)  

re = object [ "result" .= ("continue"::Text) ]
msg x = object ["sigh" .= x] 

