{-# LANGUAGE 
      LambdaCase
    , OverloadedStrings 
#-}
module Plugin where 

import Jspec
import Manifest (manifest) 
import Lightningd 
import Cli
import Graph 
import Numeric
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
        -- NOTIFICATION 
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
        let rc = lift $ yield $ Res green i 
        in case m of
            -- INITIALIZATION 
            "init" -> do 
                h <- liftIO $ connectSocket (fromInit p)
                liftIO $ writeIORef ioref h 
                rc 
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
            -- STORM 
            "stormload" -> do 
                handle <- liftIO $ readIORef ioref 
                gra <- liftIO $ loadGraph handle 
                lift $ yield $ Res (object [
                      "nodes" .= order gra 
                    , "edges" .= size gra 
                    ]) i
            "stormsize" -> do 
                gra <- liftIO $ readIORef graphRef
                lift $ yield $ Res (object [
                      "nodes" .= order gra 
                    , "edges" .= size gra 
                    , "capacity" .= ufold (\c c' -> c' + (sum $ map (\(_,e) -> collat e) $ (lpre' c)) ) 0 gra  
                    ]) i
                where sel3 (_,_,e) = e 
            "stormcomponents" -> do 
                gra <- liftIO $ readIORef graphRef
                lift $ yield $ Res (object [ "components" .= map length (components gra)]) i
            "stormnode" -> do 
                gra <- liftIO $ readIORef graphRef
                paths <- liftIO $ findPaths x y  
                lift $ yield $ Res (object [
                      "levels" .= show (foldr countNode [] $ level x gra) 
                    , "paypaths" .= show (map cost paths)     
                    ]) i 
                where 
                    x = getNodeInt $ frip p 0
                    y = getNodeInt $ frip p 1
                    countNode (_, d) c = case (lookup d c) of 
                        (Just b) -> (d, b + 1) : (filter (\(x, _) -> x /= d) c)
                        Nothing -> (d, 1) : c
            "stormcircle" -> rc 
                
            -- BCLI ---- via electrum servers? light client  
            "getchaininfo" -> rc
            "estimatefees" -> rc
            "getrawblockbyheight" -> rc
            "getutxout" -> rc
            "sendrawtransaction" -> rc
            otherwise ->  do 
                rc
    monad _ = pure ()

frip :: Value -> Int -> String 
frip v i = case v ^? nth i . _String of
    (Just b) -> filter isHexDigit (show b) 
    Nothing -> ""

c :: ConduitT (Res Value) S.ByteString IO () 
c = await >>= maybe mempty (\v -> yield $ L.toStrict $ encode v)  

green = object [ "result" .= ("continue"::Text) ]
msg x = object ["sigh" .= x] 
tail' [] = [] 
tail' x = tail x 
