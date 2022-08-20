{-# LANGUAGE 
      LambdaCase
    , OverloadedStrings 
#-}
module Plugin where 

import Jspec
import Manifest (manifest) 
import Lightningd 
import Cli
import Nodes
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
        -- NOTIFICATION NO YIELD  
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
                -- CUSTOM RPC V1
                "stormcircle" ->   
                    (liftIO $ lookupNode (filter ((/=) '\"') (show $ frip p) )) >>= 
                    \case 
                        Just n ->  do 
                            x <- liftIO $ getCircles n 
                            lift $ yield $ Res (msg.pack.show $ x) i
                        Nothing -> rc
                "stormsize" -> do
                    mb <- liftIO $ getSize
                    lift $ yield $ Res (object [
                          "nodes" .= mSize mb
                        , "capacity (sat)" .= mCapacity mb
                        , "distance, new nodes encountered" .= (tail' $ histo mb)  
                        ]) i
                "stormcandidates" -> do 
                    c <- liftIO $ getCandidates
                    lift $ yield $ Res (msg $ pack $ show $ nodeId <$> top c) i 
    
                "stormload" -> do 
                    handle <- liftIO $ readIORef ioref 
                    w <- liftIO $ getinfo handle
                    case w of
                        (Just (Correct (Res g _))) -> do 
                            xl <- liftIO $ channelsbysource handle (__id g) 
                            case xl of 
                                (Just (Correct (Res xl' _))) -> do
                                    x <- liftIO $ leaveCrumbs handle (toNode xl')
                                    mb <- liftIO $ getSize
                                    lift $ yield $ Res (object [
                                          "nodes" .= mSize mb 
                                        , "capacity (sat)" .= mCapacity mb
                                        ]) i
                                a -> lift $ yield $ Res (msg $ (pack $ show g)) i  
                        a -> rc
                "stormnode" -> do
                    h <- liftIO $ readIORef ioref
                    n <- liftIO $ lookupNode' (filter ((/=) '\"') (show $ frip p) ) 
                    lift $ yield $ Res (object [
                          "nodeid" .= nodeId n 
                        , "channels" .= (length.edges') n 
                        , "capacity" .=  sum (map sats $ edges' n)
                        , "paths to:" .= crumbs n 
                        ]) i
                "stormdeploy" -> rc
                "stormrebalance" -> rc
                -- V2
                "v2stormload" -> do
                    handle <- liftIO $ readIORef ioref 
                    gra <- liftIO $ loadGraph handle 
                    lift $ yield $ Res (object [
                          "nodes" .= order gra 
                        , "edges" .= size gra 
                        ]) i
                "v2stormsize" -> do 
                    gra <- liftIO $ readIORef graphRef
                    -- this needs to be updated to use multithread
                    lift $ yield $ Res (object [
                          "nodes" .= order gra 
                        , "edges" .= size gra 
                        , "isConnected" .= isConnected gra 
                        , "components" .= map length (components gra)
                        , "capacity" .= (foldr (\e t -> (collat.sel3) e + t) 0 
                            $ nubBy (\e e' -> (short.sel3) e == (short.sel3) e') (labEdges gra))
                        ]) i
                    where sel3 (_,_,e) = e 
                "v2stormnode" -> do 
                    gra <- liftIO $ readIORef graphRef
                    lol <- pure $ (fst.head.readHex $ (filter ((/=) '\"') (show $ frip p)))
                    lift $ yield $ Res (object [
                          "levels" .= show (foldr countNode [] $ level lol gra) 
                        , "reachable" .= length $ reachable lol gra
                        ]) i 
                    where 
                        countNode (_, d) c = case (lookup d c) of 
                            (Just b) -> (d, b + 1) : (filter (\(x, _) -> x /= d) c)
                            Nothing -> (d, 1) : c

                "v2stormcircle" -> rc 
                    
                -- BCLI OVERIDE (not enabled) - via electrum?
                "getchaininfo" -> rc
                "estimatefees" -> rc
                "getrawblockbyheight" -> rc
                "getutxout" -> rc
                "sendrawtransaction" -> rc
                otherwise ->  do 
                    rc
    monad _ = pure ()

frip :: Value -> Text 
frip v = case v ^? nth 0 . _String of
    (Just b) ->  b
    Nothing -> ""

c :: ConduitT (Res Value) S.ByteString IO () 
c = await >>= maybe mempty (\v -> yield $ L.toStrict $ encode v)  

green = object [ "result" .= ("continue"::Text) ]
msg x = object ["sigh" .= x] 
tail' [] = [] 
tail' x = tail x 
