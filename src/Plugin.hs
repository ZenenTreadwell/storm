{-# LANGUAGE 
    LambdaCase
    , OverloadedStrings 
    , DuplicateRecordFields
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

type Method = Text 
type Params = Value
type Id = Value

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
    "invoice_payment"        -> pure ()    
    "invoice_creation"       -> pure ()    
    "warning"                -> pure ()    
    "forward_event"          -> pure ()    
    "sendpay_success"        -> pure ()    
    "sendpay_failure"        -> pure ()    
    "coin_movement"          -> case ((fromJSON p) :: Result CoinMovement ) of 
        Success (a)             -> pure () 
        Error x                 -> pure ()                       
    "balance_snapshot"       -> pure ()
    "openchannel_peer_sigs"  -> pure ()    
    "shutdown"               -> pure ()    
    otherwise                -> pure ()       

hooks i m p = 
  let rc = lift $ yield $ Res (object [ "result" .= ("continue"::Text) ]) i 
  in case m of
    "init" -> do 
          liftIO $ connectCln $ fromInit p
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

    -- BCLI   
    "getchaininfo" -> rc
    "estimatefees" -> rc
    "getrawblockbyheight" -> rc
    "getutxout" -> rc
    "sendrawtransaction" -> rc

    -- GRAPH 
    "stormload" -> do 
          liftIO $ loadGraph   
          g <- liftIO $ readIORef graphRef
          lift $ yield $ Res (object [
                "nodes loaded" .= order g 
              ]) i
    "stormsize" -> do 
          g <- liftIO $ readIORef graphRef
          lift $ yield $ Res (object [
                "nodes" .= order g 
              , "edges" .= size g 
              , "capacity" .= calcCapacity g 0 
              ]) i
    "stormcomponents" -> do 
          g <- liftIO $ readIORef graphRef
          lift $ yield $ Res (object [ "components" .= map length (components g) ]) i
    "stormnode" -> rc
    "stormpaths" -> do 
          gra <- liftIO $ readIORef graphRef
          paths <- liftIO $ findPaths x y  
          lift $ yield $ Res (object [
                "levels" .= show (foldr countNode [] $ level x gra) 
              , "paypaths" .= show (map (\s -> (hops s, cost s) )  paths)     
              ]) i 
          where 
              x = getNodeInt $ getNodeArg 0 p
              y = getNodeInt $ getNodeArg 1 p 
              countNode (_, d) c = case (lookup d c) of 
                  (Just b) -> (d, b + 1) : (filter (\(x, _) -> x /= d) c)
                  Nothing -> (d, 1) : c
    -- UTIL 
    "stormwallet" -> do 
          fun <- liftIO $ listFunds  
          case fun of 
              (Just (Correct (Res fun' _))) -> 
                  lift $ yield $ Res (summarizeFunds fun') i   
              _ -> rc 
          where 
              summarizeFunds j = object [
                    "onChain" .= ((`div` 1000) $ sum  $ map ((read :: String -> Int) . (takeWhile isDigit) . (amount_msat :: LFOutput -> String)) (outputs j))
                  , "inChannel" .= (foldr channelBreakdown [] $ ((channels :: ListFunds -> [LFChannel]) j))  
                  ]                
                  where channelBreakdown :: LFChannel -> [(String, Int)] -> [(String, Int)] 
                        channelBreakdown x a = case lookup (__state x) a of 
                            Just cur -> (__state  x, cur + readAmount x) : (filter ((/= (__state x)).fst) a)  
                            Nothing -> (__state  x, readAmount x) : a
                        readAmount = ((read :: String -> Int) . (takeWhile isDigit) . (our_amount_msat :: LFChannel -> String))
    
getNodeArg i v = case v ^? nth i . _String of
    (Just b) -> filter isHexDigit (show b) 
    Nothing -> ""

