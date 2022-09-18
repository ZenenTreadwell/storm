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
import Paths
import Rebalance
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
import Data.Text.Format.Numbers
import Numeric

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
          liftIO loadGraph   
          g <- liftIO $ readIORef graphRef
          lift $ yield $ Res (object [
                "nodes loaded" .= order g 
              ]) i
    "stormsize" -> do 
          g <- liftIO $ readIORef graphRef
          lift $ yield $ Res (object [
                "nodes" .= order g 
              , "edges" .= size g
              , "capacity" .= capacity g 0 
              ]) i
    "stormcomponents" -> do 
          g <- liftIO $ readIORef graphRef
          lift $ yield $ Res (object [ "components" .= map length (components g) ]) i
    "stormnode" -> do 
          paths <- liftIO rebalance 
          lift $ yield $ Res (toJSON paths) i
    "stormpaths" -> do 
          paths <- liftIO $ bftFindPaths x y  
          lift $ yield $ Res (toJSON paths) i 
          where 
              x = getNodeInt $ getNodeArg 0 p
              y = getNodeInt $ getNodeArg 1 p 
    -- UTIL 
    "stormwallet" -> do 
          fun <- liftIO $ listfunds  
          case fun of 
              (Just (Correct (Res fun' _))) -> 
                  lift $ yield $ Res (summarizeFunds fun') i   
              (Just x) -> lift $ yield $ Res (object ["x" .= (show x)]) i
              otherwise -> rc
          where 
              summarizeFunds j = object [
                    "sat chain withdraw or channel(s)" .= (prettyI (Just ',') 
                        $ (`div` 1000) $ sum  $ map  (amount_msat :: LFOutput -> Msat) (outputs j))
                  , "sat lightning pay | invoice" .= (
                        (prettyI (Just ',') ourTote) <> " | " <> (prettyI (Just ',') (tote - ourTote) )   
                  ), "ye limbo" .=  (object $ map (\(s',i')-> ( (fromString s') .= (prettyI (Just ',') (div i' 1000)))) 
                                            $ filter (\x -> (fst x) /= ("CHANNELD_NORMAL"::String))
                                            $ foldr channelBreakdown [] c' )
                  , "ze balances" .= (countPots $ pots $ filter (isJust.sci) c') 
                  ]                
                  where tote = (`div`1000).sum $ map (amount_msat::LFChannel->Msat) normies
                        ourTote = (`div`1000).sum $ map our_amount_msat normies
                        normies = filter (\c -> __state c == "CHANNELD_NORMAL") $ c'
                        c' = (channels :: ListFunds -> [LFChannel]) j
                        channelBreakdown :: LFChannel -> [(String, Int)] -> [(String, Int)] 
                        channelBreakdown x a = case lookup (__state x) a of 
                            Just cur -> (__state  x, cur + (our_amount_msat :: LFChannel -> Msat) x) : 
                                        (filter ((/= (__state x)).fst) a)  
                            Nothing -> (__state  x, (our_amount_msat :: LFChannel -> Msat) x) : a

countPots (a,b,c,d,e) = object [
     "a. depleted" .= length a
    ,"b. mid-low" .= length b 
    , "c. balanced" .= length c 
    , "d. mid-high" .= length d
    , "e. full" .= length e  
    ]

getNodeArg i v = case v ^? nth i . _String of
    (Just b) -> filter isHexDigit (show b) 
    Nothing -> ""

