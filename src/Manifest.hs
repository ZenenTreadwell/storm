{-# LANGUAGE
    OverloadedStrings,
    DuplicateRecordFields,
    DeriveGeneric
 #-}                 

module Manifest (manifest) where 
import GHC.Generics
import Data.Text (Text) 
import Data.Aeson 

manifest :: Value
manifest = object [
    "options" .= ([]::[Option]), 
    "rpcmethods" .= ([
          RpcMethod "stormload" "0" "Loads map into memory" Nothing False  
        , RpcMethod "stormcircle" "a" "Returns a list of circular routes, sorted by fee" Nothing False
        , RpcMethod "stormrebalance" "b" "Keysend around circle" Nothing False 
        , RpcMethod "stormsize" "c" "Return info about total nodes stored in memory" Nothing False  
        , RpcMethod "stormcandidates" "d" "Targets bring you closer to many nodes" Nothing False 
        , RpcMethod "stormdeploy" "e" "Divide available between candidates & batch open" Nothing False 
        , RpcMethod "stormbot" "f" "Return chat link to simpleX bot" Nothing False
    ]), 
    "hooks" .= ([
        --  Hook "peer_connected" Nothing
        --, Hook "invoice_payment" Nothing -- HOLD TRANSACTIONS
        --, Hook "openchannel" Nothing 
        -- One of these Hooks causes plugin-topology to error
        --, Hook "htlc_accepted" Nothing 
        --, Hook "openchannel2" Nothing
        --, Hook "openchannel2_changed" Nothing
        --, Hook "openchannel2_sign" Nothing
        --, Hook "rbf_channel" Nothing
        --, Hook "rpc_command" Nothing 
        --, Hook "commitment_revocation" Nothing 
        --, Hook "onion_message_blinded" Nothing
        --, Hook "onion_message_ourpath" Nothing 
    ]::[Hook]), 
    "featurebits" .= object [],
    "notifications" .= ([]::[Notification]), 
    "subscriptions" .= ([
          "channel_opened"
        , "channel_state_changed" 
        , "connect" 
        , "disconnect" 
        , "invoice_payment"  
        , "invoice_creation" 
        , "warning" 
        , "forward_event" 
        , "sendpay_success" 
        , "sendpay_failure"
        , "coin_movement"
        , "balance_snapshot"
        , "openchannel_peer_sigs"
        , "shutdown"  
    ]::[Text]), 
    "dynamic" .= True  
    ]


data Option = Option {
    name :: Text 
  , _type :: Text
  , _default :: Text 
  , description :: Text
  , deprecated :: Bool
  } deriving Generic 
instance ToJSON Option where 
    toJSON = genericToJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = dropWhile (=='_')}

data RpcMethod = RpcMethod {
      name :: Text
    , usage  :: Text
    , description :: Text
    , long_description :: Maybe Text 
    , deprecated :: Bool
    } deriving Generic 
instance ToJSON RpcMethod where 
    toJSON = genericToJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = dropWhile (=='_')}
    
data Hook = Hook { 
    name :: Text 
  , before :: Maybe Value
  } deriving Generic 
instance ToJSON Hook where 
    toJSON = genericToJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = dropWhile (=='_')}

data Notification = Notification { 
    __method :: Text
  } deriving Generic
instance ToJSON Notification where 
    toJSON = genericToJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = dropWhile (=='_')}



