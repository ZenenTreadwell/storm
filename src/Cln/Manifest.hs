{-# LANGUAGE
      OverloadedStrings
    , DuplicateRecordFields
    , DeriveGeneric
 #-}                 

module Cln.Manifest (manifest) where 
import GHC.Generics
import Data.Text (Text) 
import Data.Aeson 

manifest :: Value
manifest = object [
    "options" .= ([]::[Option]), 
    "rpcmethods" .= ([
          RpcMethod "stormwallet" "" "Show wallet totals and some channel info." Nothing False 
        , RpcMethod "stormload" "" "Loads graph into memory" Nothing False  
        , RpcMethod "stormnetwork" "" "Return info about nodes stored in memory" Nothing False  
        , RpcMethod "stormrebalance" "" "Init rebalance attempts. Spending cap 89 satoshi." Nothing False 
        , RpcMethod "stormpaths" "[n1, n2, a, p]" "Find p paths from n1 to n2 of amount a, sorted by fee." Nothing False 
    ]), 
    "hooks" .= ([
        Hook "invoice_payment" Nothing 
        --  Hook "peer_connected" Nothing
        --, Hook "openchannel" Nothing 
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
    "featurebits" .= object [ ],
    "notifications" .= ([]::[Notification]), 
    "subscriptions" .= ([
        "coin_movement"
        --, "channel_opened"
        --, "channel_state_changed" 
        --, "connect" 
        --, "disconnect" 
        --, "invoice_payment"  
        --, "invoice_creation" 
        --, "warning" 
        --, "forward_event" 
        --, "sendpay_success" 
        --, "sendpay_failure"
        --, "balance_snapshot"
        --, "openchannel_peer_sigs"
        --, "shutdown"  
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
    toJSON = genericToJSON defaultOptions{fieldLabelModifier = dropWhile (=='_')}

data RpcMethod = RpcMethod {
      name :: Text
    , usage  :: Text
    , description :: Text
    , long_description :: Maybe Text 
    , deprecated :: Bool
    } deriving Generic 
instance ToJSON RpcMethod where 
    toJSON = genericToJSON defaultOptions{omitNothingFields = True}
    
data Hook = Hook { 
    name :: Text 
  , before :: Maybe Value
  } deriving Generic 
instance ToJSON Hook where 
    toJSON = genericToJSON defaultOptions{omitNothingFields = True}

data Notification = Notification { 
    __method :: Text
  } deriving Generic
instance ToJSON Notification where 
    toJSON = genericToJSON defaultOptions{fieldLabelModifier = dropWhile (=='_')}


