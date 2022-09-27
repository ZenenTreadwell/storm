{-# LANGUAGE 
    OverloadedStrings,
    GeneralizedNewtypeDeriving,
    DeriveGeneric, 
    DuplicateRecordFields
#-} 
module Cln.Types where
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types 

type Sat = Int 
type Msat = Int
data Short = Short {
      block :: Int
    , input  :: Int 
    , output :: Int
    } deriving (Show, Generic, Eq)
data Fee = Fee {
      base :: Int 
    , ppm :: Int 
    } deriving (Show, Generic, Eq)  
instance ToJSON Fee

--Subscription data
data BalanceSnapshot = BalanceSnapshot { 
      balance_snapshots :: [Snapshot]
    } deriving (Generic, Show) 
instance FromJSON BalanceSnapshot 

data Snapshot = Snapshot {
      node_id :: String
    , blockheight :: Int 
    , timestamp :: Int 
    , accounts :: Saccount 
    } deriving (Show, Generic)
instance FromJSON Snapshot 

data Saccount = Saccount {
      account_id :: String 
    , balance :: String 
    , coin_type :: String
    } deriving (Show, Generic) 
instance FromJSON Saccount 

data CoinMovement = CoinMovement {
    coin_movement :: Movement } deriving (Show, Generic)
instance FromJSON CoinMovement 

data Movement = Movement {
      version :: Int 
    , node_id :: String 
    , __type :: String 
    , account_id :: String
    , originating_account :: Maybe String 
    , txid :: Maybe String 
    , utxo_txid :: Maybe String
    , vout :: Maybe Int 
    , part_id :: Maybe Int 
    , payment_hash :: Maybe String 
    , credit :: Maybe String
    , debit :: Maybe String
    , output_msat :: Maybe Int
    , output_count :: Maybe Int 
    , __fees :: Maybe String
    , tags :: [String]
    , blockheight :: Maybe Int 
    , timestamp :: Int 
    , coin_type :: String  
    } deriving (Show, Generic)
instance FromJSON Movement where 
    parseJSON v = genericParseJSON defaultOptions{fieldLabelModifier = dropWhile (=='_')} v

--Hook data
data InvoicePayment = InvoicePayment {
    payment :: Payment } deriving (Show, Generic)  
instance FromJSON InvoicePayment 

data Payment = Payment {
      label :: String 
    , preimage :: String 
    , amount_msat :: Int } deriving (Show, Generic) 
instance FromJSON Payment

--RPC data
data NewAddr = NewAddr {
    bech32 :: String 
    } deriving (Show, Generic)
instance FromJSON NewAddr

data Invoice = Invoice {
      payment_hash :: String
    , expires_at :: Int
    , bolt11 :: String
    , payment_secret :: String 
    } deriving (Generic, Show)       
instance FromJSON Invoice 

data GetInfo = GetInfo {
      __id :: String 
    , alias :: String
    , __color :: String 
    , num_peers :: Int 
    , num_pending_channels :: Int 
    , num_active_channels :: Int
    , num_inactive_channels :: Int
    --, __address :: [Addr]
    --, binding :: [Addr] 
    , __version :: String 
    , blockheight :: Int
    , __network :: String
    , fees_collected_msat :: Int
    , lightning5dir :: String
    , our_features :: Features 
    } deriving (Generic, Show)
instance ToJSON GetInfo
instance FromJSON GetInfo where
    parseJSON v = genericParseJSON defaultOptions{fieldLabelModifier = map repl . dropWhile (=='_')} v
repl '5' = '-'
repl o = o


data SendPay = SendPay {
      message :: Maybe String
    , _____id :: Int 
    , payment_hash :: String 
    , groupid :: Maybe Int
    , destination :: Maybe String 
    , amount_msat :: Maybe Msat 
    , amount_sent_msat :: Msat 
    , created_at :: Int 
    , status :: String 
    , label :: Maybe String 
    , partid :: Maybe Int 
    , bolt11 :: Maybe String 
    , bolt12 :: Maybe String 
    , payment_preimage :: Maybe String 
    , completed_at :: Maybe Int 
    } deriving (Generic, Show) 
instance FromJSON SendPay where 
    parseJSON v = genericParseJSON defaultOptions{
          omitNothingFields = True
        , fieldLabelModifier = dropWhile (=='_')} v
instance ToJSON SendPay where
    toJSON v = genericToJSON defaultOptions{fieldLabelModifier = dropWhile (=='_')} v 


data WaitSendPay = WaitSendPay {
      ___id :: Maybe Int 
    , payment_hash :: Maybe String 
    , status :: Maybe String 
    , created_at :: Maybe Int
    , amount_send_msat :: Maybe Msat 
    , groupid :: Maybe Int 
    , amount_msat :: Maybe Msat 
    , destination :: Maybe String 
    , completed_at :: Maybe Int
    , label :: Maybe String
    , partid :: Maybe Int 
    , bolt11 :: Maybe String 
    , bolt12 :: Maybe String  
    , payment_preimage :: Maybe String 
    , code :: Maybe Int 
    , message :: Maybe String 
    , __data :: Maybe FailSP  
    } deriving (Generic, Show) 
instance FromJSON WaitSendPay where 
    parseJSON v = genericParseJSON defaultOptions{
          omitNothingFields = True
        , fieldLabelModifier = dropWhile (=='_')} v
instance ToJSON WaitSendPay where
    toJSON v = genericToJSON defaultOptions{fieldLabelModifier = dropWhile (=='_')} v 

data FailSP = FailSP {
      ______id :: Maybe Int 
    , payment_hash :: Maybe String 
    , groupid :: Maybe Int
    , destination :: Maybe String 
    , amount_msat :: Maybe Msat 
    , created_at :: Maybe Int
    , status :: Maybe String 
    , erring_index :: Maybe Int 
    , failcode :: Maybe Int 
    , failcodename :: Maybe String 
    , erring_node :: Maybe String 
    , erring_channel :: Maybe String 
    , erring_direction :: Maybe Int 
    , raw_message :: Maybe String 
    } deriving (Generic, Show) 
instance FromJSON FailSP where 
    parseJSON v = genericParseJSON defaultOptions{
          omitNothingFields = True
        , fieldLabelModifier = dropWhile (=='_')} v
instance ToJSON FailSP where
    toJSON v = genericToJSON defaultOptions{fieldLabelModifier = dropWhile (=='_')} v 



data ListSendPays = ListSendPays {
    payments :: [SendPays] 
    } deriving (Generic, Show) 
instance FromJSON ListSendPays
instance ToJSON ListSendPays

data SendPays = SendPays {
      ______id :: Int 
    , groupid :: Int 
    , payment_hash :: String 
    , status :: String 
    , created_at :: Int
    , amount_sent_msat :: Msat 
    , amount_msat :: Maybe Msat 
    , destination :: Maybe String 
    , label :: Maybe String 
    , bolt11 :: Maybe String 
    , bolt12 :: Maybe String 
    , description :: Maybe String 
    , erroronion :: Maybe String 
    , payment_preimage :: Maybe String 
    } deriving (Generic, Show)
instance FromJSON SendPays where 
    parseJSON v = genericParseJSON defaultOptions{
          omitNothingFields = True
        , fieldLabelModifier = dropWhile (=='_')} v
instance ToJSON SendPays where
    toJSON v = genericToJSON defaultOptions{fieldLabelModifier = dropWhile (=='_')} v 

data SetChannel = SetChannel {
      peer_id :: String 
    , channel_id :: String 
    , fee_base_msat :: Msat 
    , fee_proportional_millionths :: Int 
    , minimum_htlc_out_msat :: Msat 
    , short_channel_id :: Maybe String 
    --, warning_htlcmin_too_low -- ?
    -- warning_htlcmax_too_high
    } deriving (Generic, Show)
instance FromJSON SetChannel  

data Features = Features {
      __init :: String
    , node :: String 
    , channel :: String 
    , invoice :: String 
    } deriving (Generic, Show)
instance ToJSON Features 
instance FromJSON Features where  
    parseJSON v = genericParseJSON defaultOptions{fieldLabelModifier = dropWhile (=='_')} v
                                                                       
data ListChannels = ListChannels { 
        channels :: [Channel] 
        } deriving (Generic, Show)       
instance FromJSON ListChannels     
instance ToJSON ListChannels 

data Channel = Channel {
      source :: String
    , destination :: String 
    , short_channel_id :: String
    , public :: Bool
    , amount_msat :: Msat
    , message_flags :: Int
    , channel_flags :: Int
    , active :: Bool
    , last_update :: Int
    , base_fee_millisatoshi :: Int
    , fee_per_millionth :: Int 
    , delay :: Int
    , htlc_minimum_msat :: Msat
    , htlc_maximum_msat :: Maybe Msat
    , features :: String 
    } deriving (Show, Generic) 
instance FromJSON Channel where 
    parseJSON v = genericParseJSON defaultOptions{
          fieldLabelModifier = dropWhile (=='_')
        , omitNothingFields = True } v
instance ToJSON Channel

data ListForwards = ListForwards {
    forwards :: [Forward] 
    }deriving (Show, Generic)
instance ToJSON ListForwards
instance FromJSON ListForwards 
-- incorrect?
data Forward = Forward {
      in_channel :: String
    , in_htlc_id :: Int 
    , in_msat :: Msat 
    , status :: String 
    , received_time :: Int 
    , out_channel :: Maybe String 
    , out_htlc_id :: Maybe Int 
    , style :: String 
    , fee_msat :: Maybe Msat
    , out_msat :: Maybe Msat
    , resolved_time :: Maybe Int 
    , failcode :: Maybe Int 
    , failreason :: Maybe String 
    }deriving (Show, Generic)    
instance ToJSON Forward
instance FromJSON Forward where 
    parseJSON v = genericParseJSON defaultOptions{
          fieldLabelModifier = dropWhile (=='_')
        , omitNothingFields = True } v

data ListNodes = ListNodes {
      _nodes :: [NodeInfo]
    } deriving (Show, Generic)
instance ToJSON ListNodes 
instance FromJSON ListNodes where 
    parseJSON v = genericParseJSON defaultOptions{fieldLabelModifier = dropWhile (=='_')} v

data NodeInfo = NodeInfo {
      nodeid :: String 
    , last_timestamp :: Maybe Int 
    , __alias :: Maybe String 
    , __color :: Maybe String 
    , features :: Maybe String
    --, addresses :: Maybe [Addr] 
    , option_will_fund :: Maybe WillFund  
    } deriving (Generic, Show) 
instance ToJSON NodeInfo
instance FromJSON NodeInfo where 
    parseJSON = genericParseJSON defaultOptions{
          fieldLabelModifier = dropWhile (=='_')
        , omitNothingFields = True } 
data Addr = Addr {
      __type :: String 
    , __address :: String 
    , __port :: String 
    } deriving (Generic, Show) 
instance ToJSON Addr
instance FromJSON Addr where 
    parseJSON v = genericParseJSON defaultOptions{fieldLabelModifier = dropWhile (=='_')} v
data WillFund = WillFund {
      lease_fee_base_msat :: Int
    , lease_fee_basis :: Int
    , funding_weight :: Int
    , channel_fee_max_base_msat :: Int 
    , compact_lease :: String 
    } deriving (Generic, Show)
instance ToJSON WillFund
instance FromJSON WillFund 

data ListFunds = ListFunds {
      outputs :: [LFOutput]
    , channels :: [LFChannel]     
    } deriving (Show, Generic) 
instance FromJSON ListFunds
instance ToJSON ListFunds

data LFOutput = LFOutput {
      txid :: String 
    , output :: Int
    , amount_msat :: Msat  
    , scriptpubkey :: String 
    , address :: String 
    , status :: String 
    , blockheight :: Maybe Int 
    , reserved :: Bool 
    , reserved_to_block :: Maybe Int 
    } deriving (Show, Generic)  
instance FromJSON LFOutput where
    parseJSON v = genericParseJSON defaultOptions{
          fieldLabelModifier = dropWhile (=='_')
        , omitNothingFields = True} v
instance ToJSON LFOutput

data LFChannel = LFChannel {
      peer_id :: String
    , connected :: Bool
    , __state :: String
    , short_channel_id :: Maybe String
    , our_amount_msat :: Msat  
    , amount_msat :: Msat 
    , funding_txid :: Maybe String
    , funding_output :: Maybe Int 
    } deriving (Show, Generic) 
instance FromJSON LFChannel where 
    parseJSON v = genericParseJSON defaultOptions{
          fieldLabelModifier = dropWhile (=='_')
        , omitNothingFields = True } v
instance ToJSON LFChannel 

data GetRoute = GetRoute {
      route :: [Route] 
    } deriving (Show, Generic)
instance ToJSON GetRoute
instance FromJSON GetRoute 

data Route = Route {
      ___id :: String 
    , channel :: String 
    , direction :: Int
    , amount_msat :: Msat 
    , delay :: Int 
    , style :: String  
    } deriving (Show, Generic, Eq) 
instance FromJSON Route where 
    parseJSON v = genericParseJSON defaultOptions{fieldLabelModifier = dropWhile (=='_')} v
instance ToJSON Route where
    toJSON v = genericToJSON defaultOptions{fieldLabelModifier = dropWhile (=='_')} v 

