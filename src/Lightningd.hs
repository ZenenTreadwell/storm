{-# LANGUAGE 
    OverloadedStrings,
    GeneralizedNewtypeDeriving,
    DeriveGeneric, 
    DuplicateRecordFields
#-} 

module Lightningd where

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
    , accounts :: Saccount } deriving (Show, Generic)
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
        , omitNothingFields = True } v
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

