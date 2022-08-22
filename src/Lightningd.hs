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

-- helps 
type Sat = Int 
type Msat = Int

data Short = Short { 
      block :: Int
    , input  :: Int 
    , output :: Int
    } deriving (Show, Generic, Eq)
--instance ToJSON Short where 

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
    , coin_type :: String } deriving (Show, Generic) 
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
    , credit_msat :: Int
    , debit_msat :: Int
    , output_msat :: Maybe Int
    , output_count :: Maybe Int 
    , fees_msat :: Maybe Int 
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
        , lightning5dir  :: String
        , num_active_channels :: Int
        , num_inactive_channels :: Int
        , blockheight :: Int
        , network :: String
        , msatoshi_fees_collected :: Int
        } deriving (Generic, Show)
instance ToJSON GetInfo
instance FromJSON GetInfo where
    parseJSON v = genericParseJSON defaultOptions{fieldLabelModifier = map repl . dropWhile (=='_')} v
repl '5' = '-'
repl o = o
                                                                       
data ListChannels = ListChannels { 
        channels :: [Channel] 
        } deriving (Generic, Show)       
instance FromJSON ListChannels     
instance ToJSON ListChannels 

data Channel = Channel {
      source :: String
    , destination :: String 
    , short_channel_id :: String
    , __public :: Bool
    , satoshis :: Int
    , last_update :: Int
    , base_fee_millisatoshi :: Int
    , fee_per_millionth :: Int 
    , delay :: Int
    , htlc_minimum_msat :: String
    , htlc_maximum_msat :: String
    , features :: String 
    } deriving (Show, Generic) 
instance FromJSON Channel where 
    parseJSON v = genericParseJSON defaultOptions{fieldLabelModifier = dropWhile (=='_')} v
instance ToJSON Channel


data ListNodes = ListNodes {
    eNodes :: [NodeInfo]} deriving (Show, Generic)
instance ToJSON ListNodes 
instance FromJSON ListNodes 

data NodeInfo = NodeInfo {
      nodeid :: String 
    , alias :: Maybe String 
    , color :: Maybe String 
    , last_timestamp :: Maybe Int 
    , features :: Maybe String
    --, addresses :: [Addr]   
    } deriving (Generic, Show) 
instance FromJSON NodeInfo
instance ToJSON NodeInfo

--data Addr = Addr {
