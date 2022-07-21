{-# LANGUAGE 
    OverloadedStrings,
    GeneralizedNewtypeDeriving,
    DeriveGeneric
#-} 

module Lightningd where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types 

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

