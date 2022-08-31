{-# LANGUAGE 
      LambdaCase
    , OverloadedStrings
    , DeriveGeneric
    , DeriveAnyClass
    , GeneralizedNewtypeDeriving
#-}

module Cli where 

import Jspec
import Lightningd

import System.IO
import GHC.IO.IOMode
import GHC.Generics

import Data.IORef
import Data.ByteString      as S
import Data.ByteString.Lazy as L 

import Control.Monad 
import Control.Monad.IO.Class as O
import qualified Control.Monad.Reader as R 
import Control.Monad.Trans 
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State 

import Data.Aeson 
import Network.Socket 

import Data.Conduit hiding (connect) 
import Data.Conduit.Combinators hiding (stdout, stderr, stdin) 

import Data.Aeson
import Data.Aeson.Types
import Data.Text

fromInit v = case fromJSON v of 
    Success (InitConfig f g) -> f <> "/" <> g 
    otherwise -> "" 

connectSocket descriptor = (socket AF_UNIX Stream 0) >>= \soc -> do 
    connect soc $ SockAddrUnix descriptor
    h <- socketToHandle soc ReadWriteMode
    pure h 


getinfo h = do 
    reqToHandle h $ Req ("getinfo"::Text) (object []) (Just (Number 3))  
    runConduit $ sourceHandle h 
               .| (jinjin :: ( ConduitT S.ByteString (Fin (Res GetInfo)) IO () ))
               .| (await >>= pure)      

channelsbysource h source = do 
    reqToHandle h $ Req ("listchannels"::Text) (object [ 
        "source" .= source 
        ]) (Just (Number 44))  
    runConduit $ sourceHandle h 
               .| (jinjin :: (ConduitT S.ByteString (Fin (Res ListChannels)) IO () ))
               .| (await >>= pure)  

allchannels h = do 
    reqToHandle h $ Req ("listchannels"::Text) (object []) (Just $ Number 33)     
    runConduit $ sourceHandle h 
               .| (jinjin :: (ConduitT S.ByteString (Fin (Res ListChannels)) IO () ))
               .| (await >>= pure)  

allnodes h = do 
    reqToHandle h $ Req ("listnodes"::Text) (object []) (Just $ Number 55)     
    runConduit $ sourceHandle h 
               .| (jinjin :: (ConduitT S.ByteString (Fin (Res ListNodes)) IO () ))
               .| (await >>= pure)  
 


listFunds h = do 
    reqToHandle h $ Req ("listfunds"::Text) (object []) (Just $ Number 88) 
    runConduit $ sourceHandle h 
               .| (jinjin :: (ConduitT S.ByteString (Fin (Res ListFunds)) IO () ))
               .| (await >>= pure)  
    




data InitConfig = InitConfig String String deriving (Generic) 
instance FromJSON InitConfig where 
    parseJSON (Object v) = do 
        x <- v .: "configuration"  
        InitConfig <$> (x .: "lightning-dir") 
                   <*> (x .: "rpc-file")
    parseJSON _ = mempty
instance ToJSON InitConfig where 
    toJSON (InitConfig x y) = object ["home" .= (x <> y) ] 

reqToHandle h r = L.hPutStr h $ encode r 

