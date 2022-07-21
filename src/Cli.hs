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

import Data.Aeson 
import Network.Socket 
import Network.Socket.ByteString (send, recv) 

import Data.Conduit hiding (connect) 
import Data.Conduit.Combinators hiding (stdout) 

import Data.Aeson
import Data.Aeson.Types
import Data.Text

getinfo h = do 
    reqToHandle h $ Req ("getinfo"::Text) (object []) (Just (Number 3))  
    runConduit $ sourceHandle h 
               .| (jinjin :: ( ConduitT S.ByteString (Fin (Res GetInfo)) IO () ))
               .| (await >>= pure)      

listchannels h source = do 
    reqToHandle h $ Req ("listchannels"::Text) (object [ 
        "source" .= source 
        ]) (Just (Number 44))  
    runConduit $ sourceHandle h 
               .| (jinjin :: (ConduitT S.ByteString (Fin (Res ListChannels)) IO () ))
               .| (await >>= pure)  

fromInit v = case fromJSON v of 
    Success (InitConfig f g) -> f <> "/" <> g 
    otherwise -> "" 

connectSocket descriptor = (socket AF_UNIX Stream 0) >>= \soc -> do 
    connect soc $ SockAddrUnix descriptor
    h <- socketToHandle soc ReadWriteMode
    pure h 

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

