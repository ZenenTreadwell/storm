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
import System.IO.Unsafe
import GHC.IO.IOMode
import GHC.IORef
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

-- Every time you use unsafePerformIO, a kitten dies. :(
clnref :: IORef (Handle) 
clnref = unsafePerformIO $ newIORef stderr
{-# NOINLINE clnref #-} 

connectSocket :: String -> IO ()
connectSocket descriptor = (socket AF_UNIX Stream 0) >>= \soc -> do 
    connect soc $ SockAddrUnix descriptor
    h <- socketToHandle soc ReadWriteMode
    writeIORef clnref h

getRes :: FromJSON a => IO (Maybe (Fin a))
getRes = do 
    h <- readIORef clnref
    runConduit $ sourceHandle h .| (jinjin) .| (await >>= pure)

reqToHandle :: ToJSON a => a -> IO ()
reqToHandle r = do
    h <- readIORef clnref 
    L.hPutStr h $ encode r 

type Cln a = IO (Maybe (Fin (Res a)))

getinfo :: Cln GetInfo 
getinfo = do 
    reqToHandle $ Req ("getinfo"::Text) (object []) (Just (Number 3))  
    getRes 

-- todo: figure out iterating the req Id number
-- todo: figure out not passing handle explicitly ?

channelsbysource :: String -> Cln ListChannels
channelsbysource source = do 
    reqToHandle $ Req ("listchannels"::Text) (object [ 
        "source" .= source 
        ]) (Just (Number 44))  
    getRes 


allchannels :: Cln ListChannels
allchannels = do 
    reqToHandle $ Req ("listchannels"::Text) (object []) (Just $ Number 33)     
    getRes 

allnodes :: Cln ListNodes
allnodes = do 
    reqToHandle $ Req ("listnodes"::Text) (object []) (Just $ Number 55)     
    getRes 

listFunds :: Cln ListFunds
listFunds = do 
    reqToHandle $ Req ("listfunds"::Text) (object []) (Just $ Number 88) 
    getRes 

fromInit :: Value -> String
fromInit v = case fromJSON v of 
    Success (InitConfig f g) -> f <> "/" <> g 
    otherwise -> "" 
data InitConfig = InitConfig String String deriving (Generic) 
instance FromJSON InitConfig where 
    parseJSON (Object v) = do 
        x <- v .: "configuration"  
        InitConfig <$> (x .: "lightning-dir") 
                   <*> (x .: "rpc-file")
    parseJSON _ = mempty
instance ToJSON InitConfig where 
    toJSON (InitConfig x y) = object ["home" .= (x <> y) ] 


