{-# LANGUAGE
      LambdaCase
    , OverloadedStrings
    , DeriveGeneric
    , DeriveAnyClass
    , GeneralizedNewtypeDeriving
#-}

module Cln.Client where 

import Cln.Conduit
import Cln.Types
import System.IO
import System.IO.Unsafe
import GHC.IO.IOMode
import GHC.IORef
import GHC.Generics
import Data.IORef
import Data.ByteString      as S
import Data.ByteString.Lazy as L 
import Control.Monad 
import Control.Monad.State
import Control.Monad.IO.Class as O
import Data.Aeson 
import Network.Socket 
import Data.Conduit hiding (connect) 
import Data.Conduit.Combinators hiding (stdout, stderr, stdin) 
import Data.Aeson
import Data.Aeson.Types
import Data.Text

type Cln a = IO (Maybe (Fin (Res a)))

idref :: IORef Int
idref = unsafePerformIO $ newIORef (1 :: Int) 
clnref :: IORef Handle 
clnref = unsafePerformIO $ newIORef stderr
connectCln :: String -> IO ()
connectCln d = (socket AF_UNIX Stream 0) >>= \soc -> do 
    connect soc $ SockAddrUnix d
    h <- socketToHandle soc ReadWriteMode
    writeIORef clnref h

tick :: (FromJSON a , ToJSON w)=> (Maybe Value -> Req w) -> Cln a
tick v = do 
    i <- readIORef idref
    h <- readIORef clnref
    writeIORef idref (i + 1) 
    reqToHandle $ v (Just $ toJSON i) 
    runConduit $ sourceHandle h .| inConduit .| await >>= pure

reqToHandle :: ToJSON a => a -> IO ()
reqToHandle a = (readIORef clnref) >>= \h -> L.hPutStr h $ encode a 

newaddr :: Cln NewAddr 
newaddr =  tick $ Req ("newaddr"::Text) (object []) 

b11invoice ::  Msat -> String -> String -> Cln Invoice 
b11invoice a l d = tick $ Req ("invoice"::Text) (object [
          "amount_msat" .= a 
        , "label" .= (l)
        , "description" .= d ])  

sendpay :: [Route] -> String -> String -> Cln SendPay 
sendpay r h v =  tick $ Req ("sendpay"::Text) (object [
          "route" .= r
        , "payment_hash" .=  h
        , "payment_secret" .= v
        ])  

listsendpays :: String -> Cln ListSendPays 
listsendpays h = tick $ Req ("listsendpays"::Text) (object [
        "payment_hash" .=  h ]) 

waitsendpay :: String -> Cln WaitSendPay 
waitsendpay h =  tick $ Req ("waitsendpay"::Text) (object [
          "payment_hash" .=  h 
        , "timeout" .= (33 :: Int)  ])

setchannel :: String -> Int -> Int -> Cln SetChannel 
setchannel n b p =  tick $ Req ("setchannel"::Text) (object [
          "id" .= n
        , "feebase" .= b
        , "feeppm" .= p 
        ])

getinfo :: Cln GetInfo 
getinfo =  tick $ Req ("getinfo"::Text) (object [])  

channelsbysource :: String -> Cln ListChannels
channelsbysource source =  tick $ Req ("listchannels"::Text) (object [ 
        "source" .= source 
        ])   

getroute :: ToJSON a => a -> a -> a -> Cln GetRoute 
getroute j m r = tick $ Req ("getroute"::Text) (object [
          "id" .= j 
        , "msatoshi" .= m
        , "riskfactor" .= r
        ])  

allchannels :: Cln ListChannels
allchannels =  tick $ Req ("listchannels"::Text) (object [])     

allnodes :: Cln ListNodes
allnodes =  tick $ Req ("listnodes"::Text) (object [])      

allforwards :: Cln ListForwards
allforwards =  tick $ Req ("listforwards"::Text) (object [
        "status" .= ("settled"::Text) ]) 

listfunds :: Cln ListFunds
listfunds = tick $ Req ("listfunds"::Text) (object [])  
