{-# LANGUAGE 
      LambdaCase
    , OverloadedStrings 
    , DuplicateRecordFields
#-}

module Cln.Plugin where 

import Cln.Conduit
import Cln.Types 
import Cln.Client 
import System.IO
import Data.Conduit
import Data.Conduit.Combinators (sourceHandle, sinkHandle) 
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Aeson 
import Data.Text (Text)  
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State 
import Control.Monad.Reader
import Control.Concurrent (forkIO, threadDelay)
import Network.Socket
import Network.Socket as N

type Ploog a = ConduitT (Either (Res Value) PReq) (Res Value) (ReaderT Handle (StateT a IO) ) () 
type Pluug a = PReq -> Ploog a
type PReq = (Maybe Id, Method, Params)

runOnce d =  sourceHandle stdin .| inConduit .| a .|  d .| c .| sinkHandle stdout

runForever p =  forever $ runConduit $ sourceHandle stdin .| inConduit .| (plug p) .| sinkHandle stdout
    where plug x = a .| b x .| c 

rc i = yield $ Res (object ["result" .= ("continue" :: Text)]) i  

plugin :: Manifest -> s -> Pluug s -> IO ()   
plugin manif s p = do 
    liftIO $ mapM (flip hSetBuffering NoBuffering) [stdin,stdout] 
    runConduit $ runOnce $ do 
        (Just (Right (Just i, m, _))) <- await 
        if m == "getmanifest" then yield $ Res manif i else pure () 
    runConduit $ runOnce $ do      
        (Just (Right (Just i, m, v))) <- await 
        if m == "init" then case fromJSON v :: Result Init of
            (Success x) ->  do
                soc <- liftIO $ socket AF_UNIX Stream 0
                liftIO $ N.connect soc $ SockAddrUnix $ 
                    ((lightning5dir::InitConfig -> String).configuration $ x) 
                        <> "/" <> (rpc5file.configuration $ x)
                h <- liftIO $ socketToHandle soc ReadWriteMode
                liftIO $ forkIO $ runPlug s p h 
                rc i 
            _ -> pure ()  
        else pure () 
    threadDelay maxBound
    where 
    runPlug s p h = evalStateT (runReaderT (runForever p ) h ) s

a :: (Monad n) => ConduitT (Fin (Req Value)) (Either (Res Value) (Maybe Id, Method, Params))  n () 
a = await >>= maybe mempty (\case  
    Correct v -> yield $ Right (getReqId v, getMethod v, getParams v) 
    InvalidReq -> yield $ Left $ Derp ("Request Error"::Text) Nothing  
    ParseErr -> yield $ Left $ Derp ("Parser Err"::Text) Nothing )

b p =  await >>= monad 
    where 
    monad (Just(Left r)) = yield r  
    monad (Just (Right x)) = p x 
    monad _ = pure () 

c :: (Monad n) => ConduitT (Res Value) S.ByteString n () 
c = await >>= maybe mempty (\v -> yield $ L.toStrict $ encode v) 
