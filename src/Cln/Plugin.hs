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
import System.Directory (getHomeDirectory)
import System.FilePath
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

loggy m = liftIO $ System.IO.appendFile ("/home/zen" </> ".ao" </> "storm") (show m <> "\n")

plugin :: Manifest -> s -> Pluug s -> IO ()   
plugin manif s p = do 
    liftIO $ mapM (flip hSetBuffering NoBuffering) [stdin,stdout] 
    runConduit $ runOnce $ do 
        (Just (Right (Just i, m, _))) <- await 
        if m == "getmanifest" then yield $ Res manif i else pure () 

    loggy $ "manifest served"

    runConduit $ runOnce $ do      
        init <- await 
        case init of
            (Just (Right (Just i, m, v))) -> do
                if m == "init" then case fromJSON v :: Result Init of
                    (Success x) ->  do
                        let socket_location = x.configuration.lightning5dir </> x.configuration.rpc5file
                        loggy x
                        soc <- liftIO $ socket AF_UNIX Stream 0
                        liftIO $ N.connect soc $ SockAddrUnix socket_location 
                        h <- liftIO $ socketToHandle soc ReadWriteMode
                        liftIO $ forkIO $ runPlug s p h 
                        loggy $ "plugin activated"
                        rc i 
                    _ -> pure ()  
                else pure () 

            Nothing -> do
    		loggy "init failed"
		pure ()

    threadDelay maxBound
    where 
    runPlug s p h = evalStateT (runReaderT (runForever p ) h ) s

-- runPlug s p h = evalStateT (runReaderT (runForever p ) h ) s

initialize s p = do
    loggy "check for init"
    -- (Just (Right (Just i, m, v))) 
    init <- await 
    case init of
        (Just x) -> do
    	    let (Right (Just i, m, v)) = x
            if m == "init" then case fromJSON v :: Result Init of
                (Success x) ->  do
                    soc <- liftIO $ socket AF_UNIX Stream 0
                    liftIO $ N.connect soc $ SockAddrUnix $ 
                        x.configuration.lightning5dir
                            <> "/" <> (x.configuration.rpc5file)
                    h <- liftIO $ socketToHandle soc ReadWriteMode
                    liftIO $ forkIO $ runPlug s p h 
                    rc i 
                _ -> pure ()  
            else pure () 

        _ -> pure ()

	-- recur until initialization is complete - bad idea
        -- _ -> initialize s p
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
