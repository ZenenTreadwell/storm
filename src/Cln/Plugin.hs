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
import Control.Monad.State.Lazy 

type Ploog a = ConduitT (Either (Res Value) PReq) (Res Value) a () 
type Pluug a = PReq -> Ploog a
type PReq = (Maybe Id, Method, Params)


logy m = liftIO $ System.IO.appendFile "/home/o/.ao/storm" $ (show m) <> "\n"

oop p =  sourceHandle stdin .| inConduit .| a .|  p .| c .| sinkHandle stdout

rc i = yield $ Res (object ["result" .= ("continue" :: Text)]) i  

-- plugin :: Manifest -> s -> Pluug (StateT s IO) -> IO ()   
plugin manif s p = do 
  liftIO $ mapM (flip hSetBuffering NoBuffering) [stdin,stdout] 
  runConduit $ oop $ do 
    logy "b4 mano" 
    (Just (Right (Just i, m, _))) <- await 
    case m of 
        "init" -> logy "init????"
        "getmanifest" -> do 
            logy i
            yield $ Res manif i -- hangs 
            
            logy "afta mano" 
        _ -> pure () 

  logy "afta mani"  
  runConduit $ oop $ do      
    (Just (Right (Just i, m, _))) <- await 
    case m of 
        "init" -> do 
            logy i
            rc i 
        _ -> pure () 
    
  runPlug s p
  where 
    runPlug s p = evalStateT (do 
        forever $ runConduit 
                $ sourceHandle stdin
                .| inConduit
                .| a
                .| b p 
                .| c 
                .| sinkHandle stdout
                ) s 

plug :: (Monad n) => Pluug n -> ConduitT (Fin (Req Value)) S.ByteString n () 
plug x = a .| b x .| c 

a :: (Monad n) => ConduitT (Fin (Req Value)) (Either (Res Value) (Maybe Id, Method, Params))  n () 
a = await >>= maybe mempty (\case  
    Correct v -> yield $ Right (getReqId v, getMethod v, getParams v) 
    InvalidReq -> yield $ Left $ Derp ("Request Error"::Text) Nothing  
    ParseErr -> yield $ Left $ Derp ("Parser Err"::Text) Nothing )

b :: (Monad n) => Pluug n -> Ploog n
b p =  await >>= monad 
    where 
    monad (Just(Left r)) = yield r  
    monad (Just (Right x)) = p x 
    monad _ = pure () 

c :: (Monad n) => ConduitT (Res Value) S.ByteString n () 
c = await >>= maybe mempty (\v -> yield $ L.toStrict $ encode v) 

