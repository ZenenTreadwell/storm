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

type Ploog = ConduitT (Either (Res Value) (Maybe Id, Method, Params)) (Res Value) IO () 
type Pluug = (Maybe Id, Method, Params) -> Ploog

plugin c = runPlug $ plug c
    where 
    runPlug plug = do 
        mapM (flip hSetBuffering NoBuffering) [stdin,stdout] 
        forever $ runConduit 
                $ sourceHandle stdin
                .| inConduit
                .| plug
                .| sinkHandle stdout


plug :: Pluug -> ConduitT (Fin (Req Value)) S.ByteString IO () 
plug x = a .| b x .| c 

a :: (Monad n) => ConduitT (Fin (Req Value)) (Either (Res Value) (Maybe Id, Method, Params))  n () 
a = await >>= maybe mempty (\case  
    Correct v -> yield $ Right (getReqId v, getMethod v, getParams v) 
    InvalidReq -> yield $ Left $ Derp ("Request Error"::Text) Nothing  
    ParseErr -> yield $ Left $ Derp ("Parser Err"::Text) Nothing )

b :: Pluug -> Ploog
b p =  await >>= monad 
    where 
    monad (Just(Left r)) = yield r  
    monad (Just (Right x)) = p x 
    monad _ = pure () 

c :: ConduitT (Res Value) S.ByteString IO () 
c = await >>= maybe mempty (\v -> yield $ L.toStrict $ encode v) 

