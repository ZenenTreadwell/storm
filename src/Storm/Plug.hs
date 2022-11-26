{-# LANGUAGE
      OverloadedStrings
    , DuplicateRecordFields
    , DeriveGeneric
#-} 
module Storm.Plug where 

import System.IO 
import Data.Aeson
import Data.Text (Text)
import Control.Monad.Trans.Class 
import Control.Monad.Trans.State.Lazy 
import Cln.Types 
import Cln.Plugin
import Cln.Client 
-- import Cln.Manifest 
import Data.Conduit
import Cln.Conduit
import Storm.Graph


data Storm = S {
      getClient :: Maybe Handle 
    , getGraph :: Maybe Gra
    } 
eye = S Nothing Nothing

storm :: PReq -> Ploog (StateT Storm IO)  
storm (Just i, m, p) = case m of 
    _ -> do 
        (S a b) <- lift get
        rc i
--    "stormload" -> do 
--          put (undefined) 
--          g <- get
--          yield $ Res (object [ "nodes loaded" .= order g ]) i
--    "stormnetwork" -> do 
--          g <- get
--          yield $ Res (object [
--                "nodes" .= order g 
--              , "edges" .= size g
--              --, "capacity" .= (prettyI (Just ',') $ capacity g 0 )
--              --, "x levels" .= (object
--              --    $ map (\(l,c)-> (fromString.show $ l) .= c )
--              --    $ foldr lvls [] (level (node'.fst.matchAny $ g) g)
--              --)
--              ]) i
--              --where lvls :: (Node, Int) -> [(Int, Int)] -> [(Int, Int)]
--              --       lvls (m, i) q = case lookup i q of 
--              --          Nothing -> (i, 1) : q 
--              --          Just x -> (i, x + 1) : filter ((/= i).fst) q 
----    "stormrebalance" -> do 
----          liftIO $ rebalance 89000
----          c <- liftIO $ readIORef circleRef
----          lift $ yield $ Res (toJSON (map (toJSON.snd) c)) i
--    "stormpaths" -> do 
--          g <- get
--          found <- liftIO $ runReaderT (evalStateT (results w) (Empty,[])) (g,x,y)
--          yield $ Res (object [ 
--                  "routes" .= (map ((createRoute a).toList) $ found)   
--              ]) i
--          where 
--              x = getNodeInt $ getArgStr 0 p
--              y = getNodeInt $ getArgStr 1 p 
--              a = getArgInt 2 p 1000000
--              w = getArgInt 3 p 1
--              getArgStr :: Int -> Value -> String  
--              getArgStr i v = case v ^? ( (nth i) . _String) of
--                  (Just b) -> show b  
--                  Nothing -> ""
--              getArgInt :: Int -> Value -> Int -> Int   
--              getArgInt i v d = case v ^? ( (nth i) . _Integer) of 
--                  Just b -> (fromInteger b)   
--                  Nothing -> d 
--              
--    -- UTIL 
--    "stormwallet" -> do 
--          w <- liftIO wallet
--          yield $ Res w i 



manifest :: Value
manifest = object [
    "dynamic" .= True,
    "rpcmethods" .= ([
          RpcMethod "stormwallet" "" "print summary info" Nothing False 
        , RpcMethod "stormload" "" "Load into memory (deprecating)" Nothing False  
        , RpcMethod "stormnetwork" "" "network summary info" Nothing False  
        , RpcMethod "stormpaths" "[n1, n2, a, p]" "Find p paths from n1 to n2 of amount a" Nothing False
        -- , RpcMethod "stormrebalance" "" "rebalance attempts !!WARN!! possble 89 sat" Nothing False 
        ]),
    "options" .= ([]::[Option]),
    "featurebits" .= object [ ],
    "hooks" .= ([]::[Hook]),
    "notifications" .= ([]::[Notification]), 
    "subscriptions" .= ([ "coin_movement" ] ::[Text])     
    ]
