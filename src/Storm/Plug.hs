{-# LANGUAGE
      OverloadedStrings
    , DuplicateRecordFields
    , DeriveGeneric
    , FlexibleContexts
#-} 
module Storm.Plug where 

import Storm.Wallet 
import Storm.Search 
import Storm.Graph

import Cln.Conduit
import Cln.Types 
import Cln.Plugin
import Cln.Client 
import Cln.Route 

import System.IO 
import Data.Aeson
import Data.Text (Text)
import Control.Monad.Trans.Class 
import Control.Monad.Trans.State.Lazy 
import Data.Conduit
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query
import Control.Monad.Reader 
import Data.Text.Format.Numbers
import Control.Lens hiding ((.=))
import Data.Aeson.Lens
import Data.Foldable 

-- app data
data Storm = S {
      gg :: Gra 
    }

logy m = liftIO $ System.IO.appendFile "/home/o/.ao/storm" $ (show m) <> "\n"
-- identity value / starting state
eye = S empty

storm :: Pluug Storm -- :)     

-- handle notifications, no yield required
storm (Nothing, m, v) = case m of 
    "coin_movement" -> case ((fromJSON v) :: Result CoinMovement ) of 
        Success (CoinMovement a) -> do
            logy $ ff a 
            where ff a = if (fff a > 0) then (show $ fff a) else ""
                  fff a = maybe 0 id $ fees_msat a
        _ -> pure ()  

-- handle custom rpc and hooks - yield required 
-- use 'rc i' to continue
storm (Just i, m, v) =  case m of 
    "stormload" -> do 
        h <- lift ask 
        Just (Correct (Res n _)) <- liftIO $ allnodes h
        Just (Correct (Res c _)) <- liftIO $ allchannels h
        lift.lift $ put (S $ loadGraph c n) 
        (S g') <- lift.lift $ get
        yield $ Res (object [ "loaded" .= True  , "nodes" .= order g']) i
    "stormwallet" -> do 
          h <- lift ask
          Just (Correct (Res w _)) <- liftIO $ listfunds h 
          yield $ Res (summarizeFunds w) i 
    "stormnetwork" -> do 
          (S g) <- lift.lift $ get
            
          yield $ Res (object [
                "nodes" .= order g 
              , "edges" .= size g
              , "capacity" .= (prettyI (Just ',') $ capacity g 0 )
              ]) i
              where 
                capacity :: Gra -> Msat -> Msat 
                capacity g t
                    | isEmpty g = t
                    | otherwise = case matchAny g of
                        (n, g') -> capacity g' $ t + ( sum 
                            . (map (amount_msat::Channel -> Msat)) 
                            . (map snd)  
                            . lsuc' -- (outchannels) 
                            $ n 
                            ) 
    "stormpaths" -> do 
        (S g) <- lift.lift $ get
        found <- liftIO $ runReaderT (evalStateT (results w) (Empty,[])) (g,x,y)
        yield $ Res (object [ 
                  "routes" .= (map ((createRoute a).toList) $ found)   
              ]) i
        where 
              x = getNodeInt $ getArgStr 0 v
              y = getNodeInt $ getArgStr 1 v 
              a = getArgInt 2 v 1000000
              w = getArgInt 3 v 1
              getArgStr :: Int -> Value -> String  
              getArgStr i v = case v ^? ( (nth i) . _String) of
                  (Just b) -> show b  
                  Nothing -> ""
              getArgInt :: Int -> Value -> Int -> Int   
              getArgInt i v d = case v ^? ( (nth i) . _Integer) of 
                  Just b -> (fromInteger b)   
                  Nothing -> d 



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
