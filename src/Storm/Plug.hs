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
    , ci :: [Ref]
    , fu :: Acc
    }

data Acc = A -- (Ratio, Node) 

-- starting state
eye = S empty [] A

logy m = liftIO $ System.IO.appendFile "/home/o/.ao/storm" $ show m <> "\n"

storm :: Pluug Storm -- :)     
storm (Nothing, "coin_movement", v) = do 
    case (fromJSON v :: Result CoinMovement) of 
        Success (CoinMovement y) -> do 
            logy $ maybe 0 id $ (fees_msat :: Movement -> Maybe Int) y 
            -- // st <- lift.lift $ get
            -- undefined 
        _ -> pure ()  
storm (Nothing, m, v) = logy m 

storm (Just i, "stormload", v) =  do 
    h <- lift ask 
    Just (Correct (Res n _)) <- liftIO $ allnodes h
    Just (Correct (Res c _)) <- liftIO $ allchannels h
    lift.lift $ put (S (loadGraph c n) [] A) 
    st <- lift.lift $ get
    yield $ Res (object [ "loaded" .= True  , "nodes" .= order (gg st)]) i
storm (Just i, "stormwallet", v) = do 
    h <- lift ask
    Just (Correct (Res w _)) <- liftIO $ listfunds h 
    yield $ Res (summarizeFunds w) i 
storm (Just i, "stormnetwork", v) = do 
    st <- lift.lift $ get
    yield $ Res (object [
          "nodes" .= order (gg st)  
        , "edges" .= size (gg st)
        , "capacity" .= (prettyI (Just ',') $ capacity (gg st) 0 )
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
                            $ n )
                    


storm (Just i, "stormpaths", v) = do 
    st <- lift.lift $ get
    found <- liftIO $ runReaderT (evalStateT (results w) (Empty,[])) (gg st,x,y)
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
    "subscriptions" .= (["coin_movement", "forward_event"] ::[Text])     
    ]
