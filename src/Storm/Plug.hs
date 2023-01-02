{-# LANGUAGE
      OverloadedStrings
    , DuplicateRecordFields
    , DeriveGeneric
    , FlexibleContexts
#-} 
module Storm.Plug where 

import Storm.Wallet 
import Storm.Search
import Storm.Balance 
import Storm.Graph
import Storm.Types 
import Storm.Deploy

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
import Control.Concurrent hiding (yield)
import qualified Data.Sequence as Q
import Data.Text.Format.Numbers
import Control.Lens hiding ((.=))
import Data.Aeson.Lens
import Data.Foldable 
import Data.Ratio 
import qualified Data.Map as M

data Storm = S {
      me :: Node
    , gg :: Gra
    , ci :: [Ref]
    , fu :: [Acc]
    }

eye = S 0 empty [] [] 

loadAccounts :: [LFChannel] -> [Acc] 
loadAccounts = map la 

la :: LFChannel -> Acc 
la l = (our % tot, n) 
    where our = our_amount_msat l
          tot = (amount_msat::LFChannel->Msat) l 
          n = getNodeInt $ (peer_id::LFChannel->String) l 


logy m = liftIO $ System.IO.appendFile "/home/o/.ao/storm" $ show m <> "\n"
storm :: Pluug Storm -- :)     

storm (Nothing, "coin_movement", v) = case (fromJSON v :: Result CoinMovement) of 
    Success (CoinMovement y) -> do 
        logy $ maybe 0 id $ (fees_msat :: Movement -> Maybe Int) y 
    _ -> pure ()  

storm (Just i, "stormload", v) =  do 
    h <- lift ask 
    Just (Correct (Res n _)) <- liftIO $ allnodes h
    Just (Correct (Res c _)) <- liftIO $ allchannels h
    Just (Correct (Res fo _)) <- liftIO $ getinfo h 
    Just (Correct (Res w _)) <- liftIO $ listfunds h 
    let {
        m = getNodeInt $ __id fo ; 
        g = loadGraph n c ;
        ord = order g ; 
        a = loadAccounts $ (channels :: ListFunds -> [LFChannel]) w ;
         }
    o <- liftIO $ loadCircles g m a
    lift.lift $ put $ S m g o a 
    yield $ Res (object [ 
          "loaded" .= True
        ,  "nodes" .= ord
        , "circles" .= length o
            ]) i

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
        , "circles" .= (length $ ci st) 
        , "levels" .= (foldr countlvls M.empty (level (me st) (gg st)) )
        ]) i
    where 
        countlvls :: (Node, Int) -> M.Map Int Int -> M.Map Int Int
        countlvls a b = case M.lookup (snd a) b of 
            Just y -> M.insert (snd a) (y + 1) b
            otherwise -> M.insert (snd a) 1 b  
        capacity :: Gra -> Msat -> Msat 
        capacity g t
            | isEmpty g = t
            | otherwise = case matchAny g of
                (n, g') -> capacity g' $ t + ( sum 
                    . (map (amount_msat::Channel -> Msat)) 
                    . (map snd)  
                    . lsuc' 
                    $ n )

storm (Just i, "stormdeploy", v) = do
    (S me g _ _) <- lift.lift $ get
    h <- lift ask 
    Just (Correct (Res w _)) <- liftIO $ listfunds h 
    ww <-liftIO $ deploy h (outputs w) g me 
    -- yield $ Res (object ["jail" .= ww ]) i 
    multifund <- liftIO $ multifundchannel h $ ww
    case multifund of 
        Just (Correct (Res mf _)) -> do 
            logy mf 
            yield $ Res (object ["suctest" .= mf ]) i 
        otherwise -> do 
            logy "failed" 
            logy multifund
            yield $ Res (object ["fail" .= True ]) i 


storm (Just i, "stormpaths", v) = do 
    st <- lift.lift $ get
    found <- liftIO $ runReaderT ( evalStateT (results' w) (Empty,[]) 
        ) (gg st,x,y)
    yield $ Res (object [ 
          "routes" .= map route found   
        ]) i
        where 
              route = (createRoute a) . toList 
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
storm x = logy "unhandled" >> logy x 

manifest :: Value
manifest = object [
    "dynamic" .= True,
    "rpcmethods" .= ([
          RpcMethod "stormwallet" "" "print summary info" Nothing False 
        , RpcMethod "stormload" "" "Load into memory (deprecating)" Nothing False  
        , RpcMethod "stormnetwork" "" "network summary info" Nothing False  
        , RpcMethod "stormpaths" "[n1, n2, a, p]" "Find p paths from n1 to n2 of amount a" Nothing False
        , RpcMethod "stormdeploy" "" "multiopen" Nothing False    
       -- , RpcMethod "stormrebalance" "" "rebalance attempts !!WARN!! possble 89 sat" Nothing False 
        ]),
    "options" .= ([]::[Option]),
    "featurebits" .= object [ ],
    "hooks" .= ([]::[Hook]),
    "notifications" .= ([]::[Notification]), 
    "subscriptions" .= (["coin_movement"
                        -- , "forward_event"
                        ] ::[Text])     
    ]
