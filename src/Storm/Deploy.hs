
{-# LANGUAGE
    DuplicateRecordFields, 
    LambdaCase, 
    DeriveGeneric, 
    OverloadedStrings, 
    BangPatterns
#-}

module Storm.Deploy where 

import System.Random
import System.IO 
import Data.Maybe

import Data.Graph.Inductive.Query.BFS
import Data.Graph.Inductive.Graph
import Data.Sequence(Seq(..)) 
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

-- import Data.Foldable 
import Data.List 


import Cln.Types
import Cln.Conduit
import Cln.Client 
import Storm.Graph

import Storm.Search 
import Storm.Types

loggy m = liftIO $ System.IO.appendFile "/home/zen/.ao/storm" $ show m <> "\n"

ize :: Sat 
ize = 7654321

deploy :: Handle -> [LFOutput] -> Gra -> Node -> IO Destinations
deploy h lfo g me = 
    let cand = findCand g me
        amm = flip div 1000 $ sum $ map (.amount_msat) lfo 
        xg :: StateT (Sat, Destinations, Destinations) IO Destinations
        xg = do 
            (a, c, r) <- get
            case c of 
                [] -> pure r 
                (c':cx) -> if a > ize 
                    then do 
                        con <- liftIO $ connect' h $  ___________id c'
                        case con of 
                            Just (Correct (Res w _)) -> do
                                loggy $ "correct peer" <> (show . length) r
                                put (a - ize, cx, c':r) 
                                xg
                            otherwise -> do 
                                loggy $ "failed connect" <> (show . length) cx
                                put (a, cx, r) 
                                xg
                    else pure r
    in evalStateT xg (amm - 95000, cand, [])    

findCand :: Gra -> Node -> Destinations
findCand g me =
    let 
        far :: [Cxt]
        far = map (\(n , i) -> context g n )
            $ filter (\(n, i) -> length (suc g n) > 11 && i > 2)
            $ level me g -- [(Node,Int)] -- by depth
    in do 
        map (\(ii, nn, nl, oo) -> Desti ((nodeid :: NodeInfo -> String) nl) ize) far
            
