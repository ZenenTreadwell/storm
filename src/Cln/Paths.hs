{-# LANGUAGE 
    DuplicateRecordFields, 
    LambdaCase, 
    DeriveGeneric, 
    OverloadedStrings
#-}
module Cln.Paths where 
import Cln.Types 
import Cln.Client
import Cln.Graph
import System.IO
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import qualified Data.Sequence as Q
import Data.Sequence( Seq(..) , (<|) , (|>) , (><)) 
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query
import Data.Graph.Inductive.Query.BFS
import Data.Graph.Inductive.Internal.RootPath
import qualified Data.Graph.Inductive.Internal.Heap as Heep
import Data.Aeson 
import GHC.Generics
import Numeric 
import GHC.IORef
import Data.Function.Flip
import Data.List
import Data.Tuple
import Data.Tree

data PathInfo = P {
      hops :: Int
    , cost :: Fee
    , neck :: Sat
    } deriving (Generic, Show)
instance ToJSON PathInfo 

pInfo :: [Channel] -> PathInfo
pInfo = foldr pf (P 0 (Fee 0 0) maxBound)  
    where 
        pf :: Channel -> PathInfo -> PathInfo 
        pf e c = P 
            (hops c + 1)
            (Fee ( (base.cost) c + base_fee_millisatoshi e )
                 ( ((hops c)*((ppm.cost) c) + fee_per_millionth e ) `div` (hops c + 1) )
            )
            (case htlc_maximum_msat e of 
                (Just x) -> minimum [(neck c), ((amount_msat::Channel->Msat) e), x]
                otherwise -> min (neck c) ((amount_msat::Channel->Msat) e) 
            )

   
type SearchMeta = (Gra, Node, Node)
type RefPath = [Int]  

finPath :: ( [Channel] , Cxt ) -> Reader SearchMeta (Maybe [Channel])  
finPath (c, (ii, nn, nl, oo)) = do 
    (g,n,v) <- ask
    case filter ((== v).snd) oo of 
        [] -> pure Nothing 
        _ -> undefined -- Just $ fst x : c

--scan :: SearchMeta -> State RefPath [Channel] 
--scan m = undefined

nextRef' :: RefPath -> Reader SearchMeta RefPath 
nextRef' ref = do 
    m@(g,_,_) <- ask  
    case runReader (undefined ref) m of
        Nothing -> undefined-- pure $ take (length ref + 1) $ repeat 1
        Just c -> undefined 
    
seeker :: RefPath -> Reader SearchMeta (Either [Channel] RefPath)  
seeker r = do 
    s <- undefined r  
    case s of 
        Just x -> 
            let g = undefined
            in do 
                (g,_,v) <- ask 
                pure $ Left [] 
                -- checker (context g (last' s)) 
        Nothing -> undefined

            
 --         getNodeInt.destination' $ c
getChan :: Node -> Int -> Maybe Channel     
getChan n i = undefined

buildPaths :: [LPath Channel] -> [PathInfo] 
buildPaths = map $ pInfo . map snd . reverse . unLPath

type Sure = (Q.Seq (LPath Channel), [LPath Channel])
findPaths :: Node -> Node -> IO [PathInfo]
findPaths n1 n2 = do
    gra <- readIORef graphRef
    d   <- evalStateT (look n2 ( match n1 gra ) ) (Q.empty, [])
    pure $ sort $ buildPaths d
-- frontier can grows too large !!crashes
look :: Node 
     -> Dcp
     -> StateT Sure IO [LPath Channel] 
look v (Nothing, g) = stop
look v (Just (ii,nn,nl,oo), g) = get >>= \case 
    (Empty, []) -> do
        put $ (foldr (append (LP []) ) Q.empty oo, [])  
        next v
    (Empty, q) -> stop 
    ( p@(LP l) :<| ax , fin ) ->  
        if length fin > 1001 then stop 
        else if nn == v then do 
            put $ (ax, p : fin) 
            next v 
        else do 
            put $ (foldr (append p) ax oo , fin) 
            next v  
              
next :: Node -> StateT Sure IO [LPath Channel]
next v = get >>= \case 
    ( (LP ((n,c):ax)) :<| qx , fin ) -> do 
        g <- liftIO $ readIORef graphRef
        look v (match n g)
    otherwise -> do 
        stop

stop = do 
    (_, fin) <- get 
    return fin 

-- attempt that uses (abuses?) fgl shortest path 
-- 200 out channels/ 200 in channels = 40,000 shortest paths  
-- !! crashes too much memorya
-- !! thought this worked :( 
bftFindPaths :: Node -> Node -> IO [PathInfo]
bftFindPaths n v = do 
    gra <- readIORef graphRef
    case ( match n gra                ,  match v gra) of 
        ( (Just (_, _, _, outy), g) , (Just (inny, _,_,_) , _ ) ) -> 
            bftFP g oo ii
            where oo = filter (\(_, n') -> v /= n') outy  
                  ii = filter (\(_, n') -> n /= n') inny
        otherwise -> pure []    

bftFP :: Gra -> Adj Channel -> Adj Channel -> IO [PathInfo]
bftFP g outy inny = pure
    $ sort
    $ map pInfo   
    $ map repackPath
    $ filter (\(_,p,_)-> (length.unLPath) p > 0)
    $ map (\(a,pf,b) -> (a,pf g, b) )
    $ concat
    $ map (finPaths (take 2 inny)) 
    $ map outTree (take 2 outy) 

finPaths :: Adj Channel -> (Channel, Gra -> LRTree Channel) -> [ (Channel, Gra -> LPath Channel, Channel) ] 
finPaths i outT = map (fp outT) i
fp :: (Channel, Gra -> LRTree Channel) -> (Channel,Node) -> (Channel, Gra -> LPath Channel, Channel)
fp (f, t) (e, v) = (f, (getLPath v).t , e) 
outTree :: (Channel, Node) -> (Channel, Gra -> LRTree Channel) 
outTree o = (fst o, lbft $ snd o)   
repackPath :: (Channel, LPath Channel, Channel) -> [Channel]
repackPath (f,p,e) = [f] <> ((lp.unLPath) p) <> [e]
lp :: [LNode Channel] -> [Channel]
lp [] = [] 
lp (p:px) = map snd px

extendR :: Int -> SearchMeta -> LPath Channel-> LPath Channel
extendR = undefined 

startOf :: Int -> SearchMeta -> LPath Channel-> LPath Channel
startOf 0 _ p = p
startOf i m@(g, n1, n2) (LP []) = startOf (i-1) m $ LP $ take 1 $ lsuc g n1
startOf i m@(g, n1, n2) (LP p@(x@(n, _):_)) = LP $ (take 1 $ lsuc g n) <> p

source' :: Channel -> String 
source' = source
destination' :: Channel -> String 
destination' = destination 

last' :: [Channel] -> Node
last' = getNodeInt . destination' . last 

--ones :: Ref
ones = repeat 1 

--  package/extra-1.7.12

append :: LPath Channel -> (Channel, Node) -> Q.Seq (LPath Channel) -> Q.Seq (LPath Channel)
append (LP p) x q = q |> (LP $ (swap x) : p) 

instance Ord Fee where 
    compare a b = compare (base a + ppm a) (base b + ppm b)
instance Eq PathInfo where 
    (==) a b = (base.cost $ a) == (base.cost $ b) && (ppm.cost $ a) == (ppm.cost $ b)
instance Ord PathInfo where 
    compare a b = compare (cost a) (cost b) 
