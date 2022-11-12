{-# LANGUAGE
    LambdaCase, 
    DuplicateRecordFields
#-} 

module Cln.Search where 

import Cln.Types
import Cln.Graph
import Data.Graph.Inductive.Graph
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy -- (lazy?) 
import qualified Data.Sequence as Q
import Data.Sequence( Seq(..) , (<|) , (|>) , (><)) 
import Data.Foldable 

type Search = Reader (Gra, Node, Node)  -- from / to
type Ref = Q.Seq Int
type Way = Q.Seq Channel 
type DeRef = (Cxt, Ref, Way)

results :: Int -> StateT (Ref, [Way]) Search [Way] 
results x =   
    do 
        (r , c) <- get
        (xo, r') <- lift $ search r
        put (increment r', xo : c) 
        if x > length c 
            then results x 
            else return c

search :: Ref -> Search (Way, Ref) 
search ref = (fetchRef ref) >>= \case
    (Left x) -> search $ nextRef ref x
    (Right y) -> (checkFound y) >>= \case  
        Nothing -> search $ increment ref
        (Just y) -> pure (y, ref)  

nextRef :: Ref -> DeRef -> Ref 
nextRef r ((ii,nn,ll,oo), r', c) = 
    let 
        x = Q.length r
        y = Q.length r' 
        z = Q.length c 
    in if (x == z) 
        then oldHoppy r 
        else if 0 == z 
            then extendTo (length r + 1) Empty
            else extendTo (length r) $ increment $ Q.take z r 

fetchRef :: Ref -> Search (Either DeRef Way) 
fetchRef ref = do
    (g,n,v) <- ask  
    evalStateT getChans (context g n, ref, Empty) 

getChans :: StateT DeRef Search (Either DeRef Way)
getChans = get >>= \case 
    (_, Empty, c) -> pure (Right c) 
    x@((ii,nn,ll,oo), y :<| t, c) -> do 
        (g, n, v) <- lift ask
        case oo !? y of 
            Nothing -> pure $ Left x    
            Just (c', n') -> do 
                put ( context g n' , t , c |> c' )
                getChans

checkFound :: Way -> Search (Maybe Way) 
checkFound Empty = do  
    (g,n,v) <- ask
    final Empty n    
checkFound a@(ax :|> b) = 
    let v' = getNodeInt.(destination :: Channel -> String) $ b 
    in final a v'  

final :: Way -> Node -> Search (Maybe Way) 
final a v' = do  
    (g,n,v) <- ask
    case (filter ((== v').fst) $ lpre g v) of 
        [] -> pure Nothing 
        (x:_) -> pure $ Just $ a |> (snd x) 

increment :: Ref -> Ref
increment Empty = Q.singleton 0
increment (r :|> x) = r |> (x + 1) 
 
extend :: Ref -> Ref
extend r = r |> 0

chop :: Ref -> Ref
chop Empty = Empty 
chop (r :|> _) = r

oldHoppy :: Ref -> Ref
oldHoppy Empty = Q.singleton 0
oldHoppy r = (extend.increment.chop) r

extendTo :: Int -> Ref -> Ref 
extendTo x r
    | length r >= x = r 
    | otherwise = extendTo x $ extend r

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr findo (const Nothing) xs n
  where 
      findo :: a -> (Int -> Maybe a) -> Int -> Maybe a
      findo x r k = case k of
          0 -> Just x
          _ -> r (k-1)
