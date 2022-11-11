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



type Search = Reader (Gra, Node, Node)  -- from / to
type Way = [Channel] 
type Ref = Q.Seq Int
type DeRef = (Cxt, Ref, Way)





results :: Int -> StateT (Ref, [Way]) Search [Way] 
results x =   
    do 
        (r , c) <- get
        xo <- lift $ search r
        put (increment r, xo : c) 
        if x > length c 
            then results x 
            else return c

search :: Ref -> Search Way
search ref = (fetchRef ref) >>= \case
    (Left x) -> search $ oldHoppy ref
    (Right y) -> (checkFound y) >>= \case  
        Nothing -> search $ increment ref
        (Just y) -> pure y 

nextRef :: Ref -> DeRef -> Ref 
nextRef r ((ii,nn,ll,oo), r', c) = extend r -- !!!

fetchRef :: Ref -> Search (Either DeRef Way) 
fetchRef ref = do
    (g,n,v) <- ask  
    evalStateT getChans (context g n, ref, []) 

getChans :: StateT DeRef Search (Either DeRef Way)
getChans = get >>= \case 
    (_, Empty, c) -> do 
        pure (Right c) 
    ((ii,nn,ll,oo), y :<| t, c) -> do 
        (g, n, v) <- lift ask
        case oo !? y of 
            Nothing -> do 
                x <- get
                pure $ Left x    
            Just (c', n') -> do 
                put ( context g n' , t , c' : c )
                getChans

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr findo (const Nothing) xs n
  where 
      findo :: a -> (Int -> Maybe a) -> Int -> Maybe a
      findo x r k = case k of
          0 -> Just x
          _ -> r (k-1)

checkFound :: [Channel] -> Search (Maybe Way) 
checkFound [] = do  
    (g,n,v) <- ask
    final [] n    
checkFound a = 
    let v' = getNodeInt.(destination :: Channel -> String).last $ a 
    in final a v'  

final :: [Channel] -> Node -> Search (Maybe Way) 
final a v' = do  
    (g,n,v) <- ask
    case (filter ((== v').fst) $ lpre g v) of 
        [] -> pure Nothing 
        (x:_) -> pure $ Just $ a <> [snd x]

findPathso :: Node -> Node -> IO [Way]
findPathso = undefined 

increment :: Ref -> Ref
increment Empty = Q.singleton 0 
increment (r :|> x) = r |> (x + 1) 
 
extend :: Ref -> Ref
extend r = r |> 0

chop :: Ref -> Ref
chop Empty = Empty 
chop (r :|> _) = r

oldHoppy :: Ref -> Ref
oldHoppy = extend.increment.chop
