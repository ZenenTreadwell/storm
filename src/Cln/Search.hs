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
import Control.Monad.Trans.State.Lazy 
import qualified Data.Sequence as Q
import Data.Sequence(Seq(..),(<|),(|>),(><)) 
import Data.Foldable 

type Search = Reader (Gra, Node, Node)  -- from / to
type Way = Q.Seq Channel 
type Ref = Q.Seq Int
type Deref = (Ref, Way) 

results :: Int -> StateT (Ref, [Way]) Search [Way] 
results x = do 
    (r , c) <- get
    (xo, r') <- lift $ search r
    put (increment r', xo : c) 
    if x > length c 
        then results x 
        else return c

search :: Ref -> Search (Way, Ref) 
search r = (hydrate r) >>= \case
    (Left x) -> search $ nextr r x
    (Right y) -> (finally y) >>= \case  
        Nothing -> search $ increment r
        (Just y) -> pure (y, r)  

finally :: Way -> Search (Maybe Way) 
finally w = do 
    (g, n, v) <- ask 
    oo <- outgoing w 
    case filter ((== v).fst) $ oo of 
        [] -> pure Nothing 
        (x:_) -> pure $ Just (w |> snd x) 

nextr :: Ref -> Deref -> Ref 
nextr r (r', c) = 
    let 
        x = Q.length r
        y = Q.length r' 
        z = Q.length c 
    in if (x == z) 
        then extend.increment.chop $ r
        else if z == 0
            then extendTo (length r + 1) Empty
            else extendTo (length r) $ increment $ Q.take z r 

hydrate :: Ref -> Search (Either Deref Way)
hydrate r = evalStateT h (r, Empty) 
    
h :: StateT Deref Search (Either Deref Way)
h = get >>= \case 
    (Empty, c) -> return $ Right c
    dr@(y :<| t, c) -> do 
        (g, n, v) <- lift ask 
        oo <- lift $ outgoing c
        case oo !? y of 
            Nothing -> (pure.Left) dr 
            (Just (m, x)) -> pure $ Right (c |> x) 
 
outgoing :: Way -> Search [(Node, Channel)] 
outgoing Empty = do 
    (g, n, v) <- ask 
    pure $ lsuc g n
outgoing (c :|> d) = do 
    (g, n, v) <- ask
    pure $ lsuc g (toNode d) 

increment :: Ref -> Ref
increment Empty = Q.singleton 0
increment (r :|> x) = r |> (x + 1) 
extend :: Ref -> Ref
extend r = r |> 0
chop :: Ref -> Ref
chop Empty = Empty 
chop (r :|> _) = r

toNode :: Channel -> Node
toNode = getNodeInt.(destination :: Channel -> String) 

extendTo :: Int -> Ref -> Ref 
extendTo x r
    | length r >= x = r 
    | otherwise = extendTo x $ extend r

-- extra 1.7.12
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n

