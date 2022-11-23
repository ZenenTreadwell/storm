{-# LANGUAGE
    LambdaCase, 
    DuplicateRecordFields, 
    TypeSynonymInstances, 
    FlexibleInstances
#-} 
module Storm.Search where 
import Cln.Types
import Storm.Graph
import Data.Graph.Inductive.Graph
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy 
import qualified Data.Sequence as Q
import Data.Sequence(Seq(..),(<|),(|>),(><)) 
import Data.Foldable 

type Search = ReaderT (Gra, Node, Node) IO  -- from / to
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
    (Left x) -> do 
        search $ nextr r x
    (Right y) -> do
        (finally y) >>= \case  
            Nothing -> search $ increment r
            (Just y) -> lift $ pure (y, r)  

finally :: Way -> Search (Maybe Way) 
finally w = do 
    (g, n, v) <- ask 
    oo <- outgoing w 
    case filter ((== v).fst) $ oo of 
        [] -> pure Nothing 
        (x:_) -> pure $ Just (w |> snd x) 

nextr :: Ref -> Deref -> Ref 
nextr r (r', c)  
    | z == Q.length r = extend.increment.chop $ r
    | z == 0 = extendTo (Q.length r + 1) Empty
    | otherwise = extendTo (Q.length r) $ increment $ Q.take z r
    where z = Q.length c

hydrate :: Ref -> Search (Either Deref Way)
hydrate r = evalStateT h (r, Empty) 
    
h :: StateT Deref Search (Either Deref Way)
h = get >>= \case 
    (Empty, c) -> return $ Right c
    dr@(y :<| t, c) -> do 
        (g, n, v) <- lift ask 
        oo <- lift $ outgoing c
        case oo !? y of 
            Nothing     -> pure $ Left dr 
            Just (m, x) -> put (t, c |> x) >> h
 
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
extendTo :: Int -> Ref -> Ref 
extendTo x r
    | length r >= x = r 
    | otherwise = extendTo x $ extend r

--  
-- compile error ? Ord overlap err?
-- ghc I wanted it to overlap whats why I wrote it 
--instance Ord Ref where 
--    compare Empty Empty = EQ
--    compare r o = case compare (l r) (l o) of 
--        EQ -> deeper r o 
--        GT -> GT
--        LT -> LT 
--        where 
--            l = Q.length
--            --deeper :: Ref -> Ref -> Ordering  
--            deeper (a :<| r') (b :<| o') = case compare a b of
--                EQ -> compare r' o' 
--                LT -> LT 
--                GT -> GT 
        
toNode :: Channel -> Node
toNode = getNodeInt.(destination :: Channel -> String) 
-- extra 1.7.12
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n

