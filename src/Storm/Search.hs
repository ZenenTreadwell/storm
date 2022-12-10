{-# LANGUAGE
    LambdaCase, 
    DuplicateRecordFields, 
    TypeSynonymInstances, 
    FlexibleInstances
#-} 
module Storm.Search where 
import System.IO 
import Cln.Types
import Storm.Types 
import Storm.Graph
import Data.Graph.Inductive.Graph
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy 
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import qualified Data.Sequence as Q
import Data.Sequence(Seq(..),(<|),(|>),(><)) 
import Data.Foldable 

type Search = ReaderT (Gra, Node, Node) IO  -- from / to
type Way = Q.Seq Channel 
type Deref = (Ref, Way) 


search :: Ref -> Search (Way, Ref)  
search r = (hydrate r) >>= \case
    (Left x) -> do 
        search $ nextr r x
    (Right y) -> do
        (finally (r, y) ) >>= \case  
            Nothing -> search $ increment r
            (Just z) -> lift $ pure z   

finally :: (Ref, Way) -> Search (Maybe (Way, Ref)) 
finally (r, w) = do 
    (g, n, v) <- ask 
    oo <- outgoing w 
    let {
        f = dropWhile (not.(== v).fst) oo;
        lo = length oo; 
        la = length f ;
        rr = lo - la 
        }
    case f of 
        [] -> pure Nothing 
        (x:_) -> pure $ Just ( w |> snd x, r  |> rr) 

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
        
toNode :: Channel -> Node
toNode = getNodeInt.(destination :: Channel -> String) 

-- prior art ~ extra 1.7.12
-- foldr with 3 arg, voodoo shit
(!?) :: (Foldable t, Eq b, Num b) => t a -> b -> Maybe a
(!?) l = foldr voo (const Nothing) l
voo :: (Eq b, Num b) => a -> (b -> Maybe a) -> b -> Maybe a
voo x r k -- !?!
  | k == 0 = Just x
  | otherwise = r $ k-1 
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- (const Nothing) :: b -> Maybe a
