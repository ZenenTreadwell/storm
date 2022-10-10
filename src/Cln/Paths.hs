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
import Control.Monad.Trans.State.Lazy
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
loguy mmm = liftIO $ System.IO.appendFile "/home/o/.ao/loguy" $ mmm <> "\n"

data PathInfo = P {
      hops :: Int
    , cost :: Fee
    , neck :: Sat
    , path :: [Channel]
    } deriving (Generic, Show)
instance ToJSON PathInfo where
   toJSON p = object [
          "hops" .= hops p
        , "cost" .= cost p
        , "neck" .= neck p
        , "route" .= createRoute 1000000000 p
        , "channels" .= path p
        ]

createRoute :: Msat -> PathInfo -> [Route] 
createRoute a c | a > neck c = []  
createRoute a c = foldr (addHop a) [] $ pairUp $ path c 
    where 
        pairUp :: [Channel] -> [(Channel,Channel)] 
        pairUp [] = []
        pairUp (a:[]) = [(a,a)]
        pairUp (a:b) = (a,head b) : pairUp b

        addHop :: Msat -> (Channel, Channel) -> [Route] -> [Route] 
        addHop a (cp, c)  r = Route 
            ((destination::Channel->String) cp) 
            ((short_channel_id::Channel->String) cp)
            (getDirect (source cp) ((destination::Channel->String) cp))
            (getAmount a c r)
            (getDelay c r) 
            "tlv" 
            : r 

        getDirect :: String -> String -> Int
        getDirect a b = if readHex a < readHex b then 0 else 1

        getDelay :: Channel -> [Route] -> Int
        getDelay e [] = 9
        getDelay e (r:_) = (delay::Route->Int) r + (delay::Channel->Int) e         
        
        getAmount :: Msat -> Channel -> [Route] -> Msat
        getAmount a e [] = a
        getAmount a e r = 
            let  
                mil = 1000000 :: Integer
                basefee = base_fee_millisatoshi e
                ppmrate = fee_per_millionth e
                num = (mil * toInteger nextAmount * toInteger ppmrate) 
                denum = mil*mil
                ppmfee  = fromInteger $ div num denum
                nextAmount = maximum $ map (amount_msat::Route->Msat) r
            in sum [ nextAmount , basefee , ppmfee ] 

findPaths :: Node -> Node -> IO [PathInfo]
findPaths n1 n2 = do
    gra <- readIORef graphRef
    d <-  evalStateT (look n2 ( match n1 gra ) ) (Q.empty, [])
    pure $ map (summarizePath . map snd . reverse . unLPath) d

type Sure = (Q.Seq (LPath Channel), [LPath Channel])

look :: Node 
     -> Dcp -- :: ( Maybe Cxt , Gra ) 
     -> StateT Sure IO [LPath Channel] 
look v (Nothing, g) = do 
    loguy "look pattern match fail "
    stop
look v (Just (ii,nn,nl,oo), g) = get >>= \case 
    (Empty, []) -> do -- home node
        loguy "starting search"

        put $ (foldr (append (LP []) ) Q.empty oo, [])  

        next v
    (Empty, q) -> do
        loguy "Q empty? "
        stop 
    ( p@(LP l) :<| ax , fin ) ->  
        if length fin > 100 then stop 
        else if nn == v then do 
            loguy $ "matchering " <> show p 
            put $ (ax, p : fin) 
            next v 
        else do 
            loguy $ "extendering " <> show (length l, length fin, Q.length ax) 
            put $ (foldr (append p) ax oo , fin) 
            next v  
              
next :: Node -> StateT Sure IO [LPath Channel]
next v = get >>= \case 
    ( (LP ((n,c):ax)) :<| qx , fin ) -> do 
        g <- liftIO $ readIORef graphRef
        loguy $ "moving on ? " <> show (n, order g, length fin, length ax, Q.length qx) 
        loguy.show $ gelem n g 
        look v (match n g)
    otherwise -> do 
        loguy "next pattern match fail" 
        stop

stop :: StateT Sure IO [LPath Channel]
stop = get >>= \case 
    (_, fin) -> pure fin 

append :: LPath Channel -> (Channel, Node) -> Q.Seq (LPath Channel) -> Q.Seq (LPath Channel)
append (LP p) x q = q |> (LP $ (swap x) : p) 

extendP :: LPath Channel -> LNode Channel -> LPath Channel
extendP p m = LP $ m : (unLPath p) 
    
dstn = (destination::Channel->String)

summarizePath :: [Channel] -> PathInfo
summarizePath s@(_:sx) = foldr c2 (initP s) sx

repackPath :: (Channel, LPath Channel, Channel) -> [Channel]
repackPath (f,p,e) = [f] <> ((lp.unLPath) p) <> [e]
lp :: [LNode Channel] -> [Channel]
lp [] = [] 
lp (p:px) = map snd px

initP :: [Channel] -> PathInfo
initP p = P (1) (Fee 0 0) maxBound p

bftFindPaths :: Node -> Node -> IO [PathInfo]
bftFindPaths n v = do 
    gra <- readIORef graphRef
    case ( match n gra                ,  match v gra) of 
        ( (Just (_, _, _, outy), g) , (Just (inny, _,_,_) , _ ) ) -> 
            bftFP g oo ii
            where oo = filter (\(f, n') -> v /= n') outy  
                  ii = filter (\(f, n') -> n /= n') inny
        otherwise -> pure []    

bftFP :: Gra -> Adj Channel -> Adj Channel -> IO [PathInfo]
bftFP g outy inny = pure
    $ sort
    $ map summarizePath   
    $ map repackPath
    $ filter (\(_,p,_)-> (length.unLPath) p > 0)
    $ map (\(a,pf,b) -> (a,pf g, b) )
    $ concat
    $ map (finPaths inny) 
    $ map outTree outy

finPaths :: Adj Channel -> (Channel, Gra -> LRTree Channel) -> [ (Channel, Gra -> LPath Channel, Channel) ] 
finPaths i outT = map (fp outT) i
fp :: (Channel, Gra -> LRTree Channel) -> (Channel,Node) -> (Channel, Gra -> LPath Channel, Channel)
fp (f, t) (e, v) = (f, (getLPath v).t , e) 

outTree :: (Channel, Node) -> (Channel, Gra -> LRTree Channel) 
outTree o = (fst o, lbft $ snd o)   

c2 :: Channel -> PathInfo -> PathInfo 
c2 e c = P
    (hops c + 1)
    (Fee ( (base.cost) c + base_fee_millisatoshi e )
         ( ((hops c)*((ppm.cost) c) + fee_per_millionth e ) `div` (hops c + 1) ))
    ( case htlc_maximum_msat e of 
        (Just x) -> minimum [(neck c), ((amount_msat::Channel->Msat) e), x]
        otherwise -> min (neck c) ((amount_msat::Channel->Msat) e) 
    )
    (path c)

getCircles :: Node -> IO [PathInfo]
getCircles n = do
    g <- readIORef graphRef
    case match n g of 
        (Just (inny, n',z, outy), g') -> bftFP g' outy inny
        otherwise -> pure [] 

instance Ord Fee where 
    compare a b = compare (base a + ppm a) (base b + ppm b)
instance Eq PathInfo where 
    (==) a b = (base.cost $ a) == (base.cost $ b) && (ppm.cost $ a) == (ppm.cost $ b)
instance Ord PathInfo where 
    compare a b = compare (cost a) (cost b) 
