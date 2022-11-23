{-# LANGUAGE 
    DuplicateRecordFields, 
    LambdaCase, 
    DeriveGeneric, 
    OverloadedStrings
#-}
module Storm.Paths where 

import Cln.Types 
import GHC.Generics
import Data.Aeson

data PathInfo = P {
      cost :: Fee
    , neck :: Sat
    } deriving (Generic, Show)
instance ToJSON PathInfo 
instance Eq PathInfo where 
    (==) a b = (base.cost $ a) == (base.cost $ b) && (ppm.cost $ a) == (ppm.cost $ b)
instance Ord PathInfo where 
    compare a b = compare (cost a) (cost b) 

data Fee = Fee {
      base :: Int 
    , ppm :: Int  
    } deriving (Show, Generic, Eq)  
instance Ord Fee where 
    compare a b = compare (base a + ppm a) (base b + ppm b)
instance ToJSON Fee

info :: [Channel] -> PathInfo
info = foldr pf (P (Fee 0 0) maxBound)  
    where 
        pf :: Channel -> PathInfo -> PathInfo 
        pf e c = P 
            (Fee ( (base.cost) c + base_fee_millisatoshi e )
                 (((ppm.cost) c) + fee_per_millionth e ) 
            )
            (case htlc_maximum_msat e of 
                (Just x) -> minimum [(neck c), ((amount_msat::Channel->Msat) e), x]
                otherwise -> min (neck c) ((amount_msat::Channel->Msat) e) 
            )
