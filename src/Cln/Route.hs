{-# LANGUAGE 
    DuplicateRecordFields
#-} 

module Cln.Route where 

import Cln.Types
import Numeric (readHex) 

createRoute :: Msat -> [Channel] -> [Route]
createRoute a c = foldr (addHop a) [] $ pairUp c
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
            let mil = 1000000 :: Integer
                basefee = base_fee_millisatoshi e
                ppmrate = fee_per_millionth e
                num = (mil * toInteger nextAmount * toInteger ppmrate)
                denum = mil*mil
                ppmfee  = fromInteger $ div num denum
                nextAmount = maximum $ map (amount_msat::Route->Msat) r
            in sum [ nextAmount , basefee , ppmfee ]


