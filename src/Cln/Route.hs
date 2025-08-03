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
        pairUp (a:b:c) = (a,b) : pairUp c 

        addHop :: Msat -> (Channel, Channel) -> [Route] -> [Route]
        addHop a (cp, c)  r = Route
            (cp.destination)
            (cp.short_channel_id)
            (getDirect (cp.source) (cp.destination))
            (getAmount a c r)
            (getDelay c r)
            "tlv"
            : r

        getDirect :: String -> String -> Int
        getDirect a b = if readHex a < readHex b then 0 else 1

        getDelay :: Channel -> [Route] -> Int
        getDelay e [] = 9
        getDelay e (r:_) = r.delay + e.delay
        
        getAmount :: Msat -> Channel -> [Route] -> Msat
        getAmount a e [] = a
        getAmount a e r =
            let mil = 1000000 :: Integer
                basefee = base_fee_millisatoshi e
                ppmrate = fee_per_millionth e
                num = (mil * toInteger nextAmount * toInteger ppmrate)
                denum = mil*mil
                ppmfee  = fromInteger $ div num denum
                nextAmount = maximum $ map (.amount_msat) r
            in sum [ nextAmount , basefee , ppmfee ]


