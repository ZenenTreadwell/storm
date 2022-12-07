

module Storm.Types where 

import Data.Ratio
import qualified Data.Sequence as Q
type Ref = Q.Seq Int
type Acc = (Ratio Int, Int) 
-- cause circular ref err
