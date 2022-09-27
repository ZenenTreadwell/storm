module Main where
import Cln.Plugin (plug)
import Cln.Conduit (inConduit)
import Control.Monad (forever)
import System.IO
import Data.Conduit.Combinators (sourceHandle, sinkHandle)
import Data.Conduit
main :: IO ()
main = do
    mapM (flip hSetBuffering NoBuffering) [stdin,stdout] 
    forever $ runConduit 
            $ sourceHandle stdin
           .| inConduit
           .| plug
           .| sinkHandle stdout
