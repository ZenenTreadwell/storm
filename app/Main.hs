module Main where
import Jspec
import Plugin
import Control.Monad (forever)
import System.IO
import Data.Conduit.Combinators (sourceHandle, sinkHandle)
import Data.Conduit 
main :: IO ()
main = do 
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    forever $ runConduit $ sourceHandle stdin 
           .| jinjin 
           .| plug 
           .| sinkHandle stdout
