module Main where

import Cln.Plugin (plugin)
import Storm.Plug (storm) 

main :: IO ()
main = plugin storm
