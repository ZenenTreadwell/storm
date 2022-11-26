module Main where

import Cln.Plugin (plugin)
import Storm.Plug  

main :: IO ()
main = plugin manifest eye storm
