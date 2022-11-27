module Main where
import Cln.Plugin
import Storm.Plug
main :: IO ()
main = plugin manifest eye storm 
