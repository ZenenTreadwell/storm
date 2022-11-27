module Main where
import Cln.Plugin
import Storm.Plug
main :: IO ()
main = plugin manifest eye storm 
--    manifest ~~ Value -- 
--    eye ~~ starting state 
--    storm ~~ function called when decode successful 
    -- monads:
    -- - readerT Handle - cln rpc (client uses) -- ask
    -- - stateT a - app state, any type -- get/put
    -- - conduitT - connected to stdin /out jsonrpc -- yield   
