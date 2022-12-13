# storm 

##### Install
haskell stack project
- `stack init`
- `stack build`

Following lightningd option required: 
allow-deprecated-apis=false

Executable **storm** is [cln plugin](https://lightning.readthedocs.io/PLUGINS.html)
Storm Rpc Interface (i.e $ lightning-cli stormwallet)  
- stormwallet
- stormload -- pre-requirment of following !! 8GB Memory
- stormnetwork -- network summary  
- stormpaths fromid toid [amountMsat] [resultNum]   
- (disabled) stormrebalance  
  
Create core lightning pluggable executable, see app/Main for full example

main :: IO ()  

main = plugin manifest data app




  

