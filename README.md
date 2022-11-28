# storm 

##### Install
haskell stack project
- `stack init`
- `stack build`

Executable **storm** is [cln plugin](https://lightning.readthedocs.io/PLUGINS.html)
Storm Rpc Interface (i.e $ lightning-cli stormwallet)  
- stormwallet
- stormload -- pre-requirment of following !! 8GB Memory
- stormnetwork -- network summary  
- stormpaths fromid toid [amountMsat] [resultNum]   
- (disabled) stormrebalance  
  
Cln Interface : 

main :: IO () 
main = plugin manifest state app


  

