# storm 

##### Install
haskell stack project
- `stack init`
- `stack build`

Executable **storm** as core lightning plugin, see https://lightning.readthedocs.io/PLUGINS.html

Move or symlink file storm into .lightning/plugins/ 

##### Added Interface (i.e $ lightning-cli stormwallet)  
- stormwallet -- wallet summary
- stormload -- pre-requirment of following !! 8GB Memory
- stormnetwork -- network summary  
- stormpaths fromid toid [amountMsat] [resultNum] -- returns routes   
- stormrebalance !! max spend 89,000msat (likely fail to spend anything) 
  
(in progress)
- economic rebalancer 
- fee adjustments 
- hold invoices
- channel suggester
- accurate/specific types 
- tests?
- ...

##### Files: 
- app/Main - executable root; connects conduit to plugin
- src/Cln/Manifest - plugin configuration
- src/Cln/Plugin - storm interface, hook and notification actions
- src/Cln/Conduit - json parser
- src/Cln/Types - data types from plugin and rpc
- src/Cln/Client - lightning-rpc wrap functions
- src/Cln/Paths - pathfinding, route building
- src/Cln/Wallet - create funds summary
- src/Cln/Balance !! rebalancer

Client can be tested in gchi`:
- `\> :l Cln.Client`
- `\> connectCln "<rpc-file-path>"`
- `\> getinfo`


##### Audit Requests
- /src/Cln/Paths createRoute :: PathInfo :: [Route]
- /src/Cln/Balance (!! balance attempts failing) 

