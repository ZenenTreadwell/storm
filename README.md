# storm 

##### Install
haskell stack project
- `stack init`
- `stack build`

Executable **storm** as core lightning plugin 
- https://lightning.readthedocs.io/PLUGINS.html

##### Added Interface
(i.e $ lightning-cli stormwallet)  
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

##### Files 
app/Main - executable root; connects conduit to plugin
- src/Cln/Manifest - plugin configuration
- src/Cln/Plugin - storm interface, hook and notification actions
- src/Cln/Conduit - json parser
- src/Cln/Types - data types from plugin and rpc
- src/Cln/Client - lightning-rpc wrap functions
Client can be tested in gchi`:
- `\> :l Cln.Client`
- `\> connectCln "<rpc-file-path>"`
- `\> getinfo`
src/Cln/Paths - pathfinding, route building
Wallet - create funds summary
Balance !! work in progress

##### Audit Requests
- /src/Cln/Paths createRoute :: PathInfo :: [Route]
- /src/Cln/Balance (!! balance attempts failing) 

{
"alias": "Dagnela 🦄 Hunter",
"id": "0337694505123a12a8fadd95523dcc235898ad3b80a06e4a63ca26fed68dd0d17c",
"network":"bitcoin"
}
