# storm 

##### Install
haskell stack project
`stack init`
`stack build`

Executable **storm** as core lightning plugin 
https://lightning.readthedocs.io/PLUGINS.html

##### Added Interface
(i.e $ lightning-cli stormwallet)  
- stormwallet 
- stormload -- pre-requirment of following !! 8GB Memory
- stormnetwork -- storm prunes unrouteable/unreachable nodes and fees over threashold  
- stormpaths fromid toid -- !! for large nodes can take time : finds shortest path on every out-channel/in-channel 
- stormrebalance !! max spend 89,000msat (likely fail to spend anything) 
  
(in progress)
- economic rebalancer 
- competitive fee adjustments 
- hold invoice interface

##### Files 
app/Main - executable root; connects conduit to plugin
src/Cln/Manifest - plugin configuration 
src/Cln/Plugin - handle storm interface, hooks, and notifications
src/Cln/Conduit - parser for plugin and rpc 
src/Cln/Types - core lightning data
src/Cln/Client - calls to lightning-rpc file
try in `stack gchi`:
    \> :l Cln.Client
    \> connectCln "<rpc-file-path>" 
    \> getinfo
Balance !! work in progress





##### Audit Requests
/src/Cln/Paths 
createRoute :: PathInfo :: [Route]
    
/src/Cln/Rebalance 
payRoute :: [Route] -> IO String
     - have only tried on circular routes; see fails from my node so probably broken

{
"id": "0337694505123a12a8fadd95523dcc235898ad3b80a06e4a63ca26fed68dd0d17c",
"network":"bitcoin"
}

       


