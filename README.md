######Haskell Stack Storm
`stack init`
`stack build`

Executable **storm** cln plugin. See
- src/Manifest.hs - config
https://lightning.readthedocs.io/PLUGINS.html

- src/Cli.hs 
It works in ghci: 
    `\> connectCln "<rpc-file-path>" `
    `\> getinfo`
- src/Graph.hs - graph object using fgl
- src/Paths.hs 

command line example : 
- $ lightning-cli stormwallet 
- $ lightning-cli stormload -- prerec to below, loads several GB into memory
- $ lightning-cli stormpaths [node] [node2]
- $ lightning-cli stormcomponants
- $ lightning-cli stormsize

Working on:
- channel candidates
- balancer 
- better paths
- feeadjuster
- hold invoices
- microinteractions 
- paths explorer 
- 

{
"id": "0337694505123a12a8fadd95523dcc235898ad3b80a06e4a63ca26fed68dd0d17c",
"alias": "Dagnela 🦄 Hunter",
"network":"bitcoin"
}

       


