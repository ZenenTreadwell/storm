######Haskell Stack Storm
stack init
stack build

  Lib  ##

  Cli.hs           ## unix socket lightning-rpc
  Jspec.hs         ## json conduit
  Nodes.hs         ## map of nodes
  Lightningd.hs    ## plugin data
  Plugin.hs        ## storm api
  Manifest.hs      ## cln init

  Exe ## storm

  Main.hs  ## executable plugin 

example : 
- stormload    ## creates a map of nodes/paths in memory  
- stormsize    ## inspect map summary 
- stormnode    ## inspect node  
- ... check Manifest  // Plugin wip


