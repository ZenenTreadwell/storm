######Haskell Stack Storm 
stack init 
stack build ## Observe executable location, add to CLN plugins

ln -s \<executable-location\>/storm  ./.lightning/plugins/ ## via link auto updates on recompile

New lightning-cli commands include: 
- stormload
- stormsize
- stormcandidates
- stormcircles
- ... check src/Manifest.hs 

Files Purposes: 

app/Main.hs          ## executable 
src/Jspec.hs         ## json conduit
src/Nodes.hs         ## map/graph of nodes
src/Cli.hs           ## rpc client
src/Lightningd.hs    ## json spec
src/Plugout.hs       ## plugin handler conduit
src/Manifest.hs      ## plugin configurationa


