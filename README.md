# storm 

## Install
haskell stack project
- `stack init`
- `stack build`

Following lightningd option required: 
allow-deprecated-apis=false


Build should create a **storm** executable that can be installed as a [plugin](https://lightning.readthedocs.io/PLUGINS.html) 
that adds the following rpc interface to lightning-cli: 
- stormwallet 
- stormload
- stormnetwork   
- stormpaths fromid toid [amountMsat] [resultNum=1]  
- stormdeploy 
- (disabled) stormrebalance  
  
## Custom Plugins
Create core lightning plugin by defining a manifest (:: Value), your app data, and your app. The manifest describes the events app needs to handle. Note that notifications will not have an Id and do not require a response, but hooks do. Custom rpc methods are hooks and passthrough the yield back to the terminal.

```
import Cln.Plugin 
import Cln.Client
import Cln.Types 

app :: (Maybe Id, Method, Params) -> ConduitT (Either (Res Value) PReq) (Res Value) (ReaderT Handle (StateT a IO) ) () 
-- notification    
app event@(Nothing, "channel_opened", v) = do
    -- reader contains active lightning-rpc Handle
    rpc <- lift ask
    -- user defined state carried between requests
    sharedState <- lift.lift $ get 
    -- access to io
    liftIO $ getinfo rpc    

-- hook 
app event@(Just i, "firethemissiles", v) = do
    -- if you don't know what you are doing immeadiatly return to allow default behavior: 
    yield $ Res (object ["result" .= ("continue" :: Text)]) i


main :: IO
main = plugin manifest data app
```      


