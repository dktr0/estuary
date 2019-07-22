module Estuary.Types.LocalResources where

import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

import Data.Text

import GHCJS.DOM.Blob

import Estuary.Types.Resources

data LocalResourceServers = LocalResourceServers {
  localAudioResources :: LocalResourceServer AudioMeta,
  localVideoResources :: LocalResourceServer VideoMeta,
  localImageResources :: LocalResourceServer ImageMeta
}

emptyLocalResourceServers :: LocalResourceServers
emptyLocalResourceServers = LocalResourceServers {
  localAudioResources = emptyLocalResourceServer,
  localVideoResources = emptyLocalResourceServer,
  localImageResources = emptyLocalResourceServer
}

-- ^ An in browser resources source for user uploaded, private, resources. It is a map of group
-- name to files where the files is a map of resource name to it's data.
newtype LocalResourceServer m = LocalResourceServer { unLocalResourceServer :: Map Text (Map Text Blob) }

emptyLocalResourceServer :: LocalResourceServer m
emptyLocalResourceServer = LocalResourceServer Map.empty

uploadLocal :: Text -> Text -> Blob -> LocalResourceServer m -> LocalResourceServer m
uploadLocal groupName url file (LocalResourceServer server) = 
  let group = Map.insert url file (Map.findWithDefault Map.empty groupName server)
  in LocalResourceServer (Map.insert groupName group server)