{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Estuary.Render.LocalResources where

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

import Data.Maybe

import Data.Text as T

import GHCJS.DOM.Blob

import Estuary.Render.ResourceProvider

import Estuary.Types.Resources
import Estuary.Types.Scope

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

instance ResourceDataProvider LocalResourceServer where
  resourceIsProvidedBy (LocalResourceServer fs) Resource{resourceScope=Private, resourceGroup, resourceFileName} = 
    isJust $ do
        group <- Map.lookup resourceGroup fs
        Map.lookup resourceFileName group
  resourceIsProvidedBy _ _ = False

  fetchResource server@(LocalResourceServer fs) resource@Resource{resourceGroup, resourceFileName} =
    if resourceIsProvidedBy server resource then 
      return $ Right $ (fs ! resourceGroup) ! resourceFileName
    else
      return $ Left $ "No resource in " `T.append` resourceGroup `T.append` " named " `T.append` resourceFileName