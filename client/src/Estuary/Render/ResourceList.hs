{-# LANGUAGE OverloadedStrings #-}

module Estuary.Render.ResourceList where

-- a ResourceList is a list of resource URLs and metadata
-- acquired by asychronous download, with a record of the
-- URL from which it was retrieved (for resolution of relative URLs)

import Data.Text as T
import Data.Text.IO as T
import Data.IORef
import Data.Aeson
import GHCJS.Types
import GHCJS.Foreign.Callback
import GHCJS.Marshal (fromJSValUnchecked)

import Estuary.Types.Loadable
import Estuary.Types.ResourceMeta
import Estuary.Types.ResourceType
import Estuary.Render.XMLHttpRequest

data ResourceList = ResourceList {
  resourceListURL :: Text,
  resourceListLoadStatus :: IORef LoadStatus,
  resourceMetas :: IORef [ResourceMeta]
  }

instance FromJSON ResourceMeta where
  parseJSON (Object v) = do
    url <- v .: "url"
    t <- v .: "type"
    b <- v .: "bank"
    n <- v .: "n"
    return $ ResourceMeta {
      resourceURL = url,
      resourceType = typeTextToResourceType t,
      resourceLocation = (b,n)
    }

typeTextToResourceType :: Text -> ResourceType
typeTextToResourceType "video" = Video
typeTextToResourceType "image" = Image
typeTextToResourceType _ = Audio

instance Loadable ResourceList where

  newLoadable url successCb = do
    ls <- newIORef Loading
    rm <- newIORef []
    T.putStrLn $ "loading ResourceList " <> url
    rq <- jsonXMLHttpRequest url

    loadCb <- asyncCallback1 $ \j -> do
      if isNull j || isUndefined j then do
        let msg = "null/undefined ResourceList at " <> url
        writeIORef ls $ LoadError msg
        T.putStrLn $ "*ERROR* " <> msg
      else do
        v <- fromJSValUnchecked j
        case fromJSON (v :: Value) of
          Error e -> do
            let msg = "error decoding ResourceList at " <> url <> " : " <> T.pack e
            writeIORef ls $ LoadError msg
            T.putStrLn $ "*ERROR* " <> msg
          Success x -> do
            writeIORef ls Loaded
            writeIORef rm x 
            T.putStrLn $ "loaded ResourceList " <> url
            successCb
    onLoad rq loadCb

    cbError <- asyncCallback $ do
      let msg = "error loading " <> url
      writeIORef ls $ LoadError msg
      T.putStrLn msg
    onError rq cbError

    send rq

    return $ ResourceList {
      resourceListURL = url,
      resourceListLoadStatus = ls,
      resourceMetas = rm
    }

  loadStatus x = readIORef $ resourceListLoadStatus x
