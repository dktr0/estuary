{-# LANGUAGE OverloadedStrings #-}

module Estuary.Resources.ResourceList where

-- a ResourceList is a list of resource URLs and metadata
-- acquired by asychronous download, with a record of the
-- URL from which it was retrieved (for resolution of relative URLs)

{- an example of a resource list JSON file (note that the property names MUST be in quotation marks):
[
{ "url": "samples/bd/BT0A0A7.wav", "type": "audio", "bank": "bd", "n": 0},
{ "url": "samples/cp/HANDCLP0.wav", "type": "audio", "bank": "cp", "n": 0},
{ "url": "samples/cp/HANDCLPA.wav", "type": "audio", "bank": "cp", "n": 1},
{ "url": "flower.MOV", "type": "video", "bank": "flower", "n": 0}
]
-}

import Data.Text as T
import Data.Text.IO as T
import Data.IORef
import Data.Aeson
import GHCJS.Types
import GHCJS.Foreign.Callback
import GHCJS.Marshal (fromJSValUnchecked)

import Estuary.Types.ResourceMeta
import Estuary.Types.ResourceType
import Estuary.Resources.XMLHttpRequest
import Estuary.Resources.Loadable

data ResourceList = ResourceList {
  resourceListURL :: Text,
  resourceListLoadStatus :: IORef LoadStatus,
  resourceMetas_ :: IORef [ResourceMeta]
  }

resourceMetas :: ResourceList -> IO [ResourceMeta]
resourceMetas x = do
  ls <- readIORef $ resourceListLoadStatus x
  case ls of
    Loaded -> readIORef $ resourceMetas_ x
    otherwise -> return []

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

showResourceList :: ResourceList -> IO String
showResourceList x = do
  y <- readIORef $ resourceMetas_ x
  return $ (T.unpack $ resourceListURL x) ++ " " ++ show y

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

    let rList = ResourceList {
      resourceListURL = url,
      resourceListLoadStatus = ls,
      resourceMetas_ = rm
    }

    onLoad rq $ \j -> do
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
          Success xs -> do
            writeIORef rm $ adjustResourceURLs url xs
            writeIORef ls Loaded
            T.putStrLn $ "loaded ResourceList " <> url
            successCb rList

    onError rq $ do
      let msg = "error loading " <> url
      writeIORef ls $ LoadError msg
      T.putStrLn msg

    send rq

    return rList

  loadStatus x = readIORef $ resourceListLoadStatus x


-- adjustResourceURLs is used (above) to adjust the URL field in the record for each resource
-- so that it includes everything in the "parent" resource lists URL up to but not including
-- the filename of that resource list. In this way, the URL fields in resource list JSON files
-- can be (and must be) relative to the location of the resource list JSON file itself.

adjustResourceURLs :: Text -> [ResourceMeta] -> [ResourceMeta]
adjustResourceURLs baseURL = fmap f
  where
    baseURL' = T.dropWhileEnd (/= '/') baseURL
    f (ResourceMeta url t loc) = ResourceMeta (baseURL' <> url) t loc
