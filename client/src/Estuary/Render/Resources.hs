module Estuary.Render.Resources where

import Control.Concurrent
import Data.Sequence
import Data.Text

import Estuary.Types.ResourceOp
import Estuary.Types.LocMap as LocMap
import Estuary.Types.Loadable
import Estuary.Types.AudioResource





data Resources = Resources {
  _resourceOps :: MVar (Seq ResourceOp),
  resourceLists :: LoadMap ResourceList,
  audioMap :: MVar (LocMap Text),
  audioResources :: LoadMap AudioResource
  }


newResources :: IO Resources
newResources = do
  _resourceOps' <- newMVar Seq.empty
  audioMap' <- newMVar LocMap.empty
  audioResources' <- newLoadMap
  return $ Resources {
    _resourceOps = _resourceOps',
    audioMap = audioMap',
    audioResources = audioResources'
  }


addResourceOp :: Resources -> ResourceOp -> IO ()
addResourceOp r op = do
  opsSeq <- takeMVar $ _resourceOps r
  ... *** working here ***

deleteResourceOp :: Resources -> Int -> IO ()
deleteResourceOp r x = do
  opsSeq <- takeMVar $ _resourceOps r
  ... ???
  putMVar (_resourceOps r) $ Seq.deleteAt x opsSeq

setResourceOps :: Resources -> Seq ResourceOp -> IO ()
setResourceOps r x = do
  ... ???


resourceOps :: Resources -> IO (Seq ResourceOp)
resourceOps r = readMVar $ _resourceOps r


-- accessAudioResource replaces previous 'access' from Estuary.Types.ResourceMap
-- access :: Loadable a => Location -> ResourceMap a -> IO (Either LoadStatus JSVal)

accessAudioResource :: Resources -> Location -> IO (Either LoadStatus AudioResource)
accessAudioResource r l = do
  am <- readMVar $ audioMap r
  let l' = adjustedLocation l am
  case LocationMap.lookup l' am of
    Nothing -> return (Left $ LoadError $ "no resource at location " <> T.pack (show l))
    Just url -> do
      x <- load (audioResources r) url
      ls <- loadStatus x
      case ls of
        LoadError e -> return $ Left $ LoadError e
        otherwise -> return $ Right x
        -- ie. if loadStatus returns Loading we still return the resource, because it might be ready soon

{-

*** TODO: adapt this to new representations - should be sampleMapToResourceList?

sampleMapToAudioMap :: SampleMap -> IO AudioMap
sampleMapToAudioMap sm = mapM audioResourceFromMeta c
  where
    a = fmap ((zip [0..]) . toList) $ unSampleMap sm -- :: Map Text [(Int,Text)]
    b = concat $ Map.elems $ Map.mapWithKey (\x ys -> fmap (\(y,url) -> ((x,y),url)) ys) a
    c = fmap (\url -> AudioMeta ("samples/" <> url) 0.0) $ Map.fromList b

-}
