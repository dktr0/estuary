{-# LANGUAGE OverloadedStrings #-}

module Estuary.Resources where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef

import Estuary.Types.ResourceOp
import Estuary.Types.ResourceMeta
import Estuary.Types.ResourceType
import Estuary.Types.Location
import Estuary.Resources.LocMap as LocMap
import Estuary.Resources.Loadable
import Estuary.Resources.AudioResource
import Estuary.Resources.ResourceList
import Estuary.Languages.ExoLang

data ResourceMaps = ResourceMaps {
  audioMap :: LocMap Text,
  imageMap :: LocMap Text,
  videoMap :: LocMap Text,
  exoLangMap :: LocMap Text
  }

emptyResourceMaps :: ResourceMaps
emptyResourceMaps = ResourceMaps {
  audioMap = LocMap.empty,
  imageMap = LocMap.empty,
  videoMap = LocMap.empty,
  exoLangMap = LocMap.empty
  }

data Resources = Resources {
  _resourceOps :: MVar (Seq ResourceOp),
  resourceLists :: LoadMap ResourceList,
  maps :: MVar ResourceMaps,
  audioResources :: LoadMap AudioResource,
  exoLangResources :: LoadMap ExoLang,
  _updatedCallback :: IORef (Maybe (ResourceMaps -> IO ()))
  }


setResourcesUpdatedCallback :: MonadIO m => Resources -> (ResourceMaps -> IO ()) -> m ()
setResourcesUpdatedCallback r cb = liftIO $ writeIORef (_updatedCallback r) $ Just cb

updatedCallback :: MonadIO m => Resources -> m ()
updatedCallback r = liftIO $ do
  x <- readIORef $ _updatedCallback r
  case x of
    Just cb -> (readMVar $ maps r) >>= cb
    Nothing -> return ()


newResources :: MonadIO m => m Resources
newResources = liftIO $ do
  _resourceOps' <- newMVar defaultResourceOps
  resourceLists' <- newLoadMap
  maps' <- newMVar emptyResourceMaps
  audioResources' <- newLoadMap
  exoLangResources' <- newLoadMap
  _updatedCallback' <- newIORef Nothing
  return $ Resources {
    _resourceOps = _resourceOps',
    resourceLists = resourceLists',
    maps = maps',
    audioResources = audioResources',
    exoLangResources = exoLangResources',
    _updatedCallback = _updatedCallback'
  }


addResourceOp :: MonadIO m => Resources -> ResourceOp -> m ()
addResourceOp r op = liftIO $ do
  opsSeq <- takeMVar $ _resourceOps r
  let newOpsSeq = opsSeq |> op
  updateMaps r newOpsSeq
  putMVar (_resourceOps r) newOpsSeq


deleteResourceOp :: MonadIO m => Resources -> Int -> m ()
deleteResourceOp r x = liftIO $ do
  opsSeq <- takeMVar $ _resourceOps r
  let newOpsSeq = Seq.deleteAt x opsSeq
  updateMaps r newOpsSeq
  putMVar (_resourceOps r) newOpsSeq


clearResourceOps :: MonadIO m => Resources -> m ()
clearResourceOps r = liftIO $ setResourceOps r Seq.empty

setDefaultResourceOps :: MonadIO m => Resources -> m ()
setDefaultResourceOps r = liftIO $ setResourceOps r defaultResourceOps

setResourceOps :: MonadIO m => Resources -> Seq ResourceOp -> m ()
setResourceOps r x = liftIO $ do
  _ <- takeMVar $ _resourceOps r
  updateMaps r x
  putMVar (_resourceOps r) x


updateMaps :: MonadIO m => Resources -> Seq ResourceOp -> m ()
updateMaps r opsSeq = liftIO $ do
  _ <- takeMVar $ maps r
  newMaps <- foldM (resourceOpIO r) emptyResourceMaps opsSeq
  -- putStrLn $ "updateMaps: " ++ show newMaps
  putMVar (maps r) newMaps
  updatedCallback r


updateMapsCallback :: Resources -> IO ()
updateMapsCallback r = do
  opsSeq <- takeMVar $ _resourceOps r
  updateMaps r opsSeq
  putMVar (_resourceOps r) opsSeq


resourceOpIO :: MonadIO m => Resources -> ResourceMaps -> ResourceOp -> m ResourceMaps
resourceOpIO r maps (InsertResource t url loc) = return $ insertResource maps t url loc
resourceOpIO r maps (AppendResource t url bankName) = return $ appendResource maps t url bankName
resourceOpIO r maps (DeleteResource t loc) = return $ deleteResource maps t loc
resourceOpIO r maps (ResourceListURL url) = liftIO $ do
  resList <- load (resourceLists r) url (const $ updateMapsCallback r)
  rMetas <- resourceMetas resList
  return $ Prelude.foldl insertResourceMeta maps rMetas

insertResourceMeta :: ResourceMaps -> ResourceMeta -> ResourceMaps
insertResourceMeta maps (ResourceMeta url Audio loc _) = insertResource maps Audio url loc
insertResourceMeta maps (ResourceMeta url Image loc _) = insertResource maps Image url loc
insertResourceMeta maps (ResourceMeta url Video loc _) = insertResource maps Video url loc
insertResourceMeta maps (ResourceMeta url ExoLang loc _) = insertResource maps ExoLang url loc

insertResource :: ResourceMaps -> ResourceType -> Text -> Location -> ResourceMaps
insertResource maps Audio url loc = maps { audioMap = LocMap.insert loc url (audioMap maps) }
insertResource maps Image url loc = maps { imageMap = LocMap.insert loc url (imageMap maps) }
insertResource maps Video url loc = maps { videoMap = LocMap.insert loc url (videoMap maps) }
insertResource maps ExoLang url loc = maps { exoLangMap = LocMap.insert (fst loc,0) url (exoLangMap maps) }

appendResource :: ResourceMaps -> ResourceType -> Text -> Text -> ResourceMaps
appendResource maps Audio url bankName = maps { audioMap = LocMap.append bankName url (audioMap maps) }
appendResource maps Image url bankName = maps { imageMap = LocMap.append bankName url (imageMap maps) }
appendResource maps Video url bankName = maps { videoMap = LocMap.append bankName url (videoMap maps) }
appendResource maps ExoLang url bankName = insertResource maps ExoLang url (bankName,0) -- exoLang append/insertion is always overwrite of bankname:0

deleteResource :: ResourceMaps -> ResourceType -> Location -> ResourceMaps
deleteResource maps Audio loc = maps { audioMap = LocMap.delete loc (audioMap maps) }
deleteResource maps Image loc = maps { imageMap = LocMap.delete loc (imageMap maps) }
deleteResource maps Video loc = maps { videoMap = LocMap.delete loc (videoMap maps) }
deleteResource maps ExoLang loc = maps { exoLangMap = LocMap.delete loc (exoLangMap maps) }


accessAudioResource :: MonadIO m => Resources -> Location -> m (Either LoadStatus AudioResource)
accessAudioResource r loc = liftIO $ do
  aMap <- audioMap <$> readMVar (maps r)
  case LocMap.lookup loc aMap of
    Just url -> do
      aRes <- load (audioResources r) url (const $ return ())
      lStatus <- loadStatus aRes
      case lStatus of
        Loaded -> return $ Right aRes
        otherwise -> return $ Left lStatus
    Nothing -> return (Left $ LoadError $ "no resource at location " <> T.pack (show loc))
    
    
findExoLangURL :: MonadIO m => Resources -> Text -> m (Maybe Text)
findExoLangURL r name = liftIO $ do
  eMap <- exoLangMap <$> readMVar (maps r)
  pure $ LocMap.lookup (name,0) eMap

