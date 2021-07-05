{-# LANGUAGE OverloadedStrings #-}

module Estuary.Render.Resources where

import Control.Concurrent
import Control.Monad
import Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.Types.ResourceOp
import Estuary.Types.ResourceMeta
import Estuary.Types.ResourceType
import Estuary.Types.Location
import Estuary.Types.LocMap as LocMap
import Estuary.Types.Loadable
import Estuary.Types.AudioResource
import Estuary.Render.ResourceList


data Resources = Resources {
  _resourceOps :: MVar (Seq ResourceOp),
  resourceLists :: LoadMap ResourceList,
  maps :: MVar (LocMap Text,LocMap Text,LocMap Text), -- audio image video
  audioResources :: LoadMap AudioResource
  }


newResources :: IO Resources
newResources = do
  _resourceOps' <- newMVar Seq.empty
  resourceLists' <- newLoadMap
  maps' <- newMVar (LocMap.empty,LocMap.empty,LocMap.empty)
  audioResources' <- newLoadMap
  return $ Resources {
    _resourceOps = _resourceOps',
    resourceLists = resourceLists',
    maps = maps',
    audioResources = audioResources'
  }


addResourceOp :: Resources -> ResourceOp -> IO ()
addResourceOp r op = do
  putStrLn $ "addResourceOp: " ++ show op
  opsSeq <- takeMVar $ _resourceOps r
  let newOpsSeq = opsSeq |> op
  updateMaps r newOpsSeq
  putMVar (_resourceOps r) newOpsSeq


deleteResourceOp :: Resources -> Int -> IO ()
deleteResourceOp r x = do
  putStrLn $ "deleteResourceOp: " ++ show x
  opsSeq <- takeMVar $ _resourceOps r
  let newOpsSeq = Seq.deleteAt x opsSeq
  updateMaps r newOpsSeq
  putMVar (_resourceOps r) newOpsSeq


clearResourceOps :: Resources -> IO ()
clearResourceOps r = do
  putStrLn "clearResourceOps"
  setResourceOps r Seq.empty


setResourceOps :: Resources -> Seq ResourceOp -> IO ()
setResourceOps r x = do
  _ <- takeMVar $ _resourceOps r
  updateMaps r x
  putMVar (_resourceOps r) x


updateMaps :: Resources -> Seq ResourceOp -> IO ()
updateMaps r opsSeq = do
  _ <- takeMVar $ maps r
  let emptyMaps = (LocMap.empty,LocMap.empty,LocMap.empty)
  newMaps <- foldM (resourceOpIO r) emptyMaps opsSeq
  putStrLn $ "updateMaps: " ++ show newMaps
  putMVar (maps r) newMaps


updateMapsCallback :: Resources -> IO ()
updateMapsCallback r = do
  opsSeq <- takeMVar $ _resourceOps r
  updateMaps r opsSeq
  putMVar (_resourceOps r) opsSeq


resourceOpIO :: Resources -> (LocMap Text,LocMap Text,LocMap Text) -> ResourceOp -> IO (LocMap Text,LocMap Text,LocMap Text)
resourceOpIO r maps (InsertResource t url loc) = return $ insertResource maps t url loc
resourceOpIO r maps (AppendResource t url bankName) = return $ appendResource maps t url bankName
resourceOpIO r maps (DeleteResource t loc) = return $ deleteResource maps t loc
resourceOpIO r maps (ResourceListURL url) = do
  resList <- load (resourceLists r) url (const $ updateMapsCallback r)
  rMetas <- resourceMetas resList
  return $ Prelude.foldl insertResourceMeta maps rMetas

insertResourceMeta :: (LocMap Text,LocMap Text,LocMap Text) -> ResourceMeta -> (LocMap Text,LocMap Text,LocMap Text)
insertResourceMeta (aMap,iMap,vMap) (ResourceMeta url Audio loc) = (LocMap.insert loc url aMap,iMap,vMap)
insertResourceMeta (aMap,iMap,vMap) (ResourceMeta url Image loc) = (aMap,LocMap.insert loc url iMap,vMap)
insertResourceMeta (aMap,iMap,vMap) (ResourceMeta url Video loc) = (aMap,iMap,LocMap.insert loc url vMap)

insertResource :: (LocMap Text,LocMap Text,LocMap Text) -> ResourceType -> Text -> Location -> (LocMap Text,LocMap Text,LocMap Text)
insertResource (aMap,iMap,vMap) Audio url loc = (LocMap.insert loc url aMap,iMap,vMap)
insertResource (aMap,iMap,vMap) Image url loc = (aMap,LocMap.insert loc url iMap,vMap)
insertResource (aMap,iMap,vMap) Video url loc = (aMap,iMap,LocMap.insert loc url vMap)

appendResource :: (LocMap Text,LocMap Text,LocMap Text) -> ResourceType -> Text -> Text -> (LocMap Text,LocMap Text,LocMap Text)
appendResource (aMap,iMap,vMap) Audio url bankName = (LocMap.append bankName url aMap,iMap,vMap)
appendResource (aMap,iMap,vMap) Image url bankName = (aMap,LocMap.append bankName url iMap,vMap)
appendResource (aMap,iMap,vMap) Video url bankName = (aMap,iMap,LocMap.append bankName url vMap)

deleteResource :: (LocMap Text,LocMap Text,LocMap Text) -> ResourceType -> Location -> (LocMap Text,LocMap Text,LocMap Text)
deleteResource (aMap,iMap,vMap) Audio loc = (LocMap.delete loc aMap,iMap,vMap)
deleteResource (aMap,iMap,vMap) Image loc = (aMap,LocMap.delete loc iMap,vMap)
deleteResource (aMap,iMap,vMap) Video loc = (aMap,iMap,LocMap.delete loc vMap)


accessAudioResource :: Resources -> Location -> IO (Either LoadStatus AudioResource)
accessAudioResource r loc = do
  (aMap,_,_) <- readMVar $ maps r
  case LocMap.lookup loc aMap of
    Just url -> do
      aRes <- load (audioResources r) url (const $ return ())
      lStatus <- loadStatus aRes
      case lStatus of
        Loaded -> return $ Right aRes
        otherwise -> return $ Left lStatus
    Nothing -> return (Left $ LoadError $ "no resource at location " <> T.pack (show loc))
