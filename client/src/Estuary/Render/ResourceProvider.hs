{-# LANGUAGE NamedFieldPuns, OverloadedStrings, ScopedTypeVariables #-}
module Estuary.Render.ResourceProvider where

import Control.Monad.IO.Class

import Control.Monad.Trans.State.Strict

import Data.Text as T

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Sequence (Seq, (><), (|>))
import qualified Data.Sequence as Seq

import Estuary.Types.Resources

import GHCJS.DOM.Blob

-- not a node, but what ever you make a node with.
--class (ResourceDataProvider a) => ResourceProvider a where
  -- ^ prefetchResource is a mechanism for hinting to the provider that a resource will be needed soon and
  -- should be fetched and cached so that a future call to `getResource` is more likely to succeed.  
  -- prefetchResource :: a -> Resource m -> IO ()

class ResourceDataProvider a where
  resourceIsProvidedBy :: a m -> Resource m -> Bool
  
  -- ^ prefetchResource is a mechanism for hinting to the provider that a resource will be needed soon and
  -- should be fetched and cached so that a future call to `getResource` is more likely to succeed.  
  fetchResource :: a m -> Resource m -> IO (Either Text Blob)

class Decodable v where
  decodeResource :: Blob -> IO (Either Text v)

newtype ResourceCache v = ResourceCache { unResourceCache :: Map Text (Map Text (Either Text v)) }

cacheResult :: Text -> Text -> Either Text v -> ResourceCache v -> ResourceCache v
cacheResult groupName resourceName result (ResourceCache cache) = 
  let group = Map.findWithDefault Map.empty groupName cache
  in ResourceCache $ Map.insert groupName (Map.insert resourceName result group) cache

lookupCache :: ResourceCache v -> Text -> Text -> Maybe (Either Text v)
lookupCache (ResourceCache cache) groupName resourceName = do
  group <- Map.lookup groupName cache
  Map.lookup groupName group

type CachingResourceRequest v = StateT (ResourceCache v) IO

getResource :: (ResourceDataProvider p, Decodable v) => p m -> ResourceMap m -> Text -> Int -> CachingResourceRequest v (Maybe v)
getResource dataProvider resources groupName number =
  case resolveResource resources groupName number of
    Nothing -> return Nothing
    Just resource -> do
      cache <- get
      case lookupCache cache groupName (resourceFileName resource) of
        Just (Left error) -> return Nothing
        Just (Right cached) -> return $ Just cached
        Nothing -> do
          dataResponse <- liftIO $ fetchResource dataProvider resource
          case dataResponse of
            Left error -> do
              put $ cacheResult groupName (resourceFileName resource) (Left error) cache
              return Nothing
            Right blob -> do
              (decodeResponse :: Either Text v) <- liftIO $ decodeResource blob 
              case decodeResponse of
                Left error -> do
                  put $ cacheResult groupName (resourceFileName resource) (Left error) cache
                  return Nothing
                Right value -> do
                  put $ cacheResult groupName (resourceFileName resource) (Right value) cache
                  return $ Just value

