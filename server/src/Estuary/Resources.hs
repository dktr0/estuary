{-# LANGUAGE BangPatterns #-}
module Estuary.Resources where

import Control.Monad

import Data.Functor

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Sequence(Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Seq(toList)

import Data.Text

import Estuary.Types.Scope as Scope
import Estuary.Types.Resources

import Sound.HTagLib

import System.IO
import System.Directory
import System.FilePath

import Text.JSON(JSON)
import qualified Text.JSON as JSON

-- Resource map layout:
-- <groupName>/meta.json
-- <groupName>/<resource>

-- Resources layout:
-- <scopeRoot>/audio/<resourceMap>
-- <scopeRoot>/video/<resourceMap>
-- <scopeRoot>/image/<resourceMap>

-- Public resources:
-- <root>/<resources>

-- Ensemble resources:
-- <root>/ensembles/<ensembleName>/<resources>

-- also allow putting an entire library in a <resourceMap>
--    - no files -> sample library
--    audio/ensembles/<name>/
--    audio/Cybernetic/bd and audio/dirt/db 
--    ensembles/<name>/audio/Cybernetic/bd 


loadFromFileSystem :: FilePath -> IO Resources
loadFromFileSystem resourceRoot = undefined

newtype MetaMap = MetaMap { unMetaMap :: Map FilePath ResourceMeta }

data ResourceMeta = ResourceMeta {
    resourceMetaTags :: Seq Text
}

emptyResourceMeta :: ResourceMeta
emptyResourceMeta = ResourceMeta { resourceMetaTags = Seq.empty }

instance (JSON a) => JSON (Seq a) where
    readJSON jsonVal = do
      names <- JSON.readJSONs jsonVal
      return $ Seq.fromList names
    showJSON seq = JSON.showJSONs $ Seq.toList seq

instance JSON MetaMap where
    readJSON jsonVal = do
      pairs <- JSON.decJSDict "MetaMap" jsonVal
      return $ MetaMap $ Map.fromList pairs
    showJSON = JSON.encJSDict . Map.toList . unMetaMap 

instance JSON ResourceMeta where
    readJSON (JSON.JSObject jsonVal) = do
        tags <- JSON.valFromObj "tags" jsonVal
        return $ ResourceMeta { resourceMetaTags = tags }
    showJSON m = JSON.makeObj $ [("tags", JSON.showJSON $ resourceMetaTags m)]

loadResources :: Scope -> FilePath -> IO (Resources)
loadResources scope rootPath = do
    audioResources <- loadResourceMap readAudioMeta scope (rootPath </> "audio")
    return $ Resources {
        audioResources = audioResources,
        videoResources = ResourceMap Map.empty,
        imageResources = ResourceMap Map.empty
    }

loadResourceMap :: (FilePath -> IO m) -> Scope -> FilePath -> IO (ResourceMap m)
loadResourceMap loadResourceMeta scope mapRootPath = do
    listDirectory mapRootPath >>= print
    groupDirectory <- listDirectory mapRootPath >>= filterM (doesDirectoryExist . (mapRootPath </>))
    groups <- forM groupDirectory $ \groupName -> do
        group <- loadResourceGroup loadResourceMeta scope (mapRootPath </> groupName)
        return (pack $ groupName, group)
    return $ ResourceMap $ Map.fromList groups

loadResourceGroup :: (FilePath -> IO m) -> Scope -> FilePath -> IO (Seq (Resource m))
loadResourceGroup loadResourceMeta scope groupPath = do
    (MetaMap explicitMetaMap) <- loadMetaMap groupPath
    resourceFiles <- (listDirectory groupPath <&> Prelude.filter (/= "meta.json")) >>= filterM (doesFileExist . (groupPath </>))
    resources <- forM resourceFiles $ \resourceFileName -> do
        fileSize <- getFileSize (groupPath </> resourceFileName)
        let explicitMeta = Map.findWithDefault emptyResourceMeta resourceFileName explicitMetaMap
        meta <- loadResourceMeta (groupPath </> resourceFileName)

        return $ Resource {
            file = pack resourceFileName,
            fileSize = fileSize,
            meta = meta,
            tags = resourceMetaTags explicitMeta,
            scope = scope
          }
    
    return $ Seq.fromList resources

loadMetaMap :: FilePath -> IO MetaMap
loadMetaMap groupPath = do
    exists <- doesFileExist (groupPath </> "meta.json")
    if exists then
        withFile (groupPath </> "meta.json") ReadMode $ \f -> do
            hSetEncoding f utf8
            contents <- hGetContents f
            -- the bang is **required**. Otherwise this lazy evaluation happens after the file handle
            -- is closed causing an error.
            let !(JSON.Ok out) = JSON.decode contents
            return out
    else
        return $ MetaMap $ Map.empty 

--------------------------------------------------------------------------------
-- File meta readers
--------------------------------------------------------------------------------

readAudioMeta :: FilePath -> IO AudioMeta
readAudioMeta path = getTags path audioMetaGetter
  where audioMetaGetter :: TagGetter AudioMeta
        audioMetaGetter = AudioMeta
            <$> fmap (fromIntegral . unDuration) durationGetter
            -- <*> channelsGetter