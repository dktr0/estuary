{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, RecursiveDo #-}
module Estuary.Types.Samples (
  SampleMap(..),
  SampleList,
  emptySampleMap,
  loadSampleMapAsync,
  defaultSampleMapURL
) where

import GHCJS.DOM.Types(ToJSString(..), FromJSString(..), toJSString, fromJSString)
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Types

import Text.JSON(JSON, JSKey)
import qualified Text.JSON as JSON

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Sequence(Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Seq(toList)

-- TODO this would be a nice place to use text

newtype SampleMap = SampleMap { unSampleMap :: Map String SampleList }
  deriving (Show)

type SampleList = Seq String

emptySampleMap :: SampleMap
emptySampleMap = SampleMap Map.empty

instance (JSON a) => JSON (Seq a) where
  readJSON jsonVal = do
    names <- JSON.readJSONs jsonVal
    return $ Seq.fromList names
  showJSON seq = JSON.showJSONs $ Seq.toList seq

-- Need newtype so we can override default instance
instance JSON SampleMap where
  readJSON jsonVal = do
    pairs <- JSON.decJSDict "SampleMap" jsonVal
    return $ SampleMap $ Map.fromList pairs
  showJSON map = JSON.encJSDict $ Map.toList $ unSampleMap $ map

defaultSampleMapURL :: JSString
defaultSampleMapURL = "samples/sampleMap.json"

loadSampleMapAsync :: (ToJSString url) => url -> (Maybe SampleMap -> IO ()) -> IO ()
loadSampleMapAsync url onLoad = do
  rec cb <- asyncCallback1 $ \mapJs -> do
        if isNull mapJs || isUndefined mapJs
          then onLoad Nothing
          else
            case JSON.decode $ fromJSString $ pFromJSVal $ mapJs of
              JSON.Error err -> onLoad Nothing
              JSON.Ok map -> onLoad $ Just map
        releaseCallback cb
  js_loadSamplesAsync (toJSString url) cb

------------------------------------------
-- JS FFI
------------------------------------------

-- TODO should this not be connected to webdirt?
foreign import javascript safe
  "var xhr = new XMLHttpRequest();                 \n\
  \xhr.open('GET', $1, true);                      \n\
  \xhr.responseType = 'text';                      \n\
  \xhr.onload  = function() { $2(xhr.response); }; \n\
  \xhr.onabort = function() { $2(null); };         \n\
  \xhr.onerror = function() { $2(null); };         \n\
  \xhr.send();                                      "
  js_loadSamplesAsync :: JSString -> Callback (JSVal -> IO ()) -> IO ()
