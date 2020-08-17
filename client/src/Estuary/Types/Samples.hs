{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, RecursiveDo, DeriveGeneric #-}
module Estuary.Types.Samples (
  SampleMap(..),
  emptySampleMap,
  loadSampleMapAsync,
  defaultSampleMapURL
) where

import GHCJS.DOM.Types(ToJSString(..), FromJSString(..), toJSString, fromJSString)
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Types
import Data.Text (Text)
import qualified Data.Text.IO as T
import GHC.Generics
import Data.Aeson
import Foreign.JavaScript.Utils (jsonDecode)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Seq(toList)

type SampleList = Seq Text

newtype SampleMap = SampleMap { unSampleMap :: Map Text SampleList }
  deriving (Show,Generic)

instance ToJSON SampleMap
instance FromJSON SampleMap

emptySampleMap :: SampleMap
emptySampleMap = SampleMap Map.empty

defaultSampleMapURL :: Text
defaultSampleMapURL = "samples/sampleMap.json"

loadSampleMapAsync :: Text -> (Maybe SampleMap -> IO ()) -> IO ()
loadSampleMapAsync url onLoad = do
  rec cb <- asyncCallback1 $ \j -> do
        if isNull j || isUndefined j then do
            T.putStrLn "*ERROR* null/undefined sample map in loadSampleMapAsync"
            onLoad Nothing
          else do
            -- logJSVal j
            v <- fromJSValUnchecked j
            -- putStrLn $ show v
            case fromJSON (v :: Value) of
              Error e -> do
                putStrLn $ "*ERROR* can't decode sample map in loadSampleMapAsync: " ++ e
                onLoad Nothing
              Success map -> do
                T.putStrLn "loadSampleMapAsync (sample map decode okay)"
                onLoad $ Just $ SampleMap map
        releaseCallback cb
  js_loadSamplesAsync url cb

foreign import javascript safe
  "console.log($1)"
  logJSVal :: JSVal -> IO ()


------------------------------------------
-- JS FFI
------------------------------------------

-- TODO should this not be connected to webdirt?
foreign import javascript safe
  "var xhr = new XMLHttpRequest();                 \n\
  \xhr.open('GET', $1, true);                      \n\
  \xhr.responseType = 'text';                      \n\
  \xhr.onload  = function() { $2(JSON.parse(xhr.responseText)); }; \n\
  \xhr.onabort = function() { $2(null); };         \n\
  \xhr.onerror = function() { $2(null); };         \n\
  \xhr.send();                                      "
  js_loadSamplesAsync :: Text -> Callback (JSVal -> IO ()) -> IO ()
