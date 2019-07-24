{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables #-}
module Estuary.Widgets.ResourceUpload where

import Control.Monad
import Control.Monad.IO.Class(liftIO)

import Data.Functor

import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

import qualified Data.Sequence as Seq
import Data.Sequence(Seq)

import Data.Text

import Data.Word

import Estuary.Reflex.Utility

import Estuary.Render.LocalResources

import Estuary.Types.Context
import Estuary.Types.Resources
import Estuary.Types.Scope

import GHCJS.DOM.File
import GHCJS.DOM.Blob
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Types

import Reflex
import Reflex.Dom
import Reflex.Dom.Widget.Basic
import Reflex.Dom.Widget.Input
import Reflex.PerformEvent.Class

-- move the sample map to estuary so we give webdirt a buffer rather than a sample name so it does it's own caching
-- no serialization needed from resource map to webdirt samples.
-- estuary does the loading/caching itself which can be shared logic between the resource types.

addPrivateAudioResource :: Text -> Blob -> Resource AudioMeta -> ContextChange
addPrivateAudioResource = addPrivateResource 
  localAudioResources (\s v -> s {localAudioResources = v})
  audioResources (\s v -> s {audioResources = v})

resourceUploader :: forall t m. (MonadWidget t m) => m (Event t ContextChange)
resourceUploader = do
  text "group"
  dynGroupName <- liftM _textInput_value $ textInput $ def

  text "file"
  
  let attrs = constDyn ("accept" =: "audio/*") :: Dynamic t (Map Text Text)
  -- try changing constraints of resourceUploader to be more specific and that might fix this?
  --(dynSelectedFiles :: Dynamic t [File]) <- liftM _fileInput_value $ fileInput $ def & fileInputConfig_attributes .~ attrs
  (dynSelectedFiles :: Dynamic t [File]) <- liftM _fileInput_value $ fileInput $ def

  uploadClickedEv <- button "pick"

  let dynSelected :: Dynamic t [(Text, File)]
      dynSelected = zipDynWith (\name -> fmap (\f -> (name, f))) dynGroupName dynSelectedFiles
  
  -- (<@) :: Reflex t => Behavior t b -> Event t a -> Event t b 
  -- performEventAsync :: Event t ( (a -> IO ()) -> Performable m () ) -> m (Event t a)
  -- forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)

  dynE $
    ffor dynSelected $ \selectedFiles -> do
      (resourceLoadedEvs :: [Event t ContextChange]) <- forM selectedFiles $ \(group, file) -> do
        (resourceLoadedEv :: Event t (Resource AudioMeta)) <- performEventAsync $ liftIO . pickAudioResource file <$ uploadClickedEv
        return $ resourceLoadedEv <&> addPrivateAudioResource group (toBlob file)
          
      return $ mergeWith (.) resourceLoadedEvs


pickAudioResource :: File -> (Resource AudioMeta -> IO ()) -> IO ()
pickAudioResource f done = do
  withAudioMeta f $ \meta -> do
    name <- getName f
    size <- getSize f
    done $ Resource {
      resourceFileName = name,
      resourceFileSize = toInteger size,
      resourceMeta = meta,
      resourceTags = Seq.empty,
      resourceScope = Private
    }

withAudioMeta :: File -> (AudioMeta -> IO ()) -> IO ()
withAudioMeta f cb = mdo
  handleDone <- asyncCallback1 $ \jsDuration -> do
    (Just duration) <- fromJSVal jsDuration :: IO (Maybe Double)
    cb (AudioMeta duration)
    releaseCallback handleDone

  js_getAudioDuration f handleDone

foreign import javascript unsafe
  "var _audio = document.createElement('audio'); \
  \_audio.addEventListener('loadedmetadata', function() {$2(_audio.duration)}); \
  \_audio.src = window.URL.createObjectURL($1);"
  js_getAudioDuration :: File -> Callback (JSVal -> IO ()) -> IO ()