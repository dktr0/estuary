{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Estuary.Languages.TiempoEspacio.EscucharState where

import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.Types
import GHCJS.DOM.Types (HTMLDivElement)
import GHCJS.Marshal.Pure
import Data.IntMap.Strict as IntMap
import Data.Time
import TextShow

import Estuary.Types.Tempo
import Estuary.Languages.TiempoEspacio.Escuchar
import Estuary.Languages.CineCer0.VideoSpec

newtype EscucharVideo = EscucharVideo { videoJSVal :: JSVal }

instance PToJSVal EscucharVideo where pToJSVal (EscucharVideo val) = val

instance PFromJSVal EscucharVideo where pFromJSVal = EscucharVideo

foreign import javascript safe
  "var video = document.createElement('video'); video.setAttribute('src',$1); $r=video; video.loop = true;"
  makeVideo :: Text -> IO EscucharVideo

foreign import javascript safe
  "$2.appendChild($1); $1.play();"
  appendVideo :: EscucharVideo -> HTMLDivElement -> IO ()

foreign import javascript safe
  "$1.removeChild($2)"
  removeVideo :: HTMLDivElement -> EscucharVideo -> IO ()

foreign import javascript safe
  "$1.style = $2;"
  videoGeometry_ :: EscucharVideo -> Text -> IO ()

foreign import javascript unsafe
  "$1.muted = true;"
  muteVideo :: EscucharVideo -> IO ()

videoGeometry :: EscucharVideo -> Int -> Int -> Int -> Int -> IO ()
videoGeometry v x y w h = videoGeometry_ v $ "left: " <> showt x <> "%; top: " <> showt y <> "%; position: absolute; width:" <> showt w <> "%; height:" <> "%;"

addVideo :: HTMLDivElement -> VideoSpec -> IO EscucharVideo
addVideo j spec = do
  let url = T.pack $ sampleVideo spec
  x <- makeVideo url
  muteVideo x
  appendVideo x j
  return x

updateEscucharState :: Tempo -> UTCTime -> EscucharSpec -> EscucharState -> IO EscucharState
updateEscucharState t now spec st = do
  -- add or delete videos
  let newVideoSpecs = difference spec (videos st) -- :: IntMap VideoSpec
  let toAdd = IntMap.filter (\x -> sampleVideo x /= "") newVideoSpecs
  addedVideos <- mapM (addVideo $ videoDiv st) toAdd -- :: IntMap EscucharVideo
  let videosWithRemovedSpecs = difference (videos st) spec -- :: IntMap EscucharVideo
  let videosWithEmptySource = intersection (videos st) $ IntMap.filter (\x -> sampleVideo x == "") spec -- :: IntMap EscucharVideo
  let toDelete = union videosWithRemovedSpecs videosWithEmptySource
  mapM (removeVideo $ videoDiv st) toDelete
  let videosThereBefore = difference (videos st) toDelete -- :: IntMap EscucharVideo
  let continuingVideos = union videosThereBefore addedVideos -- :: IntMap EscucharVideo
  sequence $ intersectionWith (updateContinuingVideo t now) spec continuingVideos
  return $ st { videos = continuingVideos }

updateContinuingVideo :: Tempo -> UTCTime -> VideoSpec -> EscucharVideo -> IO ()
updateContinuingVideo t now s v = do
  let fitWidth = 100
  let fitHeight = 100
  let actualWidth = floor $ width s * fitWidth
  let actualHeight = floor $ height s * fitHeight
  videoGeometry v (floor $ posX s) (floor $ posY s) actualWidth actualHeight

emptyEscucharState :: HTMLDivElement -> EscucharState
emptyEscucharState j = EscucharState {
  videoDiv = j,
  videos = empty
  }

data EscucharState = EscucharState {
  videoDiv :: HTMLDivElement,
  videos :: IntMap EscucharVideo
  }
