{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Languages.CineCer0.CineCer0State where

import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.Types
import GHCJS.Marshal.Pure
import Data.IntMap.Strict

-- Text (String) -> VideoSpec -- we already have this
-- but we really need is something like this...
-- type CineCer0Spec = IntMap VideoSpec
-- String -> Either String CineCer0Spec

{- data VideoSpec = VideoSpec {
  sampleVideo :: String,
  sourceNumber :: Int,
  playbackPosition :: Tempo -> NominalDiffTime -> UTCTime -> Maybe NominalDiffTime,
  playbackRate :: Tempo -> NominalDiffTime -> UTCTime -> Maybe Rational,
  posX :: Rational,
  posY :: Rational,
  width :: Rational,
  height :: Rational
  } -}

newtype CineCer0Video = CineCer0Video { videoJSVal :: JSVal }

instance PToJSVal CineCer0Video where pToJSVal (CineCer0Video val) = val

instance PFromJSVal CineCer0Video where pFromJSVal = CineCer0Video

foreign import javascript safe
  "var video = document.createElement('video'); video.setAttribute('src',$1); $r=video"
  makeVideo :: Text -> IO CineCer0Video

foreign import javascript safe
  "$2.appendChild($1)"
  appendVideo :: CineCer0Video -> JSVal -> IO ()

foreign import javascript safe
  "$1.removeChild($2)"
  removeVideo :: JSVal -> CineCer0Video -> IO ()

addVideo :: JSVal -> Text -> IO CineCer0Video
addVideo j url = do
  x <- makeVideo url
  appendVideo x j
  return x

updateCineCer0State :: Tempo -> UTCTime -> CineCer0Spec -> CineCer0State -> IO CineCer0State
updateCineCer0State t now spec st = do
  let toAdd = difference spec (videos st) -- :: IntMap VideoSpec
  addedVideos <- mapM (addVideo $ videoDiv st) toAdd -- :: IntMap CineCer0Video
  let toDelete = difference (videos st) spec -- :: IntMap CineCer0Video
  mapM (removeVideo $ videoDiv st) toDelete
  let videosThereBefore = difference (videos st) toDelete -- :: IntMap CineCer0Video
  let continuingVideos = union videosThereBefore addedVideos -- :: IntMap CineCer0Video
  return $ st { videos = continuingVideos }

emptyCineCer0State :: JSVal -> CineCer0State
emptyCineCer0State j = CineCer0State {
  videoDiv = j,
  videos = empty
  }

data CineCer0State = CineCer0State {
  videoDiv :: JSVal,
  videos :: IntMap CineCer0Video
  }
