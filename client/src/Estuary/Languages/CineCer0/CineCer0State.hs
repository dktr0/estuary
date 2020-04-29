{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Estuary.Languages.CineCer0.CineCer0State where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHCJS.Types
import GHCJS.DOM.Types (HTMLDivElement)
import GHCJS.Marshal.Pure
import Data.IntMap.Strict as IntMap
import Data.Time
import TextShow
import Control.Monad
import Reflex.FunctorMaybe

import Estuary.Types.Tempo
import Estuary.Languages.CineCer0.Parser
import Estuary.Languages.CineCer0.VideoSpec
import Estuary.Languages.CineCer0.Spec
import Estuary.Languages.CineCer0.Signal

newtype CineCer0Video = CineCer0Video { videoJSVal :: JSVal }

instance PToJSVal CineCer0Video where pToJSVal (CineCer0Video val) = val

instance PFromJSVal CineCer0Video where pFromJSVal = CineCer0Video

foreign import javascript safe
  "var video = document.createElement('video'); video.setAttribute('src',$1); $r=video; video.loop = true;"
  makeVideo :: Text -> IO CineCer0Video

foreign import javascript safe
  "$2.appendChild($1); $1.play();"
  appendVideo :: CineCer0Video -> HTMLDivElement -> IO ()

foreign import javascript safe
  "$1.removeChild($2)"
  removeVideo :: HTMLDivElement -> CineCer0Video -> IO ()

foreign import javascript safe
  "$1.style = $2;"
  videoStyle_ :: CineCer0Video -> Text -> IO ()

foreign import javascript unsafe
  "$1.muted = true;"
  muteVideo :: CineCer0Video -> IO ()

foreign import javascript unsafe
  "$1.src = $2; $1.load()"
  changeVideoSource :: CineCer0Video -> Text -> IO ()

changeVideoSource' :: CineCer0Video -> Text -> IO ()
changeVideoSource' cv t = do
  --T.putStrLn $ "changing to " <> t
  changeVideoSource cv t

foreign import javascript unsafe
  "$1.videoWidth"
  videoWidth :: CineCer0Video -> IO Double

foreign import javascript unsafe
  "$1.videoHeight"
  videoHeight :: CineCer0Video -> IO Double

videoStyle :: CineCer0Video -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Double -> IO ()
videoStyle v x y w h o bl br c g s = videoStyle_ v $ "left: " <> showt x <> "px; top: " <> showt y <> "px; position: absolute; width:" <> showt w <> "px; height:" <> showt h <> "px; object-fit: fill; opacity: " <> showt o <> "%; filter:blur( " <> showt bl <> "px) " <> "brightness( " <> showt br <> "%) " <> "contrast( " <> showt c <> "%) " <> "grayscale( " <> showt g <> "%) " <> "saturate( " <> showt s <> ");"

----  Rate and Position ----

foreign import javascript unsafe
  "$1.playbackRate = $2;"
  videoPlaybackRate :: CineCer0Video -> Double -> IO ()

foreign import javascript unsafe
  "$1.currentTime = $2;"
  videoPlaybackPosition :: CineCer0Video -> Double -> IO ()

foreign import javascript unsafe
  "$1.duration"
  getLengthOfVideo :: CineCer0Video -> IO Double

addVideo :: HTMLDivElement -> VideoSpec -> IO CineCer0Video
addVideo j spec = do
  --putStrLn $ "addVideo " ++ (sampleVideo spec)
  let url = T.pack $ sampleVideo spec
  x <- makeVideo url
  muteVideo x
  appendVideo x j
  return x

onlyChangedVideoSources :: VideoSpec -> VideoSpec -> Maybe VideoSpec
onlyChangedVideoSources nSpec oSpec
  | (sampleVideo nSpec /= sampleVideo oSpec) = Just nSpec
  | (sampleVideo nSpec == sampleVideo oSpec) = Nothing


updateCineCer0State :: Tempo -> UTCTime -> Spec -> CineCer0State -> IO CineCer0State
updateCineCer0State t rTime spec st = do
  --putStrLn $ show spec
  let vSpecs = videoSpecMap spec
  let eTime = evalTime spec
  divWidth <- offsetWidth $ videoDiv st
  divHeight <- offsetHeight $ videoDiv st
  -- add videos
  let newVideoSpecs = difference vSpecs (videos st) -- :: IntMap VideoSpec
  let toAdd = IntMap.filter (\x -> sampleVideo x /= "") newVideoSpecs -- :: IntMap VideoSpec
  addedVideos <- mapM (addVideo $ videoDiv st) toAdd -- :: IntMap CineCer0Video
  -- change videos
  let continuingVideoSpecs = intersectionWith onlyChangedVideoSources vSpecs (previousVideoSpecs st) -- :: IntMap (Maybe VideoSpec)
  let toChange = fmapMaybe id continuingVideoSpecs -- :: IntMap VideoSpec
  let toChange' = intersectionWith (\a b -> (a,b)) toChange $ videos st -- IntMap (VideoSpec,CineCer0Video)
  mapM_ (\(x,cv) -> changeVideoSource' cv $ T.pack (sampleVideo x)) toChange'
  -- delete VideoSpecs
  let videosWithRemovedSpecs = difference (videos st) vSpecs -- :: IntMap CineCer0Video
  let videosWithEmptySource = intersection (videos st) $ IntMap.filter (\x -> sampleVideo x == "") vSpecs -- :: IntMap CineCer0Video
  let toDelete = union videosWithRemovedSpecs videosWithEmptySource
  mapM (removeVideo $ videoDiv st) toDelete
  let videosThereBefore = difference (videos st) toDelete -- :: IntMap CineCer0Video
  -- update videoSpecs
  let continuingVideos = union videosThereBefore addedVideos -- :: IntMap CineCer0Video
  sequence $ intersectionWith (updateContinuingVideo t eTime rTime (divWidth,divHeight)) vSpecs continuingVideos
  return $ st { videos = continuingVideos, previousVideoSpecs = vSpecs } --

updateContinuingVideo :: Tempo -> UTCTime -> UTCTime -> (Double,Double) -> VideoSpec -> CineCer0Video -> IO ()
updateContinuingVideo t eTime rTime (sw,sh) s v = do
  -- need fitWidth and fitHeight to be some representation of "maximal fit"
  vw <- videoWidth v
  vh <- videoHeight v
  when (vw /= 0 && vh /= 0) $ do
    lengthOfVideo <- realToFrac <$> getLengthOfVideo v
    let aspectRatio = vw/vh
    let heightIfFitsWidth = sw / aspectRatio
    let widthIfFitsHeight = sh * aspectRatio
    let fitByWidth = heightIfFitsWidth <= sh
    let fitWidth = if fitByWidth then sw else widthIfFitsHeight
    let fitHeight = if fitByWidth then heightIfFitsWidth else sh
    let aTime = anchortTime s t eTime -- :: UTCTime
    let actualWidth = (width s t lengthOfVideo rTime eTime aTime) * realToFrac fitWidth
    let actualHeight = (height s t lengthOfVideo rTime eTime aTime) * realToFrac fitHeight
    let centreX = ((posX s t lengthOfVideo rTime eTime aTime)* 0.5 + 0.5) * realToFrac sw
    let centreY = ((posY s t lengthOfVideo rTime eTime aTime)* 0.5 + 0.5) * realToFrac sh
    let leftX = centreX - (actualWidth * 0.5)
    let topY = realToFrac sh - (centreY + (actualHeight * 0.5))
    -- update playback rate
    let rate = playbackRate s t lengthOfVideo rTime eTime aTime
    maybe (return ()) (videoPlaybackRate v) $ fmap realToFrac rate
    -- update position in time
    let pos = (playbackPosition s) t lengthOfVideo rTime eTime aTime
    maybe (return ()) (videoPlaybackPosition v) $ fmap realToFrac pos
    -- update opacity
    let opacidad = (opacity s) t lengthOfVideo rTime eTime aTime * 100
    -- update style
    let blur' = blur s t lengthOfVideo rTime eTime aTime
    let brightness' = brightness  s t lengthOfVideo rTime eTime aTime * 100
    let contrast' = contrast s t lengthOfVideo rTime eTime aTime * 100
    let grayscale' = grayscale  s t lengthOfVideo rTime eTime aTime * 100
    let saturate' = saturate  s t lengthOfVideo rTime eTime aTime
    return $ concat $ fmap show $ [("leftX",leftX),("topY",topY),("actualWidth",actualWidth), ("actualHeight",actualHeight),("opacidad",opacidad),("blur'",blur'),("brightness",brightness'),("contrast'",contrast'),("grayscale",grayscale'),("saturate'",saturate')]
    videoStyle v (floor $ leftX) (floor $ topY) (floor $ actualWidth) (floor $ actualHeight) (floor opacidad) (floor blur') (floor brightness') (floor contrast') (floor grayscale') (realToFrac saturate')


emptyCineCer0State :: HTMLDivElement -> CineCer0State
emptyCineCer0State j = CineCer0State {
  videoDiv = j,
  videos = empty,
  previousVideoSpecs = empty
  }

data CineCer0State = CineCer0State {
  videoDiv :: HTMLDivElement,
  videos :: IntMap CineCer0Video,
  previousVideoSpecs :: IntMap VideoSpec
  }

foreign import javascript unsafe
  "$1.offsetWidth"
  offsetWidth :: HTMLDivElement -> IO Double

foreign import javascript unsafe
  "$1.offsetHeight"
  offsetHeight :: HTMLDivElement -> IO Double
