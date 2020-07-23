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
import Control.Exception

import Data.Tempo
import Estuary.Languages.CineCer0.Parser
import Estuary.Languages.CineCer0.VideoSpec as Cinecer0
import Estuary.Languages.CineCer0.Spec
import Estuary.Languages.CineCer0.Signal


-- A JSVideo is just a DOM 'video' element (a wrapped JSVal handle to an object in the browser).
-- Because it is an instance of PToJSVal and PFromJSVal we can define Javascript FFI functions
-- with JSVideo values as arguments, to query and set the status/activity of the video.

newtype JSVideo = JSVideo { videoJSVal :: JSVal }

instance PToJSVal JSVideo where pToJSVal (JSVideo x) = x
instance PFromJSVal JSVideo where pFromJSVal = JSVideo
foreign import javascript unsafe "$1.offsetWidth" offsetWidth :: HTMLDivElement -> IO Double
foreign import javascript unsafe "$1.offsetHeight" offsetHeight :: HTMLDivElement -> IO Double
foreign import javascript unsafe
  "var video = document.createElement('video'); video.setAttribute('src',$1); $r=video; video.loop = true;"
  makeVideo :: Text -> IO JSVideo
foreign import javascript unsafe
  "$2.appendChild($1); $1.play();"
  appendVideo :: JSVideo -> HTMLDivElement -> IO ()
foreign import javascript unsafe "$1.removeChild($2)" removeVideo :: HTMLDivElement -> JSVideo -> IO ()
foreign import javascript unsafe "$1.style = $2;" _setVideoStyle :: JSVideo -> Text -> IO ()
foreign import javascript unsafe "$1.muted = $2;" muteVideo :: JSVideo -> Bool -> IO ()
foreign import javascript unsafe "$1.volume = $2" videoVolume :: JSVideo -> Double -> IO ()
foreign import javascript unsafe "$1.pause(); $1.src = $2; $1.load(); $1.play();" changeVideoSource :: JSVideo -> Text -> IO ()
foreign import javascript unsafe "$1.videoWidth" videoWidth :: JSVideo -> IO Double
foreign import javascript unsafe "$1.videoHeight" videoHeight :: JSVideo -> IO Double
foreign import javascript unsafe "$1.playbackRate" getVideoPlaybackRate :: JSVideo -> IO Double
foreign import javascript safe "$1.playbackRate = $2;" setVideoPlaybackRate :: JSVideo -> Double -> IO ()
foreign import javascript unsafe "$1.currentTime" getVideoPlaybackPosition :: JSVideo -> IO Double
foreign import javascript unsafe "$1.currentTime = $2;" setVideoPlaybackPosition :: JSVideo -> Double -> IO ()
foreign import javascript unsafe "$1.duration" getLengthOfVideo :: JSVideo -> IO Double


-- A CineCer0Video is a JSVideo (the DOM element) together with some other things we need to keep
-- track of from frame to frame, such as what the style was in the previous frame and whether it is
-- too soon (or not) to set the playback position again.

data CineCer0Video = CineCer0Video {
  jsVideo :: JSVideo,
  positionLock :: Int,
  previousStyle :: Text,
  previousVol :: Double
  }

addVideo :: HTMLDivElement -> VideoSpec -> IO CineCer0Video
addVideo j spec = do
  let url = T.pack $ sampleVideo spec
  x <- makeVideo url
  muteVideo x True
  appendVideo x j
  return $ CineCer0Video {
    jsVideo = x,
    positionLock = 0,
    previousStyle = "",
    previousVol = 0
  }

setVideoRateAndPosition :: CineCer0Video -> Double -> Maybe Double -> Maybe Double -> IO CineCer0Video
setVideoRateAndPosition v vLength (Just r) (Just p) = do
  if ((r >= 0.0625) && (r <= 16)) then do
    let j = jsVideo v
    -- set rate
    x <- getVideoPlaybackRate j
    when (x /= r) $ setVideoPlaybackRate j r
    -- set playback position
    if positionLock v == 0 then do
      currentPos <- getVideoPlaybackPosition j
      let diff1 = abs (currentPos - p)
      let diff2 = abs (currentPos + vLength - p)
      let diff3 = abs (p + vLength - currentPos)
      let diff = min (min diff1 diff2) diff3
      if diff > 0.050 then do
        -- putStrLn $ "currentPos=" ++ show currentPos ++ "  target="++show p ++ "  diff=" ++ show (currentPos - p)
        setVideoPlaybackPosition j (p+(0.15*r))
        return $ v { positionLock = 12 } -- wait 12 frames before setting position again
      else return v
    else return $ v { positionLock = positionLock v - 1 }
  else return v -- silently fail when rate is outside of range [0.0625,16]
setVideoRateAndPosition v _ _ _ = return v

setVideoStyle :: CineCer0Video -> Text -> IO CineCer0Video
setVideoStyle v x = do
  if previousStyle v == x then return v
  else do
    _setVideoStyle (jsVideo v) x
    return $ v { previousStyle = x }

setVideoVol :: CineCer0Video -> Double -> IO CineCer0Video
setVideoVol v x = do
  if previousVol v == x then return v
  else do
    let j = jsVideo v
    muteVideo j $ x == 0
    videoVolume j x
    return $ v { previousVol = x }

updateContinuingVideo :: Tempo -> UTCTime -> UTCTime -> (Double,Double) -> VideoSpec -> CineCer0Video -> IO CineCer0Video
updateContinuingVideo t eTime rTime (sw,sh) s v = logExceptions v $ do
  let j = jsVideo v
  vw <- videoWidth j
  vh <- videoHeight j
  if (vw /= 0 && vh /= 0) then do
    lengthOfVideo <- realToFrac <$> getLengthOfVideo j
    let aspectRatio = vw/vh
    let heightIfFitsWidth = sw / aspectRatio
    let widthIfFitsHeight = sh * aspectRatio
    let fitByWidth = heightIfFitsWidth <= sh
    let fitWidth = if fitByWidth then sw else widthIfFitsHeight
    let fitHeight = if fitByWidth then heightIfFitsWidth else sh
    let aTime = anchorTime s t eTime -- :: UTCTime
    let actualWidth = (width s t lengthOfVideo rTime eTime aTime) * realToFrac fitWidth
    let actualHeight = (height s t lengthOfVideo rTime eTime aTime) * realToFrac fitHeight
    let centreX = ((posX s t lengthOfVideo rTime eTime aTime)* 0.5 + 0.5) * realToFrac sw
    let centreY = ((posY s t lengthOfVideo rTime eTime aTime)* 0.5 + 0.5) * realToFrac sh
    let leftX = centreX - (actualWidth * 0.5)
    let topY = realToFrac sh - (centreY + (actualHeight * 0.5))

    -- update playback rate and position
    let rate = fmap realToFrac $ playbackRate s t lengthOfVideo rTime eTime aTime
    let pos = fmap realToFrac $ playbackPosition s t lengthOfVideo rTime eTime aTime
    v' <- setVideoRateAndPosition v (realToFrac lengthOfVideo) rate pos

    -- update audio (volume and mute)
    let normVol = if (volume s t lengthOfVideo rTime eTime aTime) > 1 then 1 else (volume s t lengthOfVideo rTime eTime aTime)
    v'' <- setVideoVol v' $ realToFrac normVol

    -- update style (size, position, opacity, etc)
    let opacity' = (*) <$> (opacity s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let blur' = blur s t lengthOfVideo rTime eTime aTime
    let brightness' = (*) <$> (brightness s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let contrast' = (*) <$> (contrast s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let grayscale' = (*) <$> (grayscale s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let saturate' = (*) <$> (saturate s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let filterText = generateFilter (fmap realToFrac opacity') (fmap realToFrac blur') (fmap realToFrac brightness') (fmap realToFrac contrast') (fmap realToFrac grayscale') (fmap realToFrac saturate')
    let mask' = ((Cinecer0.mask s) t lengthOfVideo rTime eTime aTime)
    setVideoStyle v'' $ videoStyle (realToFrac $ leftX) (realToFrac $ topY) (realToFrac $ actualWidth) (realToFrac $ actualHeight) filterText mask'

  else return v

generateOpacity :: Maybe Double -> Text
generateOpacity (Just o) = "opacity(" <> showt o <> "%)"
generateOpacity Nothing = ""

generateBlur :: Maybe Double -> Text
generateBlur (Just bl) = "blur(" <> showt bl <> "px)"
generateBlur Nothing = ""

generateBrightness :: Maybe Double -> Text
generateBrightness (Just br) = "brightness(" <> showt br <> "%)"
generateBrightness Nothing = ""

generateContrast :: Maybe Double -> Text
generateContrast (Just c) = "contrast(" <> showt c <> "%)"
generateContrast Nothing = ""

generateGrayscale :: Maybe Double -> Text
generateGrayscale (Just g) = "grayscale(" <> showt g <> "%)"
generateGrayscale Nothing = ""

generateSaturate :: Maybe Double -> Text
generateSaturate (Just s) = "saturate(" <> showt s <> "%)"
generateSaturate Nothing = ""

generateFilter :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> Text
generateFilter Nothing Nothing Nothing Nothing Nothing Nothing = ""
generateFilter o bl br c g s = "filter:" <> generateOpacity o <> generateBlur bl <> generateBrightness br <> generateContrast c <> generateGrayscale g <> generateSaturate s <> ";"

videoStyle :: Double -> Double -> Double -> Double -> Text -> Text -> Text
videoStyle x y w h f m = "left: " <> showt x <> "px; top: " <> showt y <> "px; position: absolute; width:" <> showt w <> "px; height:" <> showt h <> "px; object-fit: fill;" <> f <> m

onlyChangedVideoSources :: VideoSpec -> VideoSpec -> Maybe VideoSpec
onlyChangedVideoSources nSpec oSpec
  | (sampleVideo nSpec /= sampleVideo oSpec) = Just nSpec
  | (sampleVideo nSpec == sampleVideo oSpec) = Nothing


-- A CineCer0State represents the entire state corresponding to a CineCer0 program
-- (each statement separated by ; in the program is one element within various )
data CineCer0State = CineCer0State {
  videoDiv :: HTMLDivElement,
  videos :: IntMap CineCer0Video,
  previousVideoSpecs :: IntMap VideoSpec
  }

emptyCineCer0State :: HTMLDivElement -> CineCer0State
emptyCineCer0State j = CineCer0State {
  videoDiv = j,
  videos = empty,
  previousVideoSpecs = empty
  }

updateCineCer0State :: Tempo -> UTCTime -> Spec -> CineCer0State -> IO CineCer0State
updateCineCer0State t rTime spec st = logExceptions st $ do
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
  mapM_ (\(x,cv) -> changeVideoSource (jsVideo cv) $ T.pack (sampleVideo x)) toChange'
  -- delete VideoSpecs
  let videosWithRemovedSpecs = difference (videos st) vSpecs -- :: IntMap CineCer0Video
  let videosWithEmptySource = intersection (videos st) $ IntMap.filter (\x -> sampleVideo x == "") vSpecs -- :: IntMap CineCer0Video
  let toDelete = union videosWithRemovedSpecs videosWithEmptySource
  mapM (\x -> removeVideo (videoDiv st) (jsVideo x)) toDelete
  let videosThereBefore = difference (videos st) toDelete -- :: IntMap CineCer0Video
  -- update cached state
  let continuingVideos = union videosThereBefore addedVideos -- :: IntMap CineCer0Video
  continuingVideos' <- sequence $ intersectionWith (updateContinuingVideo t eTime rTime (divWidth,divHeight)) vSpecs continuingVideos -- :: IntMap CineCer0Video
  return $ st { videos = continuingVideos', previousVideoSpecs = vSpecs }


logExceptions :: a -> IO a -> IO a
logExceptions a x = x `catch` (\e -> do
  putStrLn $ "EXCEPTION (CineCer0): " ++ show (e :: SomeException)
  return a
  )
