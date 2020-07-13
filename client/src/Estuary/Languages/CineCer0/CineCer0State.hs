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

newtype VideoObject = VideoObject { videoJSVal :: JSVal }
newtype TextObject = TextObject { textJSVal :: JSVal }

instance PToJSVal VideoObject where pToJSVal (VideoObject x) = x
instance PFromJSVal VideoObject where pFromJSVal = VideoObject
foreign import javascript unsafe "$1.offsetWidth" offsetWidth :: HTMLDivElement -> IO Double
foreign import javascript unsafe "$1.offsetHeight" offsetHeight :: HTMLDivElement -> IO Double
foreign import javascript unsafe
  "var video = document.createElement('video'); video.setAttribute('src',$1); $r=video; video.loop = true;"
  makeVideo :: Text -> IO VideoObject
foreign import javascript unsafe
  "$2.appendChild($1); $1.play();"
  appendVideo :: VideoObject -> HTMLDivElement -> IO ()
foreign import javascript unsafe
  "var text = document.createElement('div'); text.innerText = $1; $r=text;"
  makeTexto :: Text -> IO TextObject
foreign import javascript unsafe 
  "$2.appendChild($1);"
  appendTexto :: TextObject -> HTMLDivElement -> IO ()
foreign import javascript unsafe "$1.removeChild($2)" removeVideo :: HTMLDivElement -> VideoObject -> IO ()
foreign import javascript unsafe "$1.style = $2;" _setVideoStyle :: VideoObject -> Text -> IO ()
foreign import javascript unsafe "$1.muted = $2;" muteVideo :: VideoObject -> Bool -> IO ()
foreign import javascript unsafe "$1.volume = $2" videoVolume :: VideoObject -> Double -> IO ()
foreign import javascript unsafe "$1.pause(); $1.src = $2; $1.load(); $1.play();" changeVideoSource :: VideoObject -> Text -> IO ()
foreign import javascript unsafe "$1.videoWidth" videoWidth :: VideoObject -> IO Double
foreign import javascript unsafe "$1.videoHeight" videoHeight :: VideoObject -> IO Double
foreign import javascript unsafe "$1.playbackRate" getVideoPlaybackRate :: VideoObject -> IO Double
foreign import javascript safe "$1.playbackRate = $2;" setVideoPlaybackRate :: VideoObject -> Double -> IO ()
foreign import javascript unsafe "$1.currentTime" getVideoPlaybackPosition :: VideoObject -> IO Double
foreign import javascript unsafe "$1.currentTime = $2;" setVideoPlaybackPosition :: VideoObject -> Double -> IO ()
foreign import javascript unsafe "$1.duration" getLengthOfVideo :: VideoObject -> IO Double


-- A CineCer0Video is a JSVideo (the DOM element) together with some other things we need to keep
-- track of from frame to frame, such as what the style was in the previous frame and whether it is
-- too soon (or not) to set the playback position again.

data CineCer0Video = CineCer0Video {
  videoObject :: VideoObject,
  positionLock :: Int,
  previousStyle :: Text,
  previousVol :: Double
  }

data CineCer0Texto = CineCer0Texto {
  textObject :: TextObject,
  positionLockTx :: Int,
  previousStyleTx :: Text
  }

addTextOrVideo :: HTMLDivElement -> Either String String -> IO CineCer0Video
addTextOrVideo j (Right x) = addVideo j x 
addTextOrVideo j (Left x) = addTexto j x -- for now there is no text infrastructure

addVideo :: HTMLDivElement -> String -> IO CineCer0Video
addVideo j vspec = do
  let url = T.pack $ vspec
  x <- makeVideo url
  muteVideo x True
  appendVideo x j
  return $ CineCer0Video {
    videoObject = x,
    positionLock = 0,
    previousStyle = "",
    previousVol = 0
  }

addTexto :: HTMLDivElement -> String -> IO CineCer0Texto
addTexto j vspec = do
  let texto = T.pack $ vspec
  x <- makeTexto texto
  appendTexto x j
  return $ CineCer0Texto {
    textObject = x,
    positionLockTx = 0,
    previousStyleTx = ""
  }

getTextOrVideoString:: Either String String -> String
getTextOrVideoString (Right x) = x
getTextOrVideoString (Left x) = x -- for the moment there is no text infrastructure

ifEmpty:: Either String String -> Bool
ifEmpty (Right x) = x == ""
ifEmpty (Left x) = x == ""

setVideoRateAndPosition :: CineCer0Video -> Double -> Maybe Double -> Maybe Double -> IO CineCer0Video
setVideoRateAndPosition v vLength (Just r) (Just p) = do
  if ((r >= 0.0625) && (r <= 16)) then do
    let j = videoObject v
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
    _setVideoStyle (videoObject v) x
    return $ v { previousStyle = x }

setVideoVol :: CineCer0Video -> Double -> IO CineCer0Video
setVideoVol v x = do
  if previousVol v == x then return v
  else do
    let j = videoObject v
    muteVideo j $ x == 0
    videoVolume j x
    return $ v { previousVol = x }

updateContinuingVideo :: Tempo -> UTCTime -> UTCTime -> (Double,Double) -> VideoSpec -> CineCer0Video -> IO CineCer0Video
updateContinuingVideo t eTime rTime (sw,sh) s v = logExceptions v $ do
  let j = videoObject v
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
    v'' <- setVideoVol v' $ realToFrac (volume s t lengthOfVideo rTime eTime aTime)

    -- update style (size, position, opacity, etc)
    let opacity' = (*) <$> (opacity s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let blur' = blur s t lengthOfVideo rTime eTime aTime
    let brightness' = (*) <$> (brightness s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let contrast' = (*) <$> (contrast s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let grayscale' = (*) <$> (grayscale s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let saturate' = (*) <$> (saturate s) t lengthOfVideo rTime eTime aTime <*> Just 100
    -- let fontFamily' = (*) <$> (fontFamily s) t lengthOfVideo rTime eTime aTime <*> Just "Impact"
    -- let fontSize' = (*) <$> (fontSize s) t lengthOfVideo rTime eTime aTime <*> Just 50
    let filterText = generateFilter (fmap realToFrac opacity') (fmap realToFrac blur') (fmap realToFrac brightness') (fmap realToFrac contrast') (fmap realToFrac grayscale') (fmap realToFrac saturate') -- (fmap fontFamily') (fmap fontSize')
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
generateFilter o bl br c g s = "filter:" <> generateOpacity o <> generateBlur bl <> generateBrightness br <> generateContrast c <> generateGrayscale g <> generateSaturate s <>";"

videoStyle :: Double -> Double -> Double -> Double -> Text -> Text -> Text
videoStyle x y w h f m = "left: " <> showt x <> "px; top: " <> showt y <> "px; position: absolute; width:" <> showt w <> "px; height:" <> showt h <> "px; object-fit: fill;" <> f <> m

onlyChangedVideoSources :: VideoSpec -> VideoSpec -> Maybe VideoSpec
onlyChangedVideoSources nSpec oSpec
  | (textOrVideo nSpec /= textOrVideo oSpec) = Just nSpec
  | (textOrVideo nSpec == textOrVideo oSpec) = Nothing


-- A CineCer0State represents the entire state corresponding to a CineCer0 program
-- (each statement separated by ; in the program is one element within various )
data CineCer0State = CineCer0State {
  videoDiv :: HTMLDivElement,
  videos :: IntMap CineCer0Video,
  previousVideoSpecs :: IntMap VideoSpec,
  textDiv :: HTMLDivElement,
  texts :: IntMap CineCer0Texto
  }

emptyCineCer0State :: HTMLDivElement -> CineCer0State
emptyCineCer0State j = CineCer0State {
  videoDiv = j,
  videos = empty,
  previousVideoSpecs = empty,
  textDiv = j,
  texts = empty
  }

updateCineCer0State :: Tempo -> UTCTime -> Spec -> CineCer0State -> IO CineCer0State
updateCineCer0State t rTime spec st = logExceptions st $ do
  let vSpecs = videoSpecMap spec
  let eTime = evalTime spec
  divWidth <- offsetWidth $ videoDiv st
  divHeight <- offsetHeight $ videoDiv st
  -- add videos
  let newVideoSpecs = difference vSpecs (videos st) -- :: IntMap VideoSpec
  let toAdd = IntMap.filter (\x -> ifEmpty (textOrVideo x) == False) newVideoSpecs -- :: IntMap VideoSpec
  addedVideos <- mapM (\x -> addTextOrVideo (videoDiv st) (textOrVideo x)) toAdd -- :: IntMap CineCer0Video

  -- change videos
  let continuingVideoSpecs = intersectionWith onlyChangedVideoSources vSpecs (previousVideoSpecs st) -- :: IntMap (Maybe VideoSpec)
  let toChange = fmapMaybe id continuingVideoSpecs -- :: IntMap VideoSpec
  let toChange' = intersectionWith (\a b -> (a,b)) toChange $ videos st -- IntMap (VideoSpec,CineCer0Video)
  mapM_ (\(x,cv) -> changeVideoSource (videoObject cv) $ T.pack (getTextOrVideoString (textOrVideo x))) toChange'
  -- delete VideoSpecs
  let videosWithRemovedSpecs = difference (videos st) vSpecs -- :: IntMap CineCer0Video
  let videosWithEmptySource = intersection (videos st) $ IntMap.filter (\x -> (ifEmpty $ textOrVideo x) == True) vSpecs -- :: IntMap CineCer0Video
  let toDelete = union videosWithRemovedSpecs videosWithEmptySource
  mapM (\x -> removeVideo (videoDiv st) (videoObject x)) toDelete
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
