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


-- A JSVideo is just a DOM 'video' element (a wrapped JSVal handle to an Layer in the browser).
-- Because it is an instance of PToJSVal and PFromJSVal we can define Javascript FFI functions
-- with JSVideo values as arguments, to query and set the status/activity of the video.

newtype VideoLayer = VideoLayer { videoJSVal :: JSVal }
newtype TextLayer = TextLayer { textJSVal :: JSVal }

instance PToJSVal VideoLayer where pToJSVal (VideoLayer x) = x
instance PFromJSVal VideoLayer where pFromJSVal = VideoLayer

instance PToJSVal TextLayer where pToJSVal (TextLayer x) = x
instance PFromJSVal TextLayer where pFromJSVal = TextLayer


foreign import javascript unsafe "$1.offsetWidth" offsetWidth :: HTMLDivElement -> IO Double
foreign import javascript unsafe "$1.offsetHeight" offsetHeight :: HTMLDivElement -> IO Double
foreign import javascript unsafe
  "var video = document.createElement('video'); video.setAttribute('src',$1); $r=video; video.loop = true;"
  makeVideo :: Text -> IO VideoLayer
foreign import javascript unsafe
  "$2.appendChild($1); $1.play();"
  appendVideo :: VideoLayer -> HTMLDivElement -> IO ()
foreign import javascript unsafe
  "var text = document.createElement('div'); text.innerText = $1; $r=text;"
  makeText :: Text -> IO TextLayer
foreign import javascript unsafe 
  "$2.appendChild($1);"
  appendText :: TextLayer -> HTMLDivElement -> IO ()
  -- remove js funcs might become remove Layer
foreign import javascript unsafe "$1.removeChild($2)" removeVideo :: HTMLDivElement -> VideoLayer -> IO ()
foreign import javascript unsafe "$1.removeChild($2)" removeText :: HTMLDivElement -> TextLayer -> IO ()
foreign import javascript unsafe "$1.style = $2;" _setVideoStyle :: VideoLayer -> Text -> IO ()
foreign import javascript unsafe "$1.style = $2;" _setTextStyle :: TextLayer -> Text -> IO ()
foreign import javascript unsafe "$1.muted = $2;" muteVideo :: VideoLayer -> Bool -> IO ()
foreign import javascript unsafe "$1.volume = $2" videoVolume :: VideoLayer -> Double -> IO ()
foreign import javascript unsafe "$1.pause(); $1.src = $2; $1.load(); $1.play();" changeVideoSource :: VideoLayer -> Text -> IO ()
foreign import javascript unsafe "$1.src = $2;" changeTextSource :: TextLayer -> Text -> IO ()
foreign import javascript unsafe "$1.videoWidth" videoWidth :: VideoLayer -> IO Double
foreign import javascript unsafe "$1.videoHeight" videoHeight :: VideoLayer -> IO Double
foreign import javascript unsafe "$1.width" textWidth :: TextLayer -> IO Double
foreign import javascript unsafe "$1.height" textHeight :: TextLayer -> IO Double

foreign import javascript unsafe "$1.playbackRate" getVideoPlaybackRate :: VideoLayer -> IO Double
foreign import javascript safe "$1.playbackRate = $2;" setVideoPlaybackRate :: VideoLayer -> Double -> IO ()
foreign import javascript unsafe "$1.currentTime" getVideoPlaybackPosition :: VideoLayer -> IO Double
foreign import javascript unsafe "$1.currentTime = $2;" setVideoPlaybackPosition :: VideoLayer -> Double -> IO ()
foreign import javascript unsafe "$1.duration" getLengthOfVideo :: VideoLayer -> IO Double


-- A CineCer0Video is a JSVideo (the DOM element) together with some other things we need to keep
-- track of from frame to frame, such as what the style was in the previous frame and whether it is
-- too soon (or not) to set the playback position again.

data CineCer0Video = CineCer0Video {
  videoLayer :: VideoLayer,
  positionLock :: Int,
  previousStyle :: Text,
  previousVol :: Double
  }

data CineCer0Text = CineCer0Text {
  textLayer :: TextLayer,
  positionLockTx :: Int,
  previousStyleTx :: Text
  }


  -- solve thiss!!!!!!!!!!!!!!!!!!!!
addVideo :: HTMLDivElement -> LayerSpec -> IO CineCer0Video
addVideo j os = do
  let url = layerToString $ layer os
  x <- makeVideo url
  muteVideo x True
  appendVideo x j
  return $ CineCer0Video {
    videoLayer = x,
    positionLock = 0,
    previousStyle = "",
    previousVol = 0
  }

addText :: HTMLDivElement -> LayerSpec -> IO CineCer0Text
addText j os = do
  let texto = layerToString $ layer os
  x <- makeText texto
  appendText x j
  return $ CineCer0Text {
    textLayer = x,
    positionLockTx = 0,
    previousStyleTx = ""
  }


layerToString:: Either String String -> Text
layerToString (Right x) = T.pack $ x
layerToString (Left x) = T.pack $ x 


getLayerString:: Either String String -> String
getLayerString (Right x) = x
getLayerString (Left x) = x 

ifEmptyLayer:: Either String String -> Bool
ifEmptyLayer (Right x) = x == ""
ifEmptyLayer (Left x) = x == ""

setVideoRateAndPosition :: CineCer0Video -> Double -> Maybe Double -> Maybe Double -> IO CineCer0Video
setVideoRateAndPosition v vLength (Just r) (Just p) = do
  if ((r >= 0.0625) && (r <= 16)) then do
    let j = videoLayer v
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
    _setVideoStyle (videoLayer v) x
    return $ v { previousStyle = x }

setVideoVol :: CineCer0Video -> Double -> IO CineCer0Video
setVideoVol v x = do
  if previousVol v == x then return v
  else do
    let j = videoLayer v
    muteVideo j $ x == 0
    videoVolume j x
    return $ v { previousVol = x }

setTextStyle :: CineCer0Text -> Text -> IO CineCer0Text
setTextStyle tx x = do
  if previousStyleTx tx == x then return tx
  else do
    _setTextStyle (textLayer tx) x
    return $ tx { previousStyleTx = x }

updateContinuingText:: Tempo -> UTCTime -> UTCTime -> (Double,Double) -> LayerSpec -> CineCer0Text -> IO CineCer0Text
updateContinuingText t eTime rTime (sw,sh) s tx = logExceptions tx $ do
 let j = textLayer tx
 let txw = sw 
 let txh = sh
 putStrLn $ show txw
 putStrLn $ show txh
 if (txw /= 0 && txh /= 0) then do
  let aTime = anchorTime s t eTime
  let lengthOfLayer = 1
  let txFont = (fontFamily s t lengthOfLayer rTime eTime aTime)

  let aspectRatio = txw/txh
  let heightIfFitsWidth = sw / aspectRatio
  let widthIfFitsHeight = sh * aspectRatio
  let fitByWidth = heightIfFitsWidth <= sh
  let fitWidth = if fitByWidth then sw else widthIfFitsHeight
  let fitHeight = if fitByWidth then heightIfFitsWidth else sh
  let actualWidth = (width s t lengthOfLayer rTime eTime aTime) * realToFrac fitWidth
  let actualHeight = (height s t lengthOfLayer rTime eTime aTime) * realToFrac fitHeight
  let centreX = ((posX s t lengthOfLayer rTime eTime aTime)* 0.5 + 0.5) * realToFrac sw
  let centreY = ((posY s t lengthOfLayer rTime eTime aTime)* 0.5 + 0.5) * realToFrac sh
  let leftX = centreX - (actualWidth * 0.5)
  let topY = realToFrac sh - (centreY + (actualHeight * 0.5))

  let txStyle = textStyle (realToFrac $ leftX) (realToFrac $ topY) (realToFrac $ actualWidth) (realToFrac $ actualHeight) (T.pack txFont)
  putStrLn $ T.unpack $  txStyle
  setTextStyle tx $ txStyle

  else return tx
    -- solve the width /height issues first, after that, first test with 
    -- default font size, font family, etc.


updateContinuingVideo :: Tempo -> UTCTime -> UTCTime -> (Double,Double) -> LayerSpec -> CineCer0Video -> IO CineCer0Video
updateContinuingVideo t eTime rTime (sw,sh) s v = logExceptions v $ do
  let j = videoLayer v
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
    let aTime = anchorTime s t eTime
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
generateFilter o bl br c g s = "filter:" <> generateOpacity o <> generateBlur bl <> generateBrightness br <> generateContrast c <> generateGrayscale g <> generateSaturate s <>";"

videoStyle :: Double -> Double -> Double -> Double -> Text -> Text -> Text
videoStyle x y w h f m = "left: " <> showt x <> "px; top: " <> showt y <> "px; position: absolute; width:" <> showt w <> "px; height:" <> showt h <> "px; object-fit: fill;" <> f <> m

textStyle :: Double -> Double -> Double -> Double -> Text -> Text
textStyle x y w h ff = "left: " <> showt x <> "px; top: " <> showt y <> "px; position: absolute; width:" <> showt w <> "px; height:" <> showt h <> "px; font-family:" <> showt ff <> "; object-fit: fill;"


-- these two might become only one!

onlyChangedLayerSources :: LayerSpec -> LayerSpec -> Maybe LayerSpec
onlyChangedLayerSources nSpec oSpec
  | (layer nSpec /= layer oSpec) = Just nSpec
  | (layer nSpec == layer oSpec) = Nothing

  -- this will have to go also
-- onlyChangedTextSources :: LayerSpec -> LayerSpec -> Maybe LayerSpec
-- onlyChangedTextSources nSpec oSpec
--   | (layer nSpec /= layer oSpec) = Just nSpec
--   | (layer nSpec == layer oSpec) = Nothing


-- A CineCer0State represents the entire state corresponding to a CineCer0 program
-- (each statement separated by ; in the program is one element within various )
data CineCer0State = CineCer0State {
  container :: HTMLDivElement,
  videos :: IntMap CineCer0Video,
  previousLayerSpecs :: IntMap LayerSpec,
  texts :: IntMap CineCer0Text,
  previousTextSpecs :: IntMap LayerSpec
  }

  
emptyCineCer0State :: HTMLDivElement -> CineCer0State
emptyCineCer0State j = CineCer0State {
  container = j,
  videos = empty,
  previousLayerSpecs = empty,
  texts = empty,
  previousTextSpecs = empty
  }

textOrVideo:: IntMap LayerSpec -> (IntMap LayerSpec, IntMap LayerSpec)
textOrVideo layerSpecMap = partition (\x -> layerParti (layer x)) layerSpecMap

layerParti:: Either String String -> Bool
layerParti (Right x)= True
layerParti (Left x) = False 

updateCineCer0State :: Tempo -> UTCTime -> Spec -> CineCer0State -> IO CineCer0State
updateCineCer0State t rTime spec st = logExceptions st $ do
  let objSpecs = layerSpecMap spec  -- objectSpec instead of vSpec, this needs to change throughout the whole structure
  let vSpecs = fst $ textOrVideo objSpecs
  let txSpecs = snd $ textOrVideo objSpecs
  let eTime = evalTime spec
  divWidth <- offsetWidth $ container st
  divHeight <- offsetHeight $ container st
  -- add videos
  let newVideoSpecs = difference vSpecs (videos st) -- :: IntMap ObjectSpec
  let toAddv = IntMap.filter (\x -> ifEmptyLayer (layer x) == False) newVideoSpecs -- operation on Layers -- :: IntMap LayerSpec
  addedVideos <- mapM (\x -> addVideo (container st) x) toAddv -- :: IntMap CineCer0Video
  -- add text 
  let newTextSpecs = difference txSpecs (texts st) -- :: IntMap LayerSpec (this changes to LayerSpec, aslo in line 278) 
  let toAddtx = IntMap.filter (\x -> ifEmptyLayer (layer x) == False) newTextSpecs
  addedTexts <- mapM (\x -> addText (container st) x) toAddtx
  -- change videos
  let continuingLayerSpecs = intersectionWith onlyChangedLayerSources vSpecs (previousLayerSpecs st) -- :: IntMap (Maybe LayerSpec)
  let toChangeV = fmapMaybe id continuingLayerSpecs -- :: IntMap LayerSpec
  let toChangeV' = intersectionWith (\a b -> (a,b)) toChangeV $ videos st -- IntMap (LayerSpec,CineCer0Video)
  mapM_ (\(x,cv) -> changeVideoSource (videoLayer cv) $ T.pack (getLayerString (layer x))) toChangeV'
  -- change texts
  let continuingTextSpecs = intersectionWith onlyChangedLayerSources txSpecs (previousTextSpecs st)
  let toChangeTx = fmapMaybe id continuingTextSpecs -- :: IntMap LayerSpec
  let toChangeTx' = intersectionWith (\a b -> (a,b)) toChangeTx $ texts st -- IntMap (LayerSpec,CineCer0Video)
  mapM_ (\(x,cTx) -> changeTextSource (textLayer cTx) $ T.pack (getLayerString (layer x))) toChangeTx'
  -- delete LayerSpecs <- this wilh change to LayerSpecs
  -- delete videos
  let videosWithRemovedSpecs = difference (videos st) vSpecs -- :: IntMap CineCer0Video
  let videosWithEmptySource = intersection (videos st) $ IntMap.filter (\x -> (ifEmptyLayer $ layer x) == True) vSpecs -- :: IntMap CineCer0Video
  let toDeleteV = union videosWithRemovedSpecs videosWithEmptySource
  mapM (\x -> removeVideo (container st) (videoLayer x)) toDeleteV
  let videosThereBefore = difference (videos st) toDeleteV -- :: IntMap CineCer0Video
  -- delete text
  let textsWithRemovedSpecs = difference (texts st) txSpecs -- IntMap CineCer0Text
  let textsWithEmptySource = intersection (texts st) $ IntMap.filter (\x -> (ifEmptyLayer $ layer x) == True) txSpecs -- :: IntMap CineCer0Video
  let toDeleteTx = union textsWithRemovedSpecs textsWithEmptySource
  mapM (\x -> removeText (container st) (textLayer x)) toDeleteTx
  let textsThereBefore = difference (texts st) toDeleteTx -- :: IntMap CineCer0Video 
  -- update cached states
  let continuingVideos = union videosThereBefore addedVideos -- :: IntMap CineCer0Video
  continuingVideos' <- sequence $ intersectionWith (updateContinuingVideo t eTime rTime (divWidth,divHeight)) vSpecs continuingVideos -- :: IntMap CineCer0Video
  let continuingTexts = union textsThereBefore addedTexts
  continuingTexts' <- sequence $ intersectionWith (updateContinuingText t eTime rTime (divWidth,divHeight)) txSpecs continuingTexts
  return $ st { videos = continuingVideos', previousLayerSpecs = txSpecs, texts = continuingTexts', previousTextSpecs = txSpecs }
  
logExceptions :: a -> IO a -> IO a
logExceptions a x = x `catch` (\e -> do
  putStrLn $ "EXCEPTION (CineCer0): " ++ show (e :: SomeException)
  return a
  )
