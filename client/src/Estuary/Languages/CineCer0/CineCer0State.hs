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
foreign import javascript unsafe "$1.textContent = $2;" changeTextSource :: TextLayer -> Text -> IO ()
foreign import javascript unsafe "$1.videoWidth" videoWidth :: VideoLayer -> IO Double
foreign import javascript unsafe "$1.videoHeight" videoHeight :: VideoLayer -> IO Double
foreign import javascript unsafe "$1.offsetWidth" textWidth :: TextLayer -> IO Double
foreign import javascript unsafe "$1.offsetHeight" textHeight :: TextLayer -> IO Double

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

-- addInvisibleText :: HTMLDivElement -> LayerSpec -> IO CineCer0Text
-- addInvisibleText j ls = do
--   cct <- addText j ls
--   let inv = invisibleText cct
--   return inv

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

-- invisibleText :: CineCer0Text -> IO CineCer0Text
-- invisibleText tx = do
--     _setTextStyle (textLayer tx) "visibility: hidden;"
--     return $ tx { previousStyleTx = "" }

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
        -- putStrLn $ "currentPos=" ++ show currentPos ++ "  target="++show p ++ "  diff=" ++ show (currentPos - p) -- debugging postition
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
 tw <- textWidth j
 th <- textHeight j

-- if (tw /= 0) then (putStrLn "WIIIDTHHHH!!!!") else (putStrLn "no width yet")

 -- putStrLn $ show (T.split (== ' ') $ layerToString (layer s))
 -- putStrLn $ (show $ tw) <> " width"
 -- putStrLn $ (show $ th) <> " height"
 -- putStrLn $ (show $ sw) <> " div width"
 -- putStrLn $ (show $ sh) <> " div height"
 -- putStrLn $ (show $ xPos) <> " calculated x pos"

 if (tw /= 0 && th /= 0) then do
  let aTime = anchorTime s t eTime
  let lengthOfLayer = 1  -- think about how to get a length for a text
  let txFont = (fontFamily s t lengthOfLayer rTime eTime aTime)
  let striked = generateStrike (strike s t lengthOfLayer rTime eTime aTime)
  let bolded = generateBold (bold s t lengthOfLayer rTime eTime aTime)
  let italicised = generateItalic (italic s t lengthOfLayer rTime eTime aTime)
  let bordered = generateBorder (border s t lengthOfLayer rTime eTime aTime)
  let coloured = generateColours (colour s) t lengthOfLayer rTime eTime aTime
  let sized = generateFontSize (realToFrac $ (height s t lengthOfLayer rTime eTime aTime))


  let z' = generateZIndex (z s t lengthOfLayer rTime eTime aTime)

  let aspectRatio = sw/sh
  let heightIfFitsWidth = sw / aspectRatio
  let widthIfFitsHeight = sh * aspectRatio
  let fitByWidth = heightIfFitsWidth <= sh
  let fitWidth = if fitByWidth then sw else widthIfFitsHeight
  let fitHeight = if fitByWidth then heightIfFitsWidth else sh
  let actualWidth = (width s t lengthOfLayer rTime eTime aTime) * realToFrac fitWidth
  let actualHeight = (height s t lengthOfLayer rTime eTime aTime) * realToFrac fitHeight
  let centreX = ((posX s t lengthOfLayer rTime eTime aTime)* 0.5 + 0.5) * realToFrac sw
  let centreY = ((posY s t lengthOfLayer rTime eTime aTime)* 0.5 + 0.5) * realToFrac sh
  --let leftX = centreX - (actualWidth * 0.5)
  --let leftX = xPos
  --let topY = realToFrac sh - (centreY + (actualHeight * 0.5))
  --let topY = yPos
  let x = realToFrac $ (posX s t lengthOfLayer rTime eTime aTime)
  let y = realToFrac $ (posY s t lengthOfLayer rTime eTime aTime)

   -- calculate xPos
  let xPos' = sw - tw
  let xPos  = xPos' * (0.5 + (x*0.5))
  let leftX = xPos
 -- calculate yPos
  let yPos' = sh - th
  let yPos  = yPos' * (0.5 + (y*(-0.5)))
  let topY = yPos

  let txStyle = textStyle (realToFrac $ leftX) (realToFrac $ topY) (realToFrac $ actualWidth) (realToFrac $ actualHeight) (T.pack txFont) striked bolded italicised bordered coloured sized z'
  -- putStrLn $ T.unpack $ txStyle -- debugging line
  setTextStyle tx $ txStyle
  else return tx

updateContinuingVideo :: Tempo -> UTCTime -> UTCTime -> (Double,Double) -> LayerSpec -> CineCer0Video -> IO CineCer0Video
updateContinuingVideo t eTime rTime (sw,sh) s v = logExceptions v $ do
  let j = videoLayer v
  vw <- videoWidth j
  vh <- videoHeight j

--  putStrLn $ show $ vw

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
    let normVol = if (volume s t lengthOfVideo rTime eTime aTime) > 1 then 1 else (volume s t lengthOfVideo rTime eTime aTime)
    v'' <- setVideoVol v' $ realToFrac normVol

    -- z index
    let z' = generateZIndex (z s t lengthOfVideo rTime eTime aTime)

    -- update style (size, position, opacity, etc)
    let opacity' = (*) <$> (opacity s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let blur' = blur s t lengthOfVideo rTime eTime aTime
    let brightness' = (*) <$> (brightness s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let contrast' = (*) <$> (contrast s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let grayscale' = (*) <$> (grayscale s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let saturate' = (*) <$> (saturate s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let filterText = generateFilter (fmap realToFrac opacity') (fmap realToFrac blur') (fmap realToFrac brightness') (fmap realToFrac contrast') (fmap realToFrac grayscale') (fmap realToFrac saturate')
    let mask' = ((Cinecer0.mask s) t lengthOfVideo rTime eTime aTime)
    setVideoStyle v'' $ videoStyle (realToFrac $ leftX) (realToFrac $ topY) (realToFrac $ actualWidth) (realToFrac $ actualHeight) filterText mask' z'

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

videoStyle :: Double -> Double -> Double -> Double -> Text -> Text -> Text -> Text
videoStyle x y w h f m z = "left: " <> showt x <> "px; top: " <> showt y <> "px; position: absolute; width:" <> showt (w) <> "px; height:" <> showt (h) <> "px; object-fit: fill;" <> f <> m <> z


generateZIndex :: Int -> Text
generateZIndex n = "; z-index: " <> T.pack (show n) <> ";"

generateFontSize :: Double -> Text
generateFontSize size = "; font-size: " <> T.pack (show (size)) <> "em;"

generateColours:: Colour -> Tempo -> NominalDiffTime -> UTCTime -> UTCTime -> UTCTime -> Text  -- this string needs to be a text!!!!
generateColours (Colour str) t ll rT eT aT = "; color: " <> T.pack (string) <> ";"
  where string = (str t ll rT eT aT)
generateColours (ColourRGB r g b) t ll rT eT aT = "; color: rgb(" <> (showt red) <> "," <> (showt green) <> "," <> (showt blue) <> ");"
  where red = realToFrac ((r * 255) t ll rT eT aT) :: Double
        green = realToFrac ((g * 255) t ll rT eT aT) :: Double
        blue = realToFrac ((b * 255) t ll rT eT aT) :: Double
generateColours (ColourHSL h s l) t ll rT eT aT = "; color: hsl(" <> (showt hue) <> "," <> (showt saturation) <> "% ," <> (showt lightness) <> "% );"
  where hue = realToFrac ((h * 360) t ll rT eT aT) :: Double
        saturation = realToFrac ((s * 100) t ll rT eT aT) :: Double
        lightness = realToFrac ((l * 100) t ll rT eT aT) :: Double
generateColours (ColourRGBA r g b a) t ll rT eT aT = "; color: rgba(" <> (showt red) <> "," <> (showt green) <> "," <> (showt blue) <> "," <> (showt alpha) <> ");"
  where red = realToFrac ((r * 255) t ll rT eT aT) :: Double
        green = realToFrac ((g * 255) t ll rT eT aT) :: Double
        blue = realToFrac ((b * 255) t ll rT eT aT) :: Double
        alpha = realToFrac (a t ll rT eT aT) :: Double
generateColours (ColourHSLA h s l a) t ll rT eT aT = "; color: hsla(" <> (showt hue) <> "," <> (showt saturation) <> "% ," <> (showt lightness) <> "% ," <> (showt alpha) <> ");"
  where hue = realToFrac ((h * 360) t ll rT eT aT) :: Double
        saturation = realToFrac ((s * 100) t ll rT eT aT) :: Double
        lightness = realToFrac ((l * 100) t ll rT eT aT) :: Double
        alpha = realToFrac (a t ll rT eT aT) :: Double

generateStrike :: Bool -> Text
generateStrike (True) = "; text-decoration: line-through;"
generateStrike (False) = ""

generateBold :: Bool -> Text
generateBold (True) = "; font-weight: bold;"
generateBold (False) = ""

generateItalic :: Bool -> Text
generateItalic (True) = "; font-style: italic;"
generateItalic (False) = ""

generateBorder :: Bool -> Text
generateBorder (True) = "; border: 1px solid #cccccc;"
generateBorder (False) = ""

textStyle :: Double -> Double -> Double -> Double -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text
textStyle x y w h fontfam stk bld itc border colour size z = "position: absolute;" <> "left: " <> showt (x-1) <> "px; top: " <> showt y <> "px;" <> "text-align: center;" <> "font-family:" <> showt fontfam <> stk <> bld <> itc <> border <> colour <> size <> z <> ";"


-- textStyle :: Double -> Double -> Double -> Double -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text
-- textStyle x y w h ff stk bld itc clr sz z = "left: " <> showt x <> "px; top: " <> showt y <> "px; border: 1px solid #cccccc; position: absolute; translate(-50%, -50%); text-align: center; width: " <> showt (w) <> "px; height:" <> showt (h) <> "px; font-family:" <> showt ff <> stk <> bld <> itc <> clr <> sz <> z <> ";"


-- these two might become only one!

onlyChangedLayerSources :: LayerSpec -> LayerSpec -> Maybe LayerSpec
onlyChangedLayerSources nSpec oSpec
  | (layer nSpec /= layer oSpec) = Just nSpec
  | (layer nSpec == layer oSpec) = Nothing

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


-- used when a CineCer0 program has been removed from the situation
-- (the CineCer0State record is no longer usable after this action and should be discarded)
deleteCineCer0State :: CineCer0State -> IO ()
deleteCineCer0State st = do
  mapM_ ((removeVideo $ container st) . videoLayer) $ videos st
  mapM_ ((removeText $ container st) . textLayer) $ texts st


updateCineCer0State :: Tempo -> UTCTime -> Spec -> CineCer0State -> IO CineCer0State
updateCineCer0State t rTime spec st = logExceptions st $ do
  let objSpecs = layerSpecMap spec
  let vSpecs = fst $ textOrVideo objSpecs
  let txSpecs = snd $ textOrVideo objSpecs
  let eTime = evalTime spec
  divWidth <- offsetWidth $ container st
  divHeight <- offsetHeight $ container st
  -- add videos
  let newVideoSpecs = difference vSpecs (videos st) -- :: IntMap LayerSpec
  let toAddv = IntMap.filter (\x -> ifEmptyLayer (layer x) == False) newVideoSpecs -- operation on Layers -- :: IntMap LayerSpec
  addedVideos <- mapM (\x -> addVideo (container st) x) toAddv -- :: IntMap CineCer0Video
  -- add text
  let newTextSpecs = difference txSpecs (texts st) -- :: IntMap LayerSpec (this changes to LayerSpec, aslo in line 278)
  let toAddtx = IntMap.filter (\x -> ifEmptyLayer (layer x) == False) newTextSpecs -- answer false to is the layer empty?


  -- function to process text in time -- :: Tempo -> rTime -> evalTime -> st
 -- let toAddSubTx = func t rTime eTime textSpecs
-- splitting the text, tuplets: (index, subtx), depending on index compared with a module of the render time the tx is added or not.

  addedTexts <- mapM (\x -> addText (container st) x) toAddtx
  -- change videos
  let continuingLayerSpecs = intersectionWith onlyChangedLayerSources vSpecs (previousLayerSpecs st) -- :: IntMap (Maybe LayerSpec)
  let toChangeV = fmapMaybe id continuingLayerSpecs -- :: IntMap LayerSpec
  let toChangeV' = intersectionWith (\a b -> (a,b)) toChangeV $ videos st -- IntMap (LayerSpec,CineCer0Video)
  mapM_ (\(x,cv) -> changeVideoSource (videoLayer cv) $ T.pack (getLayerString (layer x))) toChangeV'
  -- change texts
  let continuingTextSpecs = intersectionWith onlyChangedLayerSources txSpecs (previousTextSpecs st)
  let toChangeTx = fmapMaybe id continuingTextSpecs -- :: IntMap LayerSpec
  let toChangeTx' = intersectionWith (\a b -> (a,b)) toChangeTx $ texts st -- IntMap (LayerSpec,CineCer0Text)
  mapM_ (\(x,cTx) -> changeTextSource (textLayer cTx) $ T.pack (getLayerString (layer x))) toChangeTx'
  -- delete LayerSpecs
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
  return $ st { videos = continuingVideos', previousLayerSpecs = vSpecs, texts = continuingTexts', previousTextSpecs = txSpecs }


logExceptions :: a -> IO a -> IO a
logExceptions a x = x `catch` (\e -> do
  putStrLn $ "EXCEPTION (CineCer0): " ++ show (e :: SomeException)
  return a
  )
