{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Estuary.Languages.CineCer0.CineCer0State where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Witherable as F
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


newtype VideoLayer = VideoLayer { videoJSVal :: JSVal }--list :: Haskellish st a -> Haskellish st [a]
newtype ImageLayer = ImageLayer { imageJSVal :: JSVal }
newtype TextLayer = TextLayer { textJSVal :: JSVal }
newtype SVGLayer = SVGLayer { svgJSVal :: JSVal }

instance PToJSVal VideoLayer where pToJSVal (VideoLayer x) = x
instance PFromJSVal VideoLayer where pFromJSVal = VideoLayer

instance PToJSVal ImageLayer where pToJSVal (ImageLayer x) = x
instance PFromJSVal ImageLayer where pFromJSVal = ImageLayer

instance PToJSVal TextLayer where pToJSVal (TextLayer x) = x
instance PFromJSVal TextLayer where pFromJSVal = TextLayer

instance PToJSVal SVGLayer where pToJSVal (SVGLayer x) = x
instance PFromJSVal SVGLayer where pFromJSVal = SVGLayer


foreign import javascript unsafe
  "var myObject = document.createElement('object'); myObject.setAttribute('type', 'image/svg+xml'); myObject.setAttribute('data', $1); $r=myObject;"
  makeSVG :: Text -> IO SVGLayer


---- text svg possibility
-- foreign import javascript unsafe
--   "var svgTx = document.createElementNS('http://www.w3.org/2000/svg', 'svg'); svgTx.setAttribute('xlink','http://www.w3.org/1999/xlink'); var text = document.createElementNS('ttp://www.w3.org/2000/svg', 'text'); text.setAttribute('x', '10'); text.setAttribute('y', '20'); text.setAttribute('fill', '#000'); text.textContent = '2666'; $r= svgTx.appendChild(text); ;"
--   makeSVG :: Text -> IO SVGLayer

foreign import javascript unsafe
  "$2.appendChild($1);"
  appendSVG :: SVGLayer -> HTMLDivElement -> IO ()


foreign import javascript unsafe "$1.offsetWidth" offsetWidth :: HTMLDivElement -> IO Double
foreign import javascript unsafe "$1.offsetHeight" offsetHeight :: HTMLDivElement -> IO Double
-- video
foreign import javascript unsafe
  "var video = document.createElement('video'); video.setAttribute('src',$1); $r=video; video.loop = true;"
  makeVideo :: Text -> IO VideoLayer
foreign import javascript unsafe
  "$2.appendChild($1); $1.play();"
  appendVideo :: VideoLayer -> HTMLDivElement -> IO ()
-- image
foreign import javascript unsafe
  "var image = document.createElement('img'); image.src = $1; $r=image;"
  makeImage :: Text -> IO ImageLayer
foreign import javascript unsafe
  "$2.appendChild($1);"
  appendImage :: ImageLayer -> HTMLDivElement -> IO ()
-- text
foreign import javascript unsafe
  "var text = document.createElement('div'); text.innerText  = $1; $r=text;"
  makeText :: Text -> IO TextLayer
foreign import javascript unsafe
  "$2.appendChild($1);"
  appendText :: TextLayer -> HTMLDivElement -> IO ()
-- video
foreign import javascript unsafe "$1.removeChild($2)" removeVideo :: HTMLDivElement -> VideoLayer -> IO ()
foreign import javascript unsafe "$1.style = $2;" _setVideoStyle :: VideoLayer -> Text -> IO ()
foreign import javascript unsafe "$1.muted = $2;" muteVideo :: VideoLayer -> Bool -> IO ()
foreign import javascript unsafe "$1.volume = $2" videoVolume :: VideoLayer -> Double -> IO ()
foreign import javascript unsafe "$1.pause(); $1.src = $2; $1.load(); $1.play();" changeVideoSource :: VideoLayer -> Text -> IO ()
foreign import javascript unsafe "$1.videoWidth" videoWidth :: VideoLayer -> IO Double
foreign import javascript unsafe "$1.videoHeight" videoHeight :: VideoLayer -> IO Double
foreign import javascript unsafe "$1.playbackRate" getVideoPlaybackRate :: VideoLayer -> IO Double
foreign import javascript safe "$1.playbackRate = $2;" setVideoPlaybackRate :: VideoLayer -> Double -> IO ()
foreign import javascript unsafe "$1.currentTime" getVideoPlaybackPosition :: VideoLayer -> IO Double
foreign import javascript unsafe "$1.currentTime = $2;" setVideoPlaybackPosition :: VideoLayer -> Double -> IO ()
foreign import javascript unsafe "$1.duration" getLengthOfVideo :: VideoLayer -> IO Double
-- image
foreign import javascript unsafe "$1.removeChild($2)" removeImage :: HTMLDivElement -> ImageLayer -> IO ()
foreign import javascript unsafe "$1.style = $2;" _setImageStyle :: ImageLayer -> Text -> IO ()
foreign import javascript unsafe "$1.src = $2;" changeImageSource :: ImageLayer -> Text -> IO ()
foreign import javascript unsafe "$1.width" imageWidth :: ImageLayer -> IO Double
foreign import javascript unsafe "$1.height" imageHeight :: ImageLayer -> IO Double
-- text
foreign import javascript unsafe "$1.removeChild($2)" removeText :: HTMLDivElement -> TextLayer -> IO ()
foreign import javascript unsafe "$1.style = $2;" _setTextStyle :: TextLayer -> Text -> IO ()
foreign import javascript unsafe "$1.textContent = $2;" changeTextSource :: TextLayer -> Text -> IO ()
foreign import javascript unsafe "$1.offsetWidth" textWidth :: TextLayer -> IO Double
foreign import javascript unsafe "$1.offsetHeight" textHeight :: TextLayer -> IO Double
-- SVG
foreign import javascript unsafe "$1.removeChild($2)" removeSVG :: HTMLDivElement -> SVGLayer -> IO ()
foreign import javascript unsafe "$1.style = $2;" _setSVGStyle :: SVGLayer -> Text -> IO ()
foreign import javascript unsafe "$1.textContent = $2;" changeSVGSource :: SVGLayer -> Text -> IO ()
foreign import javascript unsafe "$1.offsetWidth" svgWidth :: SVGLayer -> IO Double
foreign import javascript unsafe "$1.offsetHeight" svgHeight :: SVGLayer -> IO Double


data CineCer0Video = CineCer0Video {
  videoLayer :: VideoLayer,
  positionLock :: Int,
  previousStyle :: Text,
  previousVol :: Double
  }

data CineCer0Image = CineCer0Image {
  imageLayer :: ImageLayer,
  positionLockImg :: Int,
  previousStyleImg :: Text
  }

data CineCer0Text = CineCer0Text {
  textLayer :: TextLayer,
  positionLockTx :: Int,
  previousStyleTx :: Text
  }

data CineCer0SVG = CineCer0SVG {
  svgLayer :: SVGLayer,
  positionLockSVG :: Int,
  previousStyleSVG :: Text
  }

addVideo :: HTMLDivElement -> LayerSpec -> IO CineCer0Video
addVideo j os = do
  let url = sourceToText $ source os
  x <- makeVideo url
  muteVideo x True
  appendVideo x j
  return $ CineCer0Video {
    videoLayer = x,
    positionLock = 0,
    previousStyle = "",
    previousVol = 0
  }

addImage :: HTMLDivElement -> LayerSpec -> IO CineCer0Image
addImage j os = do
  let url = sourceToText $ source os
  x <- makeImage url
  appendImage x j
  return $ CineCer0Image {
    imageLayer = x,
    positionLockImg = 0,
    previousStyleImg = ""
 }

addText :: HTMLDivElement -> LayerSpec -> IO CineCer0Text
addText j os = do
  let texto = sourceToText $ source os
  x <- makeText texto
  appendText x j
  return $ CineCer0Text {
    textLayer = x,
    positionLockTx = 0,
    previousStyleTx = ""
  }

addSVG :: HTMLDivElement -> LayerSpec -> IO CineCer0SVG
addSVG j os = do
  let svg = sourceToText $ source os
  x <- makeSVG svg
  appendSVG x j
  return $ CineCer0SVG {
    svgLayer = x,
    positionLockSVG = 0,
    previousStyleSVG = ""
  }

sourceToText :: Source -> Text
sourceToText (VideoSource x) = x
sourceToText (ImageSource x) = x
sourceToText (TextSource x) = x
sourceToText (SVGSource x) = x


-- addInvisibleText :: HTMLDivElement -> LayerSpec -> IO CineCer0Text
-- addInvisibleText j ls = do
--   cct <- addText j ls
--   let inv = invisibleText cct
--   return inv
--
-- invisibleText :: CineCer0Text -> IO CineCer0Text
-- invisibleText tx = do
--     _setTextStyle (textLayer tx) "visibility: hidden;"
--     return $ tx { previousStyleTx = "" }


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

setImageStyle :: CineCer0Image -> Text -> IO CineCer0Image
setImageStyle img x = do
  if previousStyleImg img == x then return img
  else do
    _setImageStyle (imageLayer img) x
    return $ img { previousStyleImg = x }

setTextStyle :: CineCer0Text -> Text -> IO CineCer0Text
setTextStyle tx x = do
  if previousStyleTx tx == x then return tx
  else do
    _setTextStyle (textLayer tx) x
    return $ tx { previousStyleTx = x }

setSVGStyle :: CineCer0SVG -> Text -> IO CineCer0SVG
setSVGStyle svg x = do
  if previousStyleSVG svg == x then return svg
  else do
    _setSVGStyle (svgLayer svg) x
    return $ svg { previousStyleSVG = x }

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
    let normVol = if (volume s t lengthOfVideo rTime eTime aTime) > 1 then 1 else (volume s t lengthOfVideo rTime eTime aTime)
    v'' <- setVideoVol v' $ realToFrac normVol

    let z' = generateZIndex (z s t lengthOfVideo rTime eTime aTime)
    let r' = rotate s t lengthOfVideo rTime eTime aTime
    let rotateText = generateRotate (ceiling r')
    let opacity' = (*) <$> (opacity s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let blur' = blur s t lengthOfVideo rTime eTime aTime
    let brightness' = (*) <$> (brightness s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let contrast' = (*) <$> (contrast s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let grayscale' = (*) <$> (grayscale s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let saturate' = (*) <$> (saturate s) t lengthOfVideo rTime eTime aTime <*> Just 100
    let filterText = generateFilter (fmap realToFrac opacity') (fmap realToFrac blur') (fmap realToFrac brightness') (fmap realToFrac contrast') (fmap realToFrac grayscale') (fmap realToFrac saturate')
    let mask' = ((Cinecer0.mask s) t lengthOfVideo rTime eTime aTime)

    setVideoStyle v'' $ videoStyle (realToFrac $ leftX) (realToFrac $ topY) (realToFrac $ actualWidth) (realToFrac $ actualHeight) filterText rotateText mask' z'
  else return v


updateContinuingImage :: Tempo -> UTCTime -> UTCTime -> (Double,Double) -> LayerSpec -> CineCer0Image -> IO CineCer0Image
updateContinuingImage t eTime rTime (sw,sh) s img = logExceptions img $ do
  let j = imageLayer img
  iw <- imageWidth j
  ih <- imageHeight j

  if (iw /= 0 && ih /= 0) then do
    let lengthOfImage = 1
    let aspectRatio = iw/ih
    let heightIfFitsWidth = sw / aspectRatio
    let widthIfFitsHeight = sh * aspectRatio
    let fitByWidth = heightIfFitsWidth <= sh
    let fitWidth = if fitByWidth then sw else widthIfFitsHeight
    let fitHeight = if fitByWidth then heightIfFitsWidth else sh
    let aTime = anchorTime s t eTime
    let actualWidth = (width s t lengthOfImage rTime eTime aTime) * realToFrac fitWidth
    let actualHeight = (height s t lengthOfImage rTime eTime aTime) * realToFrac fitHeight
    let centreX = ((posX s t lengthOfImage rTime eTime aTime)* 0.5 + 0.5) * realToFrac sw
    let centreY = ((posY s t lengthOfImage rTime eTime aTime)* 0.5 + 0.5) * realToFrac sh
    let leftX = centreX - (actualWidth * 0.5)
    let topY = realToFrac sh - (centreY + (actualHeight * 0.5))

    let z' = generateZIndex (z s t lengthOfImage rTime eTime aTime)
    let r' = rotate s t lengthOfImage rTime eTime aTime
    let rotateText = generateRotate (ceiling r')
    let opacity' = (*) <$> (opacity s) t lengthOfImage rTime eTime aTime <*> Just 100
    let blur' = blur s t lengthOfImage rTime eTime aTime
    let brightness' = (*) <$> (brightness s) t lengthOfImage rTime eTime aTime <*> Just 100
    let contrast' = (*) <$> (contrast s) t lengthOfImage rTime eTime aTime <*> Just 100
    let grayscale' = (*) <$> (grayscale s) t lengthOfImage rTime eTime aTime <*> Just 100
    let saturate' = (*) <$> (saturate s) t lengthOfImage rTime eTime aTime <*> Just 100
    let filterText = generateFilter (fmap realToFrac opacity') (fmap realToFrac blur') (fmap realToFrac brightness') (fmap realToFrac contrast') (fmap realToFrac grayscale') (fmap realToFrac saturate')
    let mask' = ((Cinecer0.mask s) t lengthOfImage rTime eTime aTime)

    let imgStyle = imageStyle (realToFrac $ leftX) (realToFrac $ topY) (realToFrac $ actualWidth) (realToFrac $ actualHeight) filterText rotateText mask' z'
    setImageStyle img $ imgStyle
  else return img


updateContinuingText :: Tempo -> UTCTime -> UTCTime -> (Double,Double) -> LayerSpec -> CineCer0Text -> IO CineCer0Text
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

  let txStyle = textStyle (realToFrac $ leftX) (realToFrac $ topY) (realToFrac $ actualWidth) (realToFrac $ actualHeight) (txFont) striked bolded italicised bordered coloured sized z'
  -- putStrLn $ T.unpack $ txStyle -- debugging line
  setTextStyle tx $ txStyle
  else return tx

updateContinuingSVG :: Tempo -> UTCTime -> UTCTime -> (Double,Double) -> LayerSpec -> CineCer0SVG -> IO CineCer0SVG
updateContinuingSVG t eTime rTime (sw,sh) s svg = logExceptions svg $ do
  let j = svgLayer svg
  vw <- svgWidth j
  vh <- svgHeight j

  if (vw /= 0 && vh /= 0) then do
    let length = 10
    let aspectRatio = vw/vh
    let heightIfFitsWidth = sw / aspectRatio
    let widthIfFitsHeight = sh * aspectRatio
    let fitByWidth = heightIfFitsWidth <= sh
    let fitWidth = if fitByWidth then sw else widthIfFitsHeight
    let fitHeight = if fitByWidth then heightIfFitsWidth else sh
    let aTime = anchorTime s t eTime
    let actualWidth = (width s t length rTime eTime aTime) * realToFrac fitWidth
    let actualHeight = (height s t length rTime eTime aTime) * realToFrac fitHeight
    let centreX = ((posX s t length rTime eTime aTime)* 0.5 + 0.5) * realToFrac sw
    let centreY = ((posY s t length rTime eTime aTime)* 0.5 + 0.5) * realToFrac sh
    let leftX = centreX - (actualWidth * 0.5)
    let topY = realToFrac sh - (centreY + (actualHeight * 0.5))
    let mask' = ((Cinecer0.mask s) t length rTime eTime aTime)

    let z' = generateZIndex (z s t length rTime eTime aTime)

    setSVGStyle svg $ svgStyle (realToFrac $ leftX) (realToFrac $ topY) (realToFrac $ actualWidth) (realToFrac $ actualHeight) mask' z'
  else return svg


svgStyle :: Double -> Double -> Double -> Double -> Text -> Text -> Text
svgStyle x y w h m z = "left: " <> showt x <> "px; top: " <> showt y <> "px; position: absolute; width:" <> showt (w) <> "px; height:" <> showt (h) <> "px; object-fit: fill;" <> m <> z


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

generateRotate :: Int -> Text
generateRotate r = "; transform: rotate(" <> showt r <> "deg);"

videoStyle :: Double -> Double -> Double -> Double -> Text -> Text -> Text -> Text -> Text
videoStyle x y w h f r m z = "left: " <> showt x <> "px; top: " <> showt y <> "px; position: absolute; width:" <> showt (w) <> "px; height:" <> showt (h) <> "px; object-fit: fill;" <> f <> r <> m <> z

imageStyle :: Double -> Double -> Double -> Double -> Text -> Text -> Text -> Text -> Text
imageStyle x y w h f r m z = "left: " <> showt x <> "px; top: " <> showt y <> "px; position: absolute; width:" <> showt (w) <> "px; height:" <> showt (h) <> "px; object-fit: fill;" <> f <> r <> m <> z

generateZIndex :: Int -> Text
generateZIndex n = "; z-index: " <> T.pack (show n) <> ";"

generateFontSize :: Double -> Text
generateFontSize size = "; font-size: " <> T.pack (show (size)) <> "em;"

generateColours:: Colour -> Tempo -> NominalDiffTime -> UTCTime -> UTCTime -> UTCTime -> Text
generateColours (Colour str) t ll rT eT aT = "; color: " <> string <> ";"
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
  | (source nSpec /= source oSpec) = Just nSpec
  | (source nSpec == source oSpec) = Nothing

data CineCer0State = CineCer0State {
  container :: HTMLDivElement,
  videos :: IntMap CineCer0Video,
  previousLayerSpecs :: IntMap LayerSpec,
  images :: IntMap CineCer0Image,
  previousImageSpecs :: IntMap LayerSpec,
  texts :: IntMap CineCer0Text,
  previousTextSpecs :: IntMap LayerSpec,
  svgs :: IntMap CineCer0SVG,
  previousSVGSpecs :: IntMap LayerSpec
  }

emptyCineCer0State :: HTMLDivElement -> CineCer0State
emptyCineCer0State j = CineCer0State {
  container = j,
  videos = empty,
  previousLayerSpecs = empty,
  images = empty,
  previousImageSpecs = empty,
  texts = empty,
  previousTextSpecs = empty,
  svgs = empty,
  previousSVGSpecs = empty
  }

isVideo :: IntMap LayerSpec -> IntMap LayerSpec
isVideo layerSpecMap = IntMap.filter (\x -> getVideo (source x) == True) layerSpecMap

isImage :: IntMap LayerSpec -> IntMap LayerSpec
isImage layerSpecMap = IntMap.filter (\x -> getImage (source x) == True) layerSpecMap

isText :: IntMap LayerSpec -> IntMap LayerSpec
isText layerSpecMap = IntMap.filter (\x -> getText (source x) == True) layerSpecMap

isSVG :: IntMap LayerSpec -> IntMap LayerSpec
isSVG layerSpecMap = IntMap.filter (\x -> getSVG (source x) == True) layerSpecMap

getVideo :: Source -> Bool
getVideo (VideoSource x) = True
getVideo _ = False

getImage :: Source -> Bool
getImage (ImageSource x) = True
getImage _ = False

getText :: Source -> Bool
getText (TextSource x) = True
getText _ = False

getSVG :: Source -> Bool
getSVG (SVGSource x) = True
getSVG _ = False

getLayerText :: Source -> Text
getLayerText (VideoSource x) = x
getLayerText (ImageSource x) = x
getLayerText (TextSource x) = x
getLayerText (SVGSource x) = x

ifEmptyLayer :: Source -> Bool
ifEmptyLayer (VideoSource x) = x == ""
ifEmptyLayer (ImageSource x) = x == ""
ifEmptyLayer (TextSource x) = x == ""
ifEmptyLayer (SVGSource x) = x == ""


deleteCineCer0State :: CineCer0State -> IO ()
deleteCineCer0State st = do
  mapM_ ((removeVideo $ container st) . videoLayer) $ videos st
  mapM_ ((removeImage $ container st) . imageLayer) $ images st
  mapM_ ((removeText $ container st) . textLayer) $ texts st
  mapM_ ((removeSVG $ container st) . svgLayer) $ svgs st

-- filter :: (a -> Bool) -> f a -> f a


updateCineCer0State :: Tempo -> UTCTime -> Spec -> CineCer0State -> IO CineCer0State
updateCineCer0State t rTime spec st = logExceptions st $ do
  let objSpecs = layerSpecMap spec
  let vSpecs = isVideo objSpecs
  let imgSpecs = isImage objSpecs
  let txSpecs = isText objSpecs
  let svgSpecs = isSVG objSpecs
  let eTime = evalTime spec
  divWidth <- offsetWidth $ container st
  divHeight <- offsetHeight $ container st

  -- add videos
  let newVideoSpecs = difference vSpecs (videos st) -- :: IntMap LayerSpec
  let toAddv = IntMap.filter (\x -> ifEmptyLayer (source x) == False) newVideoSpecs -- operation on Layers -- :: IntMap LayerSpec
  addedVideos <- mapM (\x -> addVideo (container st) x) toAddv -- :: IntMap CineCer0Video
  -- add image
  let newImageSpecs = difference imgSpecs (images st) -- :: IntMap LayerSpec
  let toAddimg = IntMap.filter (\x -> ifEmptyLayer (source x) == False) newImageSpecs
  addedImages <- mapM (\x -> addImage (container st) x) toAddimg
  -- add text
  let newTextSpecs = difference txSpecs (texts st) -- :: IntMap LayerSpec
  let toAddtx = IntMap.filter (\x -> ifEmptyLayer (source x) == False) newTextSpecs -- answer false to is the source empty?
  addedTexts <- mapM (\x -> addText (container st) x) toAddtx
  -- add SVG
  let newSVGSpecs = difference svgSpecs (svgs st) -- :: IntMap LayerSpec
  let toAddsvg = IntMap.filter (\x -> ifEmptyLayer (source x) == False) newSVGSpecs -- answer false to is the source empty?
  addedSVGs <- mapM (\x -> addSVG (container st) x) toAddsvg

  -- change videos
  let continuingLayerSpecs = intersectionWith onlyChangedLayerSources vSpecs (previousLayerSpecs st) -- :: IntMap (Maybe LayerSpec)
  let toChangeV = F.mapMaybe id continuingLayerSpecs -- :: IntMap LayerSpec
  let toChangeV' = intersectionWith (\a b -> (a,b)) toChangeV $ videos st -- IntMap (LayerSpec,CineCer0Video)
  mapM_ (\(x,cv) -> changeVideoSource (videoLayer cv) $ (getLayerText (source x))) toChangeV'
  -- change images
  let continuingImageSpecs = intersectionWith onlyChangedLayerSources imgSpecs (previousImageSpecs st)
  let toChangeImg = F.mapMaybe id continuingImageSpecs -- :: IntMap LayerSpec
  let toChangeImg' = intersectionWith (\a b -> (a,b)) toChangeImg $ images st -- IntMap (LayerSpec,CineCer0Image)
  mapM_ (\(x,cImg) -> changeImageSource (imageLayer cImg) $ (getLayerText (source x))) toChangeImg'
  -- change texts
  let continuingTextSpecs = intersectionWith onlyChangedLayerSources txSpecs (previousTextSpecs st)
  let toChangeTx = F.mapMaybe id continuingTextSpecs -- :: IntMap LayerSpec
  let toChangeTx' = intersectionWith (\a b -> (a,b)) toChangeTx $ texts st -- IntMap (LayerSpec,CineCer0Text)
  mapM_ (\(x,cTx) -> changeTextSource (textLayer cTx) $ (getLayerText (source x))) toChangeTx'
  -- change svgs
  let continuingSVGSpecs = intersectionWith onlyChangedLayerSources svgSpecs (previousSVGSpecs st)
  let toChangeSVG = F.mapMaybe id continuingSVGSpecs -- :: IntMap LayerSpec
  let toChangeSVG' = intersectionWith (\a b -> (a,b)) toChangeSVG $ svgs st -- IntMap (LayerSpec,CineCer0Text)
  mapM_ (\(x,cSVG) -> changeSVGSource (svgLayer cSVG) $ (getLayerText (source x))) toChangeSVG'

  -- delete videos
  let videosWithRemovedSpecs = difference (videos st) vSpecs -- :: IntMap CineCer0Video
  let videosWithEmptySource = intersection (videos st) $ IntMap.filter (\x -> (ifEmptyLayer $ source x) == True) vSpecs -- :: IntMap CineCer0Video
  let toDeleteV = union videosWithRemovedSpecs videosWithEmptySource
  mapM (\x -> removeVideo (container st) (videoLayer x)) toDeleteV
  let videosThereBefore = difference (videos st) toDeleteV -- :: IntMap CineCer0Video
  -- delete image
  let imagesWithRemovedSpecs = difference (images st) imgSpecs -- IntMap CineCer0Image
  let imagesWithEmptySource = intersection (images st) $ IntMap.filter (\x -> (ifEmptyLayer $ source x) == True) imgSpecs -- :: IntMap CineCer0Video
  let toDeleteImg = union imagesWithRemovedSpecs imagesWithEmptySource
  mapM (\x -> removeImage (container st) (imageLayer x)) toDeleteImg
  let imagesThereBefore = difference (images st) toDeleteImg -- :: IntMap CineCer0Image
  -- delete text
  let textsWithRemovedSpecs = difference (texts st) txSpecs -- IntMap CineCer0Text
  let textsWithEmptySource = intersection (texts st) $ IntMap.filter (\x -> (ifEmptyLayer $ source x) == True) txSpecs -- :: IntMap CineCer0Video
  let toDeleteTx = union textsWithRemovedSpecs textsWithEmptySource
  mapM (\x -> removeText (container st) (textLayer x)) toDeleteTx
  let textsThereBefore = difference (texts st) toDeleteTx -- :: IntMap CineCer0Text
  -- delete SVG
  let svgsWithRemovedSpecs = difference (svgs st) svgSpecs -- IntMap CineCer0Text
  let svgsWithEmptySource = intersection (svgs st) $ IntMap.filter (\x -> (ifEmptyLayer $ source x) == True) svgSpecs -- :: IntMap CineCer0Video
  let toDeleteSVG = union svgsWithRemovedSpecs svgsWithEmptySource
  mapM (\x -> removeSVG (container st) (svgLayer x)) toDeleteSVG
  let svgsThereBefore = difference (svgs st) toDeleteSVG -- :: IntMap CineCer0Text

  -- update cached states
  -- video
  let continuingVideos = union videosThereBefore addedVideos -- :: IntMap CineCer0Video
  continuingVideos' <- sequence $ intersectionWith (updateContinuingVideo t eTime rTime (divWidth,divHeight)) vSpecs continuingVideos -- :: IntMap CineCer0Video
  -- image
  let continuingImages = union imagesThereBefore addedImages -- :: IntMap CineCer0Image
  continuingImages' <- sequence $ intersectionWith (updateContinuingImage t eTime rTime (divWidth,divHeight)) imgSpecs continuingImages
  -- text
  let continuingTexts = union textsThereBefore addedTexts
  continuingTexts' <- sequence $ intersectionWith (updateContinuingText t eTime rTime (divWidth,divHeight)) txSpecs continuingTexts
  -- SVG
  let continuingSVGs = union svgsThereBefore addedSVGs
  continuingSVGs' <- sequence $ intersectionWith (updateContinuingSVG t eTime rTime (divWidth,divHeight)) svgSpecs continuingSVGs


  return $ st { videos = continuingVideos', previousLayerSpecs = vSpecs, images = continuingImages', previousImageSpecs = imgSpecs, texts = continuingTexts', previousTextSpecs = txSpecs, svgs = continuingSVGs', previousSVGSpecs = svgSpecs }


logExceptions :: a -> IO a -> IO a
logExceptions a x = x `catch` (\e -> do
  putStrLn $ "EXCEPTION (CineCer0): " ++ show (e :: SomeException)
  return a
  )
