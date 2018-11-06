{-# LANGUAGE OverloadedStrings, JavaScriptFFI #-}

module Estuary.Widgets.Plot where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM
import qualified GHCJS.DOM.Types as DT
import Data.Sequence (fromList, update)
import Data.Foldable (toList)
import Control.Monad
import Control.Monad.IO.Class
import Data.JSString(JSString)

import Text.Read (readMaybe)

import Estuary.Tidal.Types
import Estuary.Types.Live
import Estuary.Types.Hint
import Estuary.Widgets.Generic -- for


plot :: MonadWidget t m => [Double] -> Double -> Double  -> Event t (EditSignal [Double]) -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal (GeneralPattern Double)), Event t Hint))
plot ivals low high update = elClass "div" "plot" $ do
  let steps = 100
  mx <- el "div" $ do
    text "max: "
    val <-growingTextInput $ def & textInputConfig_initialValue .~ (show high)
    mapDyn (maybe high id . (readMaybe::String -> Maybe Double)) $ _textInput_value val
  p <- canvasPlot ivals steps
  mn <- el "span" $ do
    text "min"
    val <-growingTextInput $ def & textInputConfig_initialValue .~ (show low)
    mapDyn (maybe low id . (readMaybe::String -> Maybe Double)) $ _textInput_value val

  p' <- combineDyn (,) p mn >>= combineDyn (\x (v,n)-> fmap (\i-> (i*(x-n)/(maximum v))+n) v) mx

  mapDyn (\x-> (Group (Live (fmap (\i->Atom i Inert Once) x, Once) L3) Inert,never,never)) p'
  -- return (val, never, never)

canvasPlot :: MonadWidget t m => [Double] -> Double -> m (Dynamic t ([Double]))
canvasPlot i steps = do
  -- let iVals =  fromList $ fmap (\x-> max 0 $ min 1 (x/maximum i)) scaleList i steps -- Normalized Haskell Sequence (not a map*)
  (canvasElement,canvasPostBuild) <- liftM (\(a,b)->(_el_element a,b)) $ el' "canvas" $ getPostBuild
  let htmlCanvasEl = DT.castToHTMLCanvasElement canvasElement -- HTMLCanvasElement

  md <- wrapDomEvent (canvasElement) (onEventName Mousedown) mouseOffsetXY
  mu <- wrapDomEvent (canvasElement) (onEventName Mouseup) mouseOffsetXY
  mo <- wrapDomEvent (canvasElement) (onEventName Mouseout) mouseOffsetXY
  mouseDown <- holdDyn ((0,0),False) $ leftmost [fmap (\x->(x,True)) md, fmap (\x->(x,False)) mu, fmap (\x->(x,False)) mo]
  moveEv <- wrapDomEvent (canvasElement) (onEventName Mousemove) mouseOffsetXY
  let updateEv = fmap snd $ ffilter (snd . fst) $ attachDyn mouseDown moveEv

  -- performEvent_ $ fmap (\((x,y),d) -> liftIO $ if d then (beginPath htmlCanvasEl >> moveTo htmlCanvasEl x y >> return ()) else (closePath htmlCanvasEl >> return ())) $ updated mouseDown
  performEvent_ $ fmap (\(x,y) -> liftIO $ do
    fillStyle htmlCanvasEl "rgb(0,255,0)"
    h <- getHeight htmlCanvasEl
    fillRect htmlCanvasEl x 0 (x+1) (round h)
    clearRect htmlCanvasEl ( x) 0 (x+1) ( y)
    -- lineTo htmlCanvasEl x y
    -- stroke htmlCanvasEl
    ) updateEv

  debug updateEv

  iDims <- liftIO (getOffsetDimensions htmlCanvasEl)::MonadWidget t m => m (Double,Double)
  -- iWidthDim <- performEvent $ fmap (\_-> liftIO (getOffsetDimensions htmlCanvasEl)) canvasPostBuild
  getCanvasOffsetDims <- performEvent $ fmap (\_ -> liftIO (getOffsetDimensions htmlCanvasEl)) updateEv
  canvasDims <- holdDyn iDims $ leftmost [getCanvasOffsetDims]
  -- let canvasDimsChanged = updated $ nubDyn canvasDims
  -- performEvent_ $ fmap (liftIO . setCanvasDims htmlCanvasEl) canvasDimsChanged
  let iVals = fromList $ fmap (\x-> max 0 $ min 1 (x/maximum i)) $ scaleList i (round $ fst iDims) -- Normalized Haskell Sequence (not a map*)
  let e = attachDynWith (\(cw,ch) (x,y)-> (cw/fromIntegral x,ch/fromIntegral y)) canvasDims updateEv
  vals <- foldDyn (\(x,y) sq -> update (round $ x * (fromIntegral $ length sq)) y sq) iVals e
  -- performEvent_ $ fmap (\(x,y) -> liftIO $ drawPlot htmlCanvasEl (x,y)) updateEv
  debug $ fmap (\x->"vals: "++show x) $ updated vals
  mapDyn toList vals


drawPlot:: DT.HTMLCanvasElement -> (Int, Int) -> IO ()
drawPlot canvas (x,y) = do
  h <- getOffsetHeight canvas
  l <- getBoundingClientLeft canvas
  t <- getBoundingClientTop canvas
  let x' = x-l
  let y' = y-t
  putStrLn ("x: "++show x')
  putStrLn ("h: "++ show h)
  beginPath canvas
  moveTo canvas x 0
  strokeStyle canvas "rgb(0,0,0)"
  lineTo canvas x (round h)
  strokeStyle canvas "rgb(0,100,0)"
  lineTo canvas x y
  stroke canvas
  closePath canvas

-- setCanvasDims:: DT.HTMLCanvasElement -> (Int,Int) -> IO ()
-- setCanvasDims c (x,y) = do
--   setWidth c x
--   setHeight c y

-- foreign import javascript unsafe
--   "$1.getContext('2d').closePath()" closePath :: DT.HTMLCanvasElement -> IO ()

foreign import javascript unsafe
-- "$r = $1.getBoundingClientRect().top" getBoundingClientTop :: DT.HTMLCanvasElement -> IO Int
  "$r = $1.offestLeft" getBoundingClientTop :: DT.HTMLCanvasElement -> IO Int

foreign import javascript unsafe
  "$r = $1.offsetTop" getBoundingClientLeft :: DT.HTMLCanvasElement -> IO Int

foreign import javascript unsafe
  "$1.getContext('2d').closePath()" closePath :: DT.HTMLCanvasElement -> IO ()

foreign import javascript unsafe
  "$1.getContext('2d').beginPath()" beginPath :: DT.HTMLCanvasElement -> IO ()

foreign import javascript unsafe
  "$1.getContext('2d').strokeStyle = $2" strokeStyle :: DT.HTMLCanvasElement -> JSString -> IO ()


foreign import javascript unsafe
  "$1.getContext('2d').moveTo($2, $3)" moveTo :: DT.HTMLCanvasElement -> Int -> Int -> IO ()

foreign import javascript unsafe
  "$1.getContext('2d').lineTo($2, $3)" lineTo :: DT.HTMLCanvasElement -> Int -> Int -> IO ()

foreign import javascript unsafe
  "$1.getContext('2d').stroke()" stroke :: DT.HTMLCanvasElement -> IO ()


scaleList:: [Double] -> Int ->[Double]
scaleList l s
  | length l > s = fmap snd $ filter (\(i,_) -> (mod i (ceiling (fromIntegral (length l)/ (fromIntegral s) ))) == 1) $ zip [(0::Int)..] l
  | length l <= s = take s $ concat $ fmap (\i -> take (ceiling $  (fromIntegral s) / (fromIntegral $ length l)) $ repeat i) l





getOffsetDimensions:: DT.HTMLCanvasElement -> IO (Double,Double)
getOffsetDimensions e = do
  w <- getOffsetWidth e
  h <- getOffsetHeight e
  return (w,h)


foreign import javascript unsafe
  "$r = $1.offsetWidth" getOffsetWidth :: DT.HTMLCanvasElement -> IO Double

foreign import javascript unsafe
  "$r = $1.offsetHeight" getOffsetHeight :: DT.HTMLCanvasElement -> IO Double

foreign import javascript unsafe
  "$r = $1.height" getHeight :: DT.HTMLCanvasElement -> IO Double

foreign import javascript unsafe
  "$1.getContext('2d').fillRect($2,$3,$4,$5)" fillRect :: DT.HTMLCanvasElement -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe
  "$1.getContext('2d').clearRect($2,$3,$4,$5)" clearRect :: DT.HTMLCanvasElement -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe
  "$1.getContext('2d').fillStyle = $2" fillStyle :: DT.HTMLCanvasElement -> JSString -> IO ()

 --
