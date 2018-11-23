{-# LANGUAGE RecursiveDo, JavaScriptFFI #-}

module Estuary.Widgets.CanvasDisplay (canvasDisplay) where

import Reflex
import Reflex.Dom
import GHCJS.DOM.Types (HTMLCanvasElement,castToHTMLCanvasElement)
import Data.JSString
import Data.Map
import Control.Monad
import Control.Monad.Trans

import Estuary.Types.CanvasOp
import Estuary.RenderInfo

canvasDisplay :: MonadWidget t m => Int -> Dynamic t RenderInfo -> m ()
canvasDisplay z rInfo = do
  let attrs = fromList [("class","canvasDisplay"),("style","z-index:" ++ show z)]
  e <- liftM (castToHTMLCanvasElement .  _el_element . fst) $ elAttr' "canvas" attrs $ return ()
  instructions <- liftM updated $ mapDyn canvasOps rInfo
  performEvent_ $ fmap (liftIO . mapM_ (canvasOpIO e)) instructions

canvasOpIO :: HTMLCanvasElement -> CanvasOp -> IO ()
canvasOpIO e (Rect x y w h) = rect e x y w h >> stroke e
canvasOpIO e (MoveTo x y) = moveTo e x y
canvasOpIO e (LineTo x y) = lineTo e x y >> stroke e
canvasOpIO e (StrokeStyle c) = strokeStyle e (pack $ show c)

foreign import javascript safe
  "$1.getContext('2d').stroke()"
  stroke :: HTMLCanvasElement -> IO ()

foreign import javascript safe
  "$1.getContext('2d').strokeStyle = $2"
  strokeStyle :: HTMLCanvasElement -> JSString -> IO ()

foreign import javascript safe
  "$1.getContext('2d').rect($2,$3,$4,$5)"
  rect :: HTMLCanvasElement -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript safe
  "$1.getContext('2d').moveTo($2,$3)"
  moveTo :: HTMLCanvasElement -> Double -> Double -> IO ()

foreign import javascript safe
  "$1.getContext('2d').lineTo($2,$3)"
  lineTo :: HTMLCanvasElement -> Double -> Double -> IO ()
