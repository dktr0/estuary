{-# LANGUAGE RecursiveDo, JavaScriptFFI #-}

module Estuary.Widgets.CanvasDisplay (canvasDisplay) where

import Reflex
import Reflex.Dom
import GHCJS.DOM.Types (HTMLCanvasElement,castToHTMLCanvasElement)
import GHCJS.Types (JSVal)
import Data.JSString
import Data.Map
import Control.Monad
import Control.Monad.Trans

import Estuary.Types.CanvasOp
import Estuary.RenderInfo

canvasDisplay :: MonadWidget t m => Int -> Dynamic t RenderInfo -> m ()
canvasDisplay z rInfo = do
  let attrs = fromList [("class","canvasDisplay"),("style","z-index:" ++ show z),("width","1920"),("height","1080")]
  cvs <- liftM (castToHTMLCanvasElement .  _el_element . fst) $ elAttr' "canvas" attrs $ return ()
  ctx <- liftIO $ getContext cvs
  instructions <- liftM updated $ mapDyn canvasOps rInfo
  performEvent_ $ fmap (liftIO . adjustOps cvs ctx) instructions

adjustOps :: HTMLCanvasElement -> JSVal -> [CanvasOp] -> IO ()
adjustOps cvs ctx ops = mapM_ (canvasOp ctx) $ fmap (toActualWandH 1920 1080) ops

canvasOp :: JSVal -> CanvasOp -> IO ()
canvasOp ctx (Rect x y w h) = beginPath ctx >> rect ctx x y w h >> stroke ctx >> fill ctx
canvasOp ctx (MoveTo x y) = moveTo ctx x y
canvasOp ctx (LineTo x y) = beginPath ctx >> lineTo ctx x y >> stroke ctx >> fill ctx
canvasOp ctx (StrokeStyle c) = strokeStyle ctx (pack $ show c)

foreign import javascript safe
  "$r=$1.getContext('2d')"
  getContext :: HTMLCanvasElement -> IO JSVal

foreign import javascript safe
  "$1.beginPath()"
  beginPath :: JSVal -> IO ()

foreign import javascript safe
  "$1.stroke()"
  stroke :: JSVal -> IO ()

foreign import javascript safe
  "$1.fill()"
  fill :: JSVal -> IO ()

foreign import javascript safe
  "$1.strokeStyle = $2"
  strokeStyle :: JSVal -> JSString -> IO ()

foreign import javascript safe
  "$1.rect($2,$3,$4,$5)"
  rect :: JSVal -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript safe
  "$1.moveTo($2,$3)"
  moveTo :: JSVal -> Double -> Double -> IO ()

foreign import javascript safe
  "$1.lineTo($2,$3)"
  lineTo :: JSVal -> Double -> Double -> IO ()
