{-# LANGUAGE RecursiveDo, JavaScriptFFI, OverloadedStrings #-}

module Estuary.Widgets.CanvasDisplay (canvasDisplay) where

import Reflex
import Reflex.Dom
import GHCJS.DOM.Types (JSVal,HTMLCanvasElement(..),pToJSVal,uncheckedCastTo)
import GHCJS.Foreign.Callback
import Data.Map
import Data.List
import Data.Text as T
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.MVar
import Data.Time
import JavaScript.Web.AnimationFrame
import JavaScript.Web.Canvas
import GHCJS.Concurrent

import Sound.MusicW.AudioContext

import Estuary.Types.Color
import Estuary.Types.CanvasOp
import Estuary.Types.CanvasState
import Estuary.RenderInfo

canvasDisplay :: MonadWidget t m => Int -> MVar CanvasState -> m ()
canvasDisplay z mv = do
  let attrs = fromList [("class","canvasDisplay"),("style",T.pack $ "z-index:" ++ show z),("width","1920"),("height","1080")]
  cvs <- liftM (uncheckedCastTo HTMLCanvasElement .  _element_raw . fst) $ elAttr' "canvas" attrs $ return ()
  ctx <- liftIO $ getContext (unsafeToCanvas $ pToJSVal cvs)
  liftIO $ requestAnimationFrame ctx mv
  -- *** note: also need to consider how to interrupt requestAnimationFrame when widget is destroyed

requestAnimationFrame :: Context -> MVar CanvasState -> IO ()
requestAnimationFrame ctx mv = do
  inAnimationFrame ThrowWouldBlock $ redrawCanvas ctx mv
  return ()

redrawCanvas :: Context -> MVar CanvasState -> Double -> IO ()
redrawCanvas ctx mv _ = synchronously $ do
  t1 <- liftAudioIO $ audioUTCTime
  cState <- takeMVar mv
  let ops = queuedOps cState
  let dropTime = addUTCTime (-0.2) t1 -- drop anything that is more than 1/5 second late
  let (opsToDrop,opsToKeep) = Data.List.partition ((< dropTime) . fst) ops
  let nOpsToDrop = Data.List.length opsToDrop
  when (nOpsToDrop > 0) $ putStrLn $ "dropping " ++ show nOpsToDrop ++ " canvas ops"
  let (opsForNow,opsForLater) = Data.List.partition ((<= t1) . fst) opsToKeep
  putMVar mv $ cState { queuedOps = opsForLater, previousDrawStart = t1 }
  performCanvasOps ctx opsForNow
  requestAnimationFrame ctx mv

performCanvasOps :: Context -> [(UTCTime,CanvasOp)] -> IO ()
performCanvasOps ctx ops = mapM_ (canvasOp ctx) $ fmap (toActualWandH 1920 1080 . snd) ops

canvasOp :: Context -> CanvasOp -> IO ()
canvasOp ctx (Clear a) = do
  fillStyle 0 0 0 (a/100) ctx
  strokeStyle 0 0 0 (a/100) ctx
  rect 0 0 1920 1080 ctx
  stroke ctx
  fill ctx
canvasOp ctx (Rect x y w h) = beginPath ctx >> rect x y w h ctx >> stroke ctx >> fill ctx
canvasOp ctx (Tri x0 y0 x1 y1 x2 y2) = do
  beginPath ctx
  moveTo x0 y0 ctx
  lineTo x1 y1 ctx
  lineTo x2 y2 ctx
  lineTo x0 y0 ctx
  stroke ctx
  fill ctx
canvasOp ctx (MoveTo x y) = moveTo x y ctx
canvasOp ctx (LineTo x y) = beginPath ctx >> lineTo x y ctx >> stroke ctx >> fill ctx
canvasOp ctx (StrokeStyle (RGBA r g b a)) = strokeStyle (f r) (f g) (f b) (a/100) ctx
  where f x = round $ x*255/100
canvasOp ctx (FillStyle (RGBA r g b a)) = fillStyle (f r) (f g) (f b) (a/100) ctx
  where f x = round $ x*255/100
