{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Languages.LocoMotion (locoMotion) where

import Data.Text
import Data.Time
import qualified Data.IntMap as IntMap
import GHCJS.DOM.Types (HTMLCanvasElement,JSVal)
import Control.Monad.State.Strict

import Estuary.Types.TextNotation
import Estuary.Types.Context
import Estuary.Render.R
import Estuary.Render.TextNotationRenderer
import Estuary.Types.RenderState


locoMotion :: TextNotationRenderer
locoMotion = TextNotationRenderer {
  parseZone = parseZone',
  clearZone' = clearZone'',
  preAnimationFrame = return (),
  zoneAnimationFrame = zoneAnimationFrame',
  postAnimationFrame = return ()
  }

parseZone' :: Context -> Int -> Text -> UTCTime -> R ()
parseZone' ctx z txt eTime = do
  s <- get
  let cvs = locoMotionCanvas s
  j <- case IntMap.lookup z (locoMotions s) of
    Nothing -> do
      j <- liftIO $ launch cvs
      modify' $ \s -> s { locoMotions = IntMap.insert z j $ locoMotions s }
      return j
    Just x -> return x
  r <- liftIO $ evaluateLocoMotion j txt
  case r of
    Just err -> do
      setBaseNotation z LocoMotion
      setZoneError z err
    Nothing -> do
      setBaseNotation z LocoMotion
      setEvaluationTime z eTime
      clearZoneError z



clearZone'' :: Int -> R ()
clearZone'' z = modify' $ \x -> x { locoMotions = IntMap.delete z $ locoMotions x }


zoneAnimationFrame' :: UTCTime -> Int -> R ()
zoneAnimationFrame' now z = do
  s <- get
  case IntMap.lookup z (locoMotions s) of
    Just x -> liftIO $ animateLocoMotion x
    Nothing -> return ()


foreign import javascript safe
  "locoMotion.launch($1)"
  launch :: HTMLCanvasElement -> IO JSVal

-- Nothing values represent successful evaluation
evaluateLocoMotion :: JSVal -> Text -> IO (Maybe Text)
evaluateLocoMotion j x = do
  x <- _evaluateLocoMotion j x
  case _success x of
    True -> return Nothing
    False -> return $ Just $ _error x

foreign import javascript safe
  "locoMotion.evaluateLocomotion($1,$2)"
  _evaluateLocoMotion :: JSVal -> Text -> IO JSVal

foreign import javascript safe
  "$1.success"
  _success :: JSVal -> Bool

foreign import javascript safe
  "$1.error"
  _error :: JSVal -> Text

foreign import javascript unsafe
  "locoMotion.animateLocomotion($1)"
  animateLocoMotion :: JSVal -> IO ()
