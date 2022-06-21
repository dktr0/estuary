{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Languages.LocoMotion (Estuary.Languages.LocoMotion.locoMotion) where

import Data.Text
import Data.Time
import qualified Data.IntMap as IntMap
import GHCJS.DOM.Types hiding (Text)
import Control.Monad.State.Strict

import Estuary.Types.TextNotation as TextNotation
import Estuary.Types.Context
import Estuary.Types.Tempo
import Estuary.Render.R
import Estuary.Render.ForeignTempo
import Estuary.Render.TextNotationRenderer
import Estuary.Types.RenderState


locoMotion :: TextNotationRenderer
locoMotion = emptyTextNotationRenderer {
  parseZone = parseZone',
  clearZone' = clearZone'',
  preAnimationFrame = preAnimationFrame',
  zoneAnimationFrame = zoneAnimationFrame',
  postAnimationFrame = postAnimationFrame'
  }


-- if a LocoMotion object hasn't already been allocated, allocate it
-- otherwise, just retrieve it from the RenderState
getLocoMotion :: R LocoMotion
getLocoMotion = do
  s <- get
  case Estuary.Types.RenderState.locoMotion s of
    Just j -> pure $ pFromJSVal j
    Nothing -> do
      let cvs = locoMotionCanvas s
      lm <- liftIO $ newLocoMotion cvs
      modify' $ \x -> x { Estuary.Types.RenderState.locoMotion = Just $ pToJSVal lm }
      pure lm


-- note: Context is unused here and could eventually be removed as an argument
parseZone' :: Context -> Int -> Text -> UTCTime -> R ()
parseZone' _ z txt eTime = do
  lm <- getLocoMotion
  r <- liftIO $ evaluate lm z txt
  case r of
    Just err -> do
      setBaseNotation z TextNotation.LocoMotion
      setZoneError z err
    Nothing -> do
      setBaseNotation z TextNotation.LocoMotion
      setEvaluationTime z eTime
      clearZoneError z


clearZone'' :: Int -> R ()
clearZone'' z = do
  lm <- getLocoMotion
  liftIO $ clearZone lm z


preAnimationFrame' :: R ()
preAnimationFrame' = do
  lm <- getLocoMotion
  liftIO $ preAnimate lm


zoneAnimationFrame' :: UTCTime -> Int -> R ()
zoneAnimationFrame' _ z = do
  lm <- getLocoMotion
  s <- get
  liftIO $ setTempo lm (tempoCache s)
  liftIO $ animateZone lm z


postAnimationFrame' :: R ()
postAnimationFrame' = do
  lm <- getLocoMotion
  liftIO $ postAnimate lm



foreign import javascript safe
  "new LocoMotion.LocoMotion($1)"
  newLocoMotion :: HTMLCanvasElement -> IO LocoMotion

-- Nothing values represent successful evaluation
evaluate :: LocoMotion -> Int -> Text -> IO (Maybe Text)
evaluate lm z x = _evaluate lm z x >>= (pure . exoResultToErrorText)

foreign import javascript safe
  "$1.evaluate($2,$3)"
  _evaluate :: LocoMotion -> Int -> Text -> IO ExoResult

foreign import javascript safe
  "$1.clearZone($2)"
  clearZone :: LocoMotion -> Int -> IO ()

foreign import javascript unsafe
  "$1.preAnimate()"
  preAnimate :: LocoMotion -> IO ()

foreign import javascript unsafe
  "$1.animateZone($2)"
  animateZone :: LocoMotion -> Int -> IO ()

foreign import javascript unsafe
  "$1.postAnimate()"
  postAnimate :: LocoMotion -> IO ()

foreign import javascript unsafe
  "$1.setTempo($2)"
  _setTempo :: LocoMotion -> ForeignTempo -> IO ()

setTempo :: LocoMotion -> Tempo -> IO ()
setTempo lm x = _setTempo lm $ toForeignTempo x
