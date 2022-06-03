{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Languages.LocoMotion (locoMotion) where

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
      j <- liftIO $ newLocoMotion cvs
      modify' $ \s -> s { locoMotions = IntMap.insert z j $ locoMotions s }
      return j
    Just x -> return x
  r <- liftIO $ evaluate j txt
  case r of
    Just err -> do
      setBaseNotation z TextNotation.LocoMotion
      setZoneError z err
    Nothing -> do
      setBaseNotation z TextNotation.LocoMotion
      setEvaluationTime z eTime
      clearZoneError z


clearZone'' :: Int -> R ()
clearZone'' z = modify' $ \x -> x { locoMotions = IntMap.delete z $ locoMotions x }


zoneAnimationFrame' :: UTCTime -> Int -> R ()
zoneAnimationFrame' now z = do
  s <- get
  case IntMap.lookup z (locoMotions s) of
    Just lm -> liftIO $ setTempo lm (tempoCache s) >> animate lm
    Nothing -> return ()


foreign import javascript safe
  "new LocoMotion.LocoMotion($1)"
  newLocoMotion :: HTMLCanvasElement -> IO LocoMotion

-- Nothing values represent successful evaluation
evaluate :: LocoMotion -> Text -> IO (Maybe Text)
evaluate lm x = do
  x <- _evaluate lm x
  case _success x of
    True -> return Nothing
    False -> return $ Just $ _error x

foreign import javascript safe
  "$1.evaluate($2)"
  _evaluate :: LocoMotion -> Text -> IO JSVal

foreign import javascript safe
  "$1.success"
  _success :: JSVal -> Bool

foreign import javascript safe
  "$1.error"
  _error :: JSVal -> Text

foreign import javascript unsafe
  "$1.animate()"
  animate :: LocoMotion -> IO ()

foreign import javascript unsafe
  "$1.setTempo($2)"
  _setTempo :: LocoMotion -> ForeignTempo -> IO ()

setTempo :: LocoMotion -> Tempo -> IO ()
setTempo lm x = _setTempo lm $ toForeignTempo x
