module Estuary.Languages.TimeNot (timeNot) where

import qualified Data.IntMap as IntMap
import Data.Text (Text)
import Data.Time
import qualified Data.Text as T
import Control.Monad.State.Strict
import GHCJS.Types
import GHCJS.Marshal -- .Internal

import Estuary.Types.NoteEvent
import Estuary.Render.R hiding (setTempo)
import Estuary.Types.TextNotation
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Types.Tempo
import Estuary.Render.ForeignTempo
import Estuary.Languages.ExoLang (ExoResult,utcTimeToWhenPOSIX,exoResultToErrorText)


timeNot :: TextNotationRenderer
timeNot = emptyTextNotationRenderer {
  parseZone = _parseZone,
  scheduleWebDirtEvents = _scheduleWebDirtEvents,
  clearZone' = _clearZone
  }


_parseZone :: Int -> Text -> UTCTime -> R ()
_parseZone z txt eTime = do
  s <- get
  timekNot <- case IntMap.lookup z (timeNots s) of
    Just j -> pure j
    Nothing -> do
      x <- liftIO $ launch
      modify' $ \ss -> ss { timeNots = IntMap.insert z x $ timeNots ss }
      pure x
  evalResult <- liftIO $ evaluate timekNot txt (utcTimeToWhenPOSIX eTime)
  case exoResultToErrorText evalResult of
    Just err -> do
      setBaseNotation z TimeNot
      setZoneError z err
    Nothing -> do
      setBaseNotation z TimeNot
      setEvaluationTime z eTime
      clearZoneError z


_scheduleWebDirtEvents :: Int -> R [JSVal]
_scheduleWebDirtEvents z = do
  liftIO $ putStrLn "_scheduleWebDirtEvents"
  s <- get
  case IntMap.lookup z (timeNots s) of
    Just timekNot -> liftIO $ do
      setTempo timekNot $ tempoCache s
      let wStart = utcTimeToWhenPOSIX $ renderStart s
      let wEnd = utcTimeToWhenPOSIX $ renderEnd s
      j <- _scheduleEvents timekNot wStart wEnd
      j' <- fromJSVal j -- :: JSM (Maybe [JSVal])
      case j' of
        Just xs -> return xs
        Nothing -> do
          putStrLn "Estuary.Languages.TimeNot JSval problem"
          return []
    Nothing -> return []


_clearZone :: Int -> R ()
_clearZone z = modify' $ \s -> s { timeNots = IntMap.delete z (timeNots s) }


foreign import javascript safe
  "window.timekNot.launch()"
  launch :: IO JSVal

foreign import javascript safe
  "window.timekNot.evaluate($1,$2,$3)"
  evaluate :: JSVal -> Text -> Double -> IO ExoResult

foreign import javascript safe
  "window.timekNot.scheduleNoteEvents($1,$2,$3)"
  _scheduleEvents :: JSVal -> Double -> Double -> IO JSVal

foreign import javascript unsafe
  "window.timekNot.setTempo($1,$2)"
  _setTempo :: JSVal -> ForeignTempo -> IO ()

setTempo :: JSVal -> Tempo -> IO ()
setTempo timekNot x = _setTempo timekNot $ toForeignTempo x
