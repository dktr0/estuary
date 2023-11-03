module Estuary.Languages.MiniTidal (miniTidal,renderTidalPattern,renderControlPattern) where

import Data.Time
import Control.Monad.Except
import Data.IntMap as IntMap
import Data.Text as T
import Control.Exception
import Control.Monad.State.Strict
import Sound.MusicW.AudioContext
import Control.Monad.Reader
import qualified Sound.Tidal.Context as Tidal
import Control.DeepSeq
import Data.Maybe

import Estuary.Render.R
import Estuary.Types.RenderInfo
import Estuary.Types.TextNotation
import Estuary.Render.MainBus
import Estuary.Types.Tempo as Tempo
import Estuary.Types.Ensemble
import Estuary.Types.EnsembleC
import Estuary.Types.TidalParser
import Estuary.Languages.TidalParsers
import Estuary.Types.NoteEvent


miniTidal :: TextNotationRenderer
miniTidal = emptyTextNotationRenderer {
  parseZone = _parseZone,
  scheduleNoteEvents = _scheduleNoteEvents,
  clearZone' = clearParamPattern
  }


_parseZone :: Int -> Text -> UTCTime -> R ()
_parseZone z y eTime = do
  s <- get
  parseResult <- liftIO $ (return $! force (tidalParser MiniTidal y)) `catch` (return . Left . (show :: SomeException -> String))
  case parseResult of
    Right p -> do
      clearZoneError z
      setBaseNotation z (TidalTextNotation MiniTidal)
      setEvaluationTime z eTime
      modify' $ \xx -> xx { paramPatterns = insert z p $ paramPatterns xx }
    Left err -> setZoneError z $ T.pack err


_scheduleNoteEvents :: Int -> R [NoteEvent]
_scheduleNoteEvents z = do
  s <- get
  let lt = renderStart s
  let rp = renderPeriod s
  let vMap = valueMap s
  let controlPattern = IntMap.lookup z $ paramPatterns s -- :: Maybe ControlPattern
  case controlPattern of
    Just controlPattern' -> do
      ns <- liftIO $ (return $! force $ renderTidalPattern vMap lt rp (tempoCache s) controlPattern')
        `catch` (\e -> putStrLn (show (e :: SomeException)) >> return [])
      pure $ tidalEventToNoteEvent <$> ns
    Nothing -> return []


renderControlPattern :: Int -> R ()
renderControlPattern z = do
  wdOn <- webDirtOn
  sdOn <- superDirtOn
  when (wdOn || sdOn) $ do
    s <- get
    let controlPattern = IntMap.lookup z $ paramPatterns s -- :: Maybe ControlPattern
    let vMap = valueMap s
    case controlPattern of
      Just controlPattern' -> do
        let lt = renderStart s
        let rp = renderPeriod s
        ns <- liftIO $ (return $! force $ renderTidalPattern vMap lt rp (tempoCache s) controlPattern')
          `catch` (\e -> putStrLn (show (e :: SomeException)) >> return [])
        pushNoteEvents $ tidalEventToNoteEvent <$> ns
      Nothing -> return ()


renderTidalPattern :: Tidal.ValueMap -> UTCTime -> NominalDiffTime -> Tempo -> Tidal.ControlPattern -> [(UTCTime,Tidal.ValueMap)]
renderTidalPattern vMap start range t p = events''
  where
    start' = (realToFrac $ diffUTCTime start (time t)) * freq t + Tempo.count t -- start time in cycles since beginning of tempo
    end = realToFrac range * freq t + start' -- end time in cycles since beginning of tempo
    a = Tidal.Arc (toRational start') (toRational end)
    events = Tidal.query p $ Tidal.State a vMap
    events' = Prelude.filter Tidal.eventHasOnset events
    events'' = f <$> events'
    f e = (utcTime,Tidal.value e)
      where
        utcTime = addUTCTime (realToFrac ((fromRational w1 - Tempo.count t)/freq t)) (time t)
        w1 = Tidal.start $ fromJust $ Tidal.whole e
