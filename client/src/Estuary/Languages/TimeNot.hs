module Estuary.Languages.TimeNot (timeNot) where

import qualified Data.IntMap as IntMap
import Data.Text (Text)
import Data.Time
import qualified Data.Text as T
import Control.Monad.State.Strict

import qualified Sound.TimeNot.AST as TimeNot
import qualified Sound.TimeNot.Parsers as TimeNot
import qualified Sound.TimeNot.Render as TimeNot

import Estuary.Types.NoteEvent
import Estuary.Types.RenderState
import Estuary.Render.R
import Estuary.Types.Context
import Estuary.Render.TextNotationRenderer
import Estuary.Types.TextNotation
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Types.Tempo


timeNot :: TextNotationRenderer
timeNot = emptyTextNotationRenderer {
  parseZone = _parseZone,
  scheduleNoteEvents = _scheduleNoteEvents -- ,
  -- clearZone' = _clearZone
  }


_parseZone :: Context -> Int -> Text -> UTCTime -> R ()
_parseZone c z x eTime = do
  let parseResult = TimeNot.runCanonParser $ T.unpack x
  case parseResult of
    Right p -> do
      clearZoneError z
      setBaseNotation z TimeNot
      setEvaluationTime z eTime
      modify' $ \xx -> xx { timeNots = IntMap.insert z p (timeNots xx) }
    Left e -> setZoneError z (T.pack $ show e)

_scheduleNoteEvents :: Context -> Int -> R [NoteEvent]
_scheduleNoteEvents c z = do
  s <- get
  let theTempo = (tempo . ensemble . ensembleC) c
  let eTime = IntMap.findWithDefault (wakeTimeSystem s) z $ evaluationTimes s
  let wStart = renderStart s
  let wEnd = renderEnd s
  let oTime = firstCycleStartAfter theTempo eTime
  let p = IntMap.lookup z $ timeNots s
  case p of
    (Just p') -> return $ fmap TimeNot.mapForEstuary $ TimeNot.render oTime p' wStart wEnd
    Nothing -> return []
