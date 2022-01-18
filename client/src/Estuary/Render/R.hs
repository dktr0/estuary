module Estuary.Render.R where

import Data.Time
import Data.Text (Text)
import Control.Monad.State.Strict
import Control.Exception (evaluate,catch,SomeException,try)
import Data.IntMap.Strict as IntMap

import qualified Sound.Tidal.Context as Tidal

import Estuary.Types.TextNotation
import Estuary.Types.NoteEvent
import Estuary.Types.RenderInfo
import Estuary.Types.RenderState

type R = StateT RenderState IO

runR :: R a -> RenderState -> IO RenderState
runR r rs = (execStateT r rs) `catch` (\e -> putStrLn "runRenderer" >> putStrLn (show (e :: SomeException)) >> return rs)

pushNoteEvents :: [NoteEvent] -> R ()
pushNoteEvents xs = modify' $ \x -> x { noteEvents = noteEvents x ++ xs }

pushTidalEvents :: [(UTCTime,Tidal.ValueMap)] -> R ()
pushTidalEvents xs = modify' $ \x -> x { tidalEvents = tidalEvents x ++ xs }

setZoneError :: Int -> Text -> R ()
setZoneError z t = do
  s <- get
  let oldErrors = errors $ info s
  let newErrors = insert z t oldErrors
  modify' $ \x -> x { info = (info s) { errors = newErrors } }

clearZoneError :: Int -> R ()
clearZoneError z = do
  s <- get
  let oldErrors = errors $ info s
  let newErrors = delete z oldErrors
  modify' $ \x -> x { info = (info s) { errors = newErrors } }

setBaseNotation :: Int -> TextNotation -> R ()
setBaseNotation z n = modify' $ \x -> x { baseNotations = insert z n $ baseNotations x}

setEvaluationTime :: Int -> UTCTime -> R ()
setEvaluationTime z n = modify' $ \x -> x { evaluationTimes = insert z n $ evaluationTimes x}
