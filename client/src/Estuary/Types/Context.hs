module Estuary.Types.Context where

import Data.Time
import Data.IntMap.Strict
import Control.Concurrent.MVar

import Estuary.Tidal.Types
import Estuary.Types.Language
import Estuary.Types.Definition
import Estuary.Types.Samples
import Estuary.WebDirt.WebDirt
import Estuary.WebDirt.SuperDirt
import Estuary.RenderState
import Estuary.Types.Tempo
import Estuary.Types.CanvasOp

data Context = Context {
  webDirt :: WebDirt,
  superDirt :: SuperDirt,
  language :: Language,
  theme :: String,
  tempo :: Tempo,
  definitions :: DefinitionMap,
  samples :: SampleMap,
  webDirtOn :: Bool,
  superDirtOn :: Bool,
  peakLevels :: [Double],
  rmsLevels :: [Double],
  wsStatus :: String,
  clientCount :: Int,
  canvasOpsQueue :: MVar [(UTCTime,CanvasOp)]
  }

initialContext :: UTCTime -> WebDirt -> SuperDirt -> MVar [(UTCTime,CanvasOp)] -> Context
initialContext now wd sd mv = Context {
  webDirt = wd,
  superDirt = sd,
  language = English,
  theme = "../css-custom/classic.css",
  tempo = Tempo { cps = 0.5, at = now, beat = 0.0 },
  definitions = empty,
  samples = emptySampleMap,
  webDirtOn = True,
  superDirtOn = False,
  peakLevels = [],
  rmsLevels = [],
  wsStatus = "",
  clientCount = 0,
  canvasOpsQueue = mv
}

type ContextChange = Context -> Context

setTheme :: String -> ContextChange
setTheme x c = c {theme = x}

setLanguage :: Language -> ContextChange
setLanguage x c = c { language = x }

setPeakLevels :: [Double] -> ContextChange
setPeakLevels xs c = c { peakLevels = xs }

setRmsLevels :: [Double] -> ContextChange
setRmsLevels xs c = c { rmsLevels = xs }

setClientCount :: Int -> ContextChange
setClientCount x c = c { clientCount = x }

setDefinitions :: DefinitionMap -> ContextChange
setDefinitions x c = c { definitions = x }

setSampleMap :: SampleMap -> ContextChange
setSampleMap x c = c { samples = x}
