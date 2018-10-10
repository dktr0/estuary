module Estuary.Types.Context where

import Data.Time
import Data.Map

import qualified Sound.Tidal.Context as Tidal
import Estuary.Tidal.Types
import Estuary.Types.Language
import Estuary.Types.Definition

data Context = Context {
  language :: Language,
  theme :: String,
  startTime :: UTCTime,
  tempo :: Tidal.Tempo,
  definitions :: DefinitionMap,
  webDirtOn :: Bool,
  superDirtOn :: Bool,
  peakLevels :: [Double],
  rmsLevels :: [Double],
  wsStatus :: String,
  clientCount :: Int
  }

emptyContext :: UTCTime -> Context
emptyContext now = Context {
  language = English,
  theme = "classic.css",
  startTime = now,
  tempo = Tidal.Tempo now 0.0 0.5 False 0.2,
  definitions = empty,
  webDirtOn = True,
  superDirtOn = False,
  peakLevels = [],
  rmsLevels = [],
  wsStatus = "",
  clientCount = 0
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
