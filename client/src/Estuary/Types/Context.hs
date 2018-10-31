module Estuary.Types.Context where

import Data.Time
import Data.IntMap.Strict
import Estuary.Tidal.Types
import Estuary.Types.Language
import Estuary.Types.Definition
import Estuary.WebDirt.WebDirt
import Estuary.WebDirt.SuperDirt
import Estuary.RenderState
import Estuary.Types.Tempo

data Context = Context {
  webDirt :: WebDirt,
  superDirt :: SuperDirt,
  language :: Language,
  theme :: String,
  tempo :: Tempo,
  definitions :: DefinitionMap,
  webDirtOn :: Bool,
  superDirtOn :: Bool,
  peakLevels :: [Double],
  rmsLevels :: [Double],
  wsStatus :: String,
  clientCount :: Int
  }

initialContext :: UTCTime -> WebDirt -> SuperDirt -> Context
initialContext now wd sd = Context {
  webDirt = wd,
  superDirt = sd,
  language = English,
  theme = "classic.css",
  tempo = Tempo { cps = 0.5, at = now, beat = 0.0 },
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
