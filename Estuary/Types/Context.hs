module Estuary.Types.Context where

data Context = Context {
  peakLevels :: [Double],
  rmsLevels :: [Double]
  }

type ContextChange = Context -> Context

emptyContext :: Context
emptyContext = Context {
  peakLevels = [],
  rmsLevels = []
}

setPeakLevels :: [Double] -> ContextChange
setPeakLevels xs c = c { peakLevels = xs }

setRmsLevels :: [Double] -> ContextChange
setRmsLevels xs c = c { rmsLevels = xs }
