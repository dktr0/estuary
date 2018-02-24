module Estuary.Types.Context where

data Context = Context {
  levels :: [Double]
  }

type ContextChange = Context -> Context

emptyContext :: Context
emptyContext = Context {
  levels = []
}

setLevels :: [Double] -> ContextChange
setLevels xs c = c { levels = xs }
