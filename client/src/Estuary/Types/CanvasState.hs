module Estuary.Types.CanvasState where

import Data.Time

import Estuary.Types.CanvasOp

data CanvasState = CanvasState {
  queuedOps :: [(UTCTime,CanvasOp)],
  previousDrawStart :: UTCTime
  }

pushCanvasOps :: [(UTCTime,CanvasOp)] -> CanvasState -> CanvasState
pushCanvasOps xs c = c { queuedOps = queuedOps c ++ xs }

emptyCanvasState :: IO CanvasState
emptyCanvasState = do
  now <- getCurrentTime
  return $ CanvasState {
    queuedOps = [],
    previousDrawStart = now
  }
