module Estuary.Types.Loadable where

import GHCJS.Types
import Data.Text
import Data.Map
import Data.IORef

-- Instances of the class Loadable represent resources that are initiated/allocated
-- by starting an asynchronous URL request, and can report/keep track of the status
-- of that asynchronous request (LoadStatus).

class Loadable a where
  newLoadable :: Text -> (a -> IO ()) -> IO a   -- arguments are URL and a callback when loading succeeds
  loadStatus :: a -> IO LoadStatus

data LoadStatus =
  NotLoaded |
  Loading |
  Loaded |
  LoadError Text deriving (Eq,Show)


-- LoadMap a represents an updatable map from text URLs to asynchronous resources (instances of Loadable)
-- for example, a LoadMap can be used to acquire and cache audio files resources from the web.

type LoadMap a = IORef (Map Text a)

newLoadMap :: IO (LoadMap a)
newLoadMap = newIORef Data.Map.empty

load :: Loadable a => LoadMap a -> Text -> (a -> IO ()) -> IO a
load m url cb = do
  m' <- readIORef m
  case Data.Map.lookup url m' of
    Nothing -> do
      x <- newLoadable url cb
      writeIORef m $ Data.Map.insert url x m'
      return x
    Just x -> return x
