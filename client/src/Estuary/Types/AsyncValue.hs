module Estuary.Types.AsyncValue
  (
  AsyncValue,
  asyncValue,
  blocking,
  nonBlocking
  ) where

import GHCJS.DOM.Types hiding (Text)
import Data.IORef
import Control.Concurrent
import Control.Concurrent.Async

data AsyncValueState a = NotLoaded (IO a) | Loading (Async a) | Loaded a

data AsyncValue a = AsyncValue (IORef (AsyncValueState a))

asyncValue :: IO a -> IO (AsyncValue a)
asyncValue io = AsyncValue <$> newIORef (NotLoaded io)

blocking :: AsyncValue a -> IO a
blocking (AsyncValue ioref) = do
  x <- readIORef ioref
  case x of
    Loaded a -> pure a
    Loading asyncA -> do
      wait asyncA
      blocking (AsyncValue ioref)
    NotLoaded ioA -> withAsync ioA $ \asyncA -> do
      a <- wait asyncA
      writeIORef ioref (Loaded a)
      pure a

nonBlocking :: AsyncValue a -> IO (Maybe a)
nonBlocking (AsyncValue ioref) = do
  x <- readIORef ioref
  case x of
    Loaded a -> pure (Just a)
    Loading _ -> pure Nothing
    NotLoaded ioA -> do
      asyncA <- async $ do
        a <- ioA
        writeIORef ioref (Loaded a)
        pure a
      writeIORef ioref (Loading asyncA)
      pure Nothing
