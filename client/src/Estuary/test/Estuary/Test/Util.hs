module Estuary.Test.Util where

import Control.Concurrent
import Control.Exception

tryUntilTimeout :: (Real interval) => interval -> Int -> IO a -> IO a
tryUntilTimeout wait attempts action = go (max (1::Int) attempts)
  where 
    go attemptsLeft = do
      result <- try action
      case result of
        Left e -> 
          if attemptsLeft > 1 then do
            threadDelay $ (ceiling (toRational wait * 1000) :: Int)
            go (attemptsLeft - 1)
          else
            throwIO (e :: SomeException)
        Right a -> return a
