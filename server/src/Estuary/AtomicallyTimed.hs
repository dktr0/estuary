{-# LANGUAGE TemplateHaskell #-}

module Estuary.AtomicallyTimed where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Time
import Control.Monad
import Language.Haskell.TH

atomicallyTimed :: String -> STM a -> IO a
atomicallyTimed m x = do
  warningThread <- forkIO $ do
    threadDelay 1000
    putStrLn $ "*** atomically (" ++ m ++ ") is taking a long time..."
  t0 <- getCurrentTime
  x' <- Control.Concurrent.STM.atomically x
  t1 <- getCurrentTime
  killThread warningThread
  let diff = diffUTCTime t1 t0
  when (diff > 0.001) $ putStrLn $ "*** atomically (" ++ m ++ ") took " ++ show diff ++ " seconds ***"
  return x'

readTVarIOTimed :: String -> TVar a -> IO a
readTVarIOTimed m x = do
  warningThread <- forkIO $ do
    threadDelay 1000
    putStrLn $ "*** readTVarIO (" ++ m ++ ") is taking a long time..."
  t0 <- getCurrentTime
  x' <- Control.Concurrent.STM.readTVarIO x
  t1 <- getCurrentTime
  killThread warningThread
  let diff = diffUTCTime t1 t0
  when (diff > 0.001) $ putStrLn $ "*** readTVarIO (" ++ m ++ ") took " ++ show diff ++ " seconds ***"
  return x'

atomically :: Q Exp
atomically = do
  loc <- location
  let m = loc_module loc ++ ":" ++ show (fst (loc_start loc))
  [|atomicallyTimed m|]

readTVarIO :: Q Exp
readTVarIO = do
  loc <- location
  let m = loc_module loc ++ ":" ++ show (fst (loc_start loc))
  [|readTVarIOTimed m|]
