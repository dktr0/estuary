{-# LANGUAGE OverloadedStrings #-}

module Estuary.Render.WebSerial (
  WebSerial,
  newWebSerial,
  isActive,
  listPorts,
  setActivePort,
  setNoActivePort,
  send,
  flush
  ) where

import Data.IntMap
import Data.IORef
import Data.Text
import qualified Data.ByteString as B
import Data.Time
import Sound.Osc.Datum
import Estuary.Types.NoteEvent
import Data.Map as Map
import Control.Monad (guard)
import Data.List (partition)


type WebSerialEvent = (UTCTime,B.ByteString)

data WebSerial = WebSerial {
  activePort :: IORef (Maybe Int),
  queue :: IORef [WebSerialEvent]
  }

newWebSerial :: IO WebSerial
newWebSerial = do
  _activePort <- newIORef Nothing
  _queue <- newIORef []
  putStrLn "Estuary WebSerial: initialized."
  pure $ WebSerial {
    activePort = _activePort,
    queue = _queue
  }

isActive :: WebSerial -> IO Bool
isActive webSerial = do
  _activePort <- readIORef (activePort webSerial)
  case _activePort of
    Nothing -> pure False
    Just _ -> pure True

listPorts :: WebSerial -> IO (IntMap Text)
listPorts webSerial = pure $ Data.IntMap.empty

setActivePort :: WebSerial -> Int -> IO ()
setActivePort webSerial x = writeIORef (activePort webSerial) (Just x)

setNoActivePort :: WebSerial -> IO ()
setNoActivePort webSerial = writeIORef (activePort webSerial) Nothing


send :: WebSerial -> NoteEvent -> IO ()
send webSerial ev = case noteEventToWebSerialEvent ev of
  Just ev' -> do
    evs <- readIORef (queue webSerial)
    writeIORef (queue webSerial) $ evs ++ [ev']
  Nothing -> pure ()


flush :: WebSerial -> IO ()
flush webSerial = do
  now <- getCurrentTime
  evs <- readIORef (queue webSerial)
  let (evs1,evs2) = Data.List.partition (\(t,_) -> now >= t ) evs
  mapM_ (\_ -> putStrLn "Estuary WebSerial: flushing event (PLACEHOLDER)") evs1
  writeIORef (queue webSerial) evs2


noteEventToWebSerialEvent :: NoteEvent -> Maybe WebSerialEvent
noteEventToWebSerialEvent (nWhenUtc,nMap) = do
  s <- Map.lookup "s" nMap
  guard $ s == AsciiString "serial"
  b <- Map.lookup "byte" nMap
  b' <- datumToInteger b
  pure $ (nWhenUtc,B.singleton $ fromInteger b')
