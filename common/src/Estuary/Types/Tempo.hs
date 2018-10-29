{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.Tempo where

import Text.JSON
import Text.JSON.Generic
import Data.Time.Clock

data Tempo = Tempo {
  cps :: Double,
  at :: UTCTime,
  beat :: Double
  } deriving (Eq,Data,Typeable)

instance JSON Tempo where
  showJSON = toJSON
  readJSON = fromJSON

instance Show Tempo where
  show _ = "a tempo"

adjustCps :: Tempo -> UTCTime -> Double -> Tempo
adjustCps prevTempo now newCps = Tempo {
  cps = newCps,
  at = now,
  beat = elapsedTime * cps prevTempo + beat prevTempo
  }
  where elapsedTime = realToFrac $ diffUTCTime now (at prevTempo)

adjustCpsNow :: Tempo -> Double -> IO Tempo
adjustCpsNow prevTempo newCps = do
  now <- getCurrentTime
  return $ adjustCps prevTempo now newCps
