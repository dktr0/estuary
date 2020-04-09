{- LANGUAGE DeriveGeneric -}

module Estuary.Types.Tempo (module Data.Tempo,module Estuary.Types.Tempo) where

import Data.Tempo
import Data.Time
import Data.Time.Clock.POSIX
-- import GHC.Generics
import Data.Aeson

instance ToJSON Tempo where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Tempo

audioSecondsToUTC :: (UTCTime,Double) -> Double -> UTCTime
audioSecondsToUTC (t0utc,t0audio) t1audio = posixSecondsToUTCTime $ clockDiff + (realToFrac t1audio)
  where clockDiff = utcTimeToPOSIXSeconds t0utc - (realToFrac t0audio)

utcTimeToAudioSeconds :: (UTCTime,Double) -> UTCTime -> Double
utcTimeToAudioSeconds (t0utc,t0audio) t1utc = realToFrac $ utcTimeToPOSIXSeconds $ addUTCTime clockDiff t1utc
  where clockDiff = realToFrac t0audio - utcTimeToPOSIXSeconds t0utc

adjustByClockDiff :: UTCTime -> Tempo -> IO Tempo
adjustByClockDiff x t = do
  tSystem <- getCurrentTime
  let clockDiff = diffUTCTime tSystem x
  return $ t {
    time = addUTCTime (clockDiff*(-1)) (time t)
  }

firstCycleStartAfter :: Tempo -> UTCTime -> UTCTime
firstCycleStartAfter x t = countToTime x $ fromIntegral $ ceiling $ timeToCount x t
