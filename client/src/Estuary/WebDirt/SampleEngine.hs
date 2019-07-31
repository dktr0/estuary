module Estuary.WebDirt.SampleEngine where

import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Sound.Tidal.Context as Tidal
import Control.Exception
import Sound.MusicW.AudioContext (utcTimeToDouble)
import Data.Bifunctor

class SampleEngine e where
  playSample :: e -> (Double,Tidal.ControlMap) -> IO ()
  getPeakLevels :: e -> IO [Double]
  getRmsLevels :: e -> IO [Double]

-- sendSoundsPOSIX expects timestamps that are UTCTime (ie. relative to "global" time)
-- and sends them as doubles in POSIX time (seconds since beginning of Jan 1 1970), plus a small latency
sendSoundsPOSIX :: SampleEngine e => e -> [(UTCTime,Tidal.ControlMap)] -> IO ()
sendSoundsPOSIX e sounds = do
  let latency = 0.2
  let sounds' = fmap (first (\x -> realToFrac $ utcTimeToPOSIXSeconds x + latency)) sounds
  catch (mapM_ (playSample e) sounds')
    (\msg -> putStrLn $ "exception (sendSoundsPOSIX): " ++ show (msg :: SomeException))

-- sendSoundsAudio expects timestamps that are doubles (ie. relative to audio clock)
-- and sends them "as is", with the addition of a small hardwired latency
sendSoundsAudio :: SampleEngine e => e -> [(Double,Tidal.ControlMap)] -> IO ()
sendSoundsAudio e sounds = do
  let latency = 0.2
  let sounds' = fmap (\(x,y) -> (x + latency,y)) sounds
  catch (mapM_ (playSample e) sounds')
    (\msg -> putStrLn $ "exception (sendSoundsAudio): " ++ show (msg :: SomeException))
