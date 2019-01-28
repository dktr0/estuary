module Estuary.WebDirt.SampleEngine where

import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Sound.Tidal.Context as Tidal
import Control.Exception
import Sound.MusicW.AudioContext (utcTimeToDouble)

class SampleEngine e where
  getClockDiff :: e -> IO Double -- diff btw clock used to play sample events and POSIX
  playSample :: e -> (Double,Tidal.ControlMap) -> IO ()
  getPeakLevels :: e -> IO [Double]
  getRmsLevels :: e -> IO [Double]

sendSounds :: SampleEngine e => e -> [(UTCTime,Tidal.ControlMap)] -> IO ()
sendSounds e sounds = do
  clockDiff <- getClockDiff e
  let latency = 0.2 -- hardwired latency for now???
  -- putStrLn $ show sounds
--  let sounds' = fmap (\(x,y) -> (realToFrac (utcTimeToPOSIXSeconds x) - clockDiff + latency,y)) sounds
  let sounds' = fmap (\(x,y) -> (utcTimeToDouble x - clockDiff + latency,y)) sounds
  -- putStrLn $ show sounds'
  catch (mapM_ (playSample e) sounds')
    (\msg -> putStrLn $ "exception: " ++ show (msg :: SomeException))
