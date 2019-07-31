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

sendSoundsUtc :: SampleEngine e => e -> [(UTCTime,Tidal.ControlMap)] -> IO ()
sendSoundsUtc e sounds = do
  clockDiff <- getClockDiff e
  let latency = 0.2 -- hardwired latency for now???
  let sounds' = fmap (\(x,y) -> (utcTimeToDouble x - clockDiff + latency,y)) sounds
  catch (mapM_ (playSample e) sounds')
    (\msg -> putStrLn $ "exception: " ++ show (msg :: SomeException))

sendSoundsAudio :: SampleEngine e => e -> [(Double,Tidal.ControlMap)] -> IO ()
sendSoundsAudio e sounds = do
  clockDiff <- getClockDiff e
  let latency = 0.2 -- hardwired latency for now???
  let sounds' = fmap (\(x,y) -> (x - clockDiff + latency,y)) sounds
  catch (mapM_ (playSample e) sounds')
    (\msg -> putStrLn $ "exception: " ++ show (msg :: SomeException))
