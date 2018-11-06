module Estuary.WebDirt.SampleEngine where

import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Sound.Tidal.Context as Tidal
import Control.Exception

class SampleEngine e where
  getClockDiff :: e -> IO Double -- diff btw clock used to play sample events and POSIX
  playSample :: e -> (Double,Tidal.ParamMap) -> IO ()
  getPeakLevels :: e -> IO [Double]
  getRmsLevels :: e -> IO [Double]

sendSounds :: SampleEngine e => e -> [(UTCTime,Tidal.ParamMap)] -> IO ()
sendSounds e sounds = do
  clockDiff <- getClockDiff e
  let latency = 0.2 -- hardwired latency for now???
  let sounds' = fmap (\(x,y) -> (realToFrac (utcTimeToPOSIXSeconds x) - clockDiff + latency,y)) sounds
  -- putStrLn $ show sounds'
  catch (mapM_ (playSample e) sounds')
    (\msg -> putStrLn $ "exception: " ++ show (msg :: SomeException))
