module Estuary.WebDirt.Stream where

import Sound.Tidal.Context
import Control.Concurrent.MVar
import Control.Monad.Loops (iterateM_)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX
import Data.Map
import qualified Control.Exception as E
import Data.Time

import Estuary.WebDirt.SampleEngine
import qualified Estuary.WebDirt.WebDirt as WebDirt
import qualified Estuary.WebDirt.SuperDirt as SuperDirt


type SampleStream = ParamPattern -> IO ()

sampleStream :: SampleEngine e => e -> MVar Tempo -> IO SampleStream
sampleStream engine tempo = do
  mPattern <- newMVar silence
  forkIO $ do
    nowUtc <- getCurrentTime
    tempo' <- readMVar tempo
    iterateM_ (clockedTickLoop engine tempo (tick engine mPattern)) 0
  return $ \p -> do swapMVar mPattern p
                    return ()

utcToBeat :: UTCTime -> Tempo -> Double
utcToBeat now t = beat t + beatDelta
  where
    delta = realToFrac $ diffUTCTime now (at t)
    beatDelta = cps t * delta

clockedTickLoop engine tempo callback _ = do
  nowUtc <- getCurrentTime
  tempo' <- readMVar tempo
  let nowBeat = utcToBeat nowUtc tempo'
  let nextTick = ceiling (nowBeat * (fromIntegral ticksPerCycle))
  let beatsFromAtToTick = fromIntegral nextTick / fromIntegral ticksPerCycle - beat tempo'
  let delayUntilTick = beatsFromAtToTick / cps tempo' - realToFrac (diffUTCTime nowUtc (at tempo'))
  -- putStrLn $ show tempo'
  -- putStrLn $ "nowBeat=" ++ (show nowBeat) ++ " nextTick " ++ (show nextTick) ++ " in " ++ (show $ delayUntilTick * 1000000) ++ " microseconds"
  threadDelay $ floor (delayUntilTick * 1000000)
  callback tempo' nextTick
  return 0

tick :: SampleEngine e => e -> MVar ParamPattern -> Tempo -> Int -> IO ()
tick e patternM tempo ticks = do
  p <- readMVar patternM
  clockDiff <- getClockDiff e
  let latency = clockLatency tempo
  let ticks' = (fromIntegral ticks) :: Integer
      a = ticks' % ticksPerCycle
      b = (ticks' + 1) % ticksPerCycle
      events = seqToRelOnsetDeltas (a,b) p -- :: [(Double,Map Param (Maybe Value))], note: Double is POSIX time
      events' = Prelude.map (\(o,_,m) -> ((logicalOnset' tempo ticks o 0) + clockDiff + latency,m)) events
  E.catch (mapM_ (playSample e) events') (\msg -> putStrLn $ "exception: " ++ show (msg :: E.SomeException))
