module Estuary.WebDirt.Stream where

import Sound.Tidal.Context
import Control.Concurrent.MVar
import Control.Monad.Loops (iterateM_)
import Control.Monad (liftM)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX
import Data.Map
import qualified Control.Exception as E
import Data.Time
import qualified Estuary.WebDirt.Foreign as WebDirt
import qualified GHCJS.Types as T
import qualified GHCJS.Marshal.Pure as P

webDirtTicksPerCycle :: Integer
webDirtTicksPerCycle = 8

type WebDirtStream = ParamPattern -> IO ()

webDirtStream :: WebDirt.WebDirt -> IO WebDirtStream
webDirtStream webDirt = do
  x <- WebDirt.getCurrentTime webDirt
  let now = posixSecondsToUTCTime $ realToFrac x
  -- mTempo <- newMVar (Tempo {at=now,beat=0.0,cps=1.0,paused=False,clockLatency=0.2})
  mPattern <- newMVar silence
  forkIO $ clockedTickWebDirt webDirt (webDirtTick webDirt mPattern)
  return $ \p -> do swapMVar mPattern p
                    return ()

beatNowWebDirt :: WebDirt.WebDirt -> Tempo -> IO Double
beatNowWebDirt webDirt t = do
  x <- WebDirt.getCurrentTime webDirt
  let now = posixSecondsToUTCTime $ realToFrac x
  let delta = realToFrac $ diffUTCTime now (at t)
  let beatDelta = cps t * delta
  return $ beat t + beatDelta

readWebDirtTempo :: WebDirt.WebDirt -> IO Tempo
readWebDirtTempo webDirt = do
  (time,beats,bpm) <- WebDirt.tempo webDirt
  let time' = posixSecondsToUTCTime $ realToFrac time
  return $ Tempo {at=time',beat=beats,cps=bpm/60.0,paused=False,clockLatency=0.2}

getCurrentWebDirtBeat :: WebDirt.WebDirt -> IO Rational
getCurrentWebDirtBeat webDirt = readWebDirtTempo webDirt >>= beatNowWebDirt webDirt >>= return . toRational

clockedTickWebDirt :: WebDirt.WebDirt -> (Tempo -> Int -> IO()) -> IO ()
clockedTickWebDirt webDirt callback = do
  nowBeat <- getCurrentWebDirtBeat webDirt
  let nextTick = ceiling (nowBeat * (fromIntegral webDirtTicksPerCycle))
  iterateM_ (clockedTickWebDirtLoop webDirt callback) nextTick

clockedTickWebDirtLoop webDirt callback tick = do
  tempo <- readWebDirtTempo webDirt
  if (paused tempo)
    then do
      let pause = 0.01
      threadDelay $ floor (pause * 1000000)
      return $ if cps tempo < 0 then 0 else tick -- reset tick to 0 if cps is negative
    else do
      now <- liftM (posixSecondsToUTCTime . realToFrac) $ WebDirt.getCurrentTime webDirt
      let beatsFromAtToTick = fromIntegral tick / fromIntegral webDirtTicksPerCycle - beat tempo
          delayUntilTick = beatsFromAtToTick / cps tempo - realToFrac (diffUTCTime now (at tempo))
      threadDelay $ floor (delayUntilTick * 1000000)
      callback tempo tick
      return $ tick + 1

webDirtTick :: WebDirt.WebDirt -> MVar ParamPattern -> Tempo -> Int -> IO ()
webDirtTick webDirt patternM tempo ticks = do
  p <- readMVar patternM
  let ticks' = (fromIntegral ticks) :: Integer
      a = ticks' % webDirtTicksPerCycle
      b = (ticks' + 1) % webDirtTicksPerCycle
      events = seqToRelOnsetDeltas (a,b) p -- :: [(Double,Map Param (Maybe Value))]
      events' = Prelude.map (\(o,_,m) -> (f o,m)) events
  E.catch (mapM_ (WebDirt.playSample webDirt) events') (\msg -> putStrLn $ "exception: " ++ show (msg :: E.SomeException))
  where f x = logicalOnset' tempo ticks x 0

