module Estuary.WebDirt.Stream where

import Sound.Tidal.Context
import Control.Concurrent.MVar
import Control.Monad.Loops (iterateM_)
import Data.Time (getCurrentTime)
import Data.Map
import qualified Control.Exception as E
import Data.Time

webDirtTicksPerCycle :: Integer
webDirtTicksPerCycle = 8

webDirtStream :: IO (ParamPattern -> IO ())
webDirtStream = do
  now <- getCurrentTime
  mTempo <- newMVar (Tempo {at=now,beat=0.0,cps=1.0,paused=False,clockLatency=0.2})
  mPattern <- newMVar silence
  forkIO $ clockedTickWebDirt mTempo (webDirtTick mPattern)
  return $ \p -> do swapMVar mPattern p
                    return ()

clockedTickWebDirt :: MVar Tempo -> (Tempo -> Int -> IO()) -> IO ()
clockedTickWebDirt mTempo callback = do
  nowBeat <- getCurrentBeat mTempo
  let nextTick = ceiling (nowBeat * (fromIntegral webDirtTicksPerCycle))
  iterateM_ (clockedTickWebDirtLoop callback mTempo) nextTick

clockedTickWebDirtLoop callback mTempo tick = do
  tempo <- readMVar mTempo
  if (paused tempo)
    then do
      let pause = 0.01
      threadDelay $ floor (pause * 1000000)
      return $ if cps tempo < 0 then 0 else tick -- reset tick to 0 if cps is negative
    else do
      now <- getCurrentTime
      let beatsFromAtToTick = fromIntegral tick / fromIntegral webDirtTicksPerCycle - beat tempo
          delayUntilTick = beatsFromAtToTick / cps tempo - realToFrac (diffUTCTime now (at tempo))
      threadDelay $ floor (delayUntilTick * 1000000)
      callback tempo tick
      return $ tick + 1

webDirtTick :: MVar ParamPattern -> Tempo -> Int -> IO ()
webDirtTick patternM tempo ticks = do
  p <- readMVar patternM
  let ticks' = (fromIntegral ticks) :: Integer
      a = ticks' % webDirtTicksPerCycle
      b = (ticks' + 1) % webDirtTicksPerCycle
      events = seqToRelOnsets (a,b) p -- :: [(Double,Map Param (Maybe Value))]
  E.catch (mapM_ (tidalEventToWebDirt) events) (\msg -> putStrLn $ "exception: " ++ show (msg :: E.SomeException))

tidalEventToWebDirt :: (Double,ParamMap) -> IO ()
tidalEventToWebDirt (t,e) = putStrLn $ show t ++ " " ++ show e
