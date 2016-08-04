module Estuary.WebDirt.Stream where

import Sound.Tidal.Context
import Control.Concurrent.MVar
import Control.Monad.Loops (iterateM_)
import Data.Time (getCurrentTime)
import Data.Map
import qualified Control.Exception as E
import Data.Time
import Estuary.WebDirt.Foreign
import qualified GHCJS.Types as T
import qualified GHCJS.Marshal.Pure as P

webDirtTicksPerCycle :: Integer
webDirtTicksPerCycle = 8

type WebDirtStream = ParamPattern -> IO ()

webDirtStream :: IO WebDirtStream
webDirtStream = do
  webDirt <- newWebDirt
  now <- getCurrentTime
  mTempo <- newMVar (Tempo {at=now,beat=0.0,cps=1.0,paused=False,clockLatency=0.2})
  mPattern <- newMVar silence
  forkIO $ clockedTickWebDirt mTempo (webDirtTick webDirt mPattern)
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

webDirtTick :: T.JSVal -> MVar ParamPattern -> Tempo -> Int -> IO ()
webDirtTick webDirt patternM tempo ticks = do
  p <- readMVar patternM
  let ticks' = (fromIntegral ticks) :: Integer
      a = ticks' % webDirtTicksPerCycle
      b = (ticks' + 1) % webDirtTicksPerCycle
      events = seqToRelOnsets (a,b) p -- :: [(Double,Map Param (Maybe Value))]
      events' = Prelude.map (\(o,m) -> (f o,m)) events
  E.catch (mapM_ (tidalEventToWebDirt webDirt) events') (\msg -> putStrLn $ "exception: " ++ show (msg :: E.SomeException))
  where f x = logicalOnset' tempo ticks x 0

valueToJSVal :: Value -> T.JSVal
valueToJSVal (VI x) = P.pToJSVal x
valueToJSVal (VF x) = P.pToJSVal x
valueToJSVal (VS x) = P.pToJSVal x

-- ParamMap = Map Param (Maybe Value)

tidalEventToWebDirt :: T.JSVal -> (Double,ParamMap) -> IO ()
tidalEventToWebDirt webDirt (t,e) = do
  let e' = mapMaybe (id) e :: Map Param Value
      t' = P.pToJSVal t
      s = maybe (P.pToJSVal "bd") (valueToJSVal) (Data.Map.lookup s_p e')
      n = maybe (P.pToJSVal (0::Int)) (valueToJSVal) (Data.Map.lookup n_p e')
  putStrLn $ show t ++ " " ++ show e'
  playSample' webDirt t' s n
  return ()

{-

in Stream.hs:
type ToMessageFunc = Shape -> Tempo -> Int -> (Double, ParamMap) -> Maybe (IO ())

in OscStream.hs:
makeConnection :: String -> Int -> OscSlang -> IO (ToMessageFunc)
makeConnection returns a curried version of 'send' which uses logicalOnset

in Dirt.hs:
makeConnection is used in definition of dirtBackend :: IO Backend
the curried version of send is what is stored in the ToMessageFunc field of the Backend
in dirtStream, the backend is the first argument to stream

in Stream.hs:
stream :: Backend a -> Shape -> IO (ParamPattern -> IO ())
calls start (same args) which returns IO (MVar (ParamPattern)) (new mvar for pattern)
which forks clockedTick ticksPerCycle (onTick backend shape patternM)

in Tempo.hs:
clockedTick :: Int -> (Tempo -> Int -> IO()) -> IO ()
clockedTick basically just repeatedly calls the provided function with the
current tempo and tick

onTick :: Backend a -> Shape -> MVar ParamPattern -> Tempo -> Int -> IO ()
onTick    backend      shape    patternM             change   ticks
calls seqToRelOnsets (ticks%8,nextTick%8) thePattern
then mapMaybe (toMessage backend shape change ticks) on that...
in other words, it calls send :: Shape (shape) -> Tempo (change) -> Int (ticks)
 -> (Double,ParamMap) -> Maybe (IO ())

in OscStream.hs:
in the definition of send, logicalOnset is formed by calling
logicalOnset' change tick o ((latency shape) + nudge)

in Stream.hs:
logicalOnset' change tick o offset = logicalNow + (logicalPeriod * o) + offset
    where
      tpc = fromIntegral ticksPerCycle
      cycleD = ((fromIntegral tick) / tpc) :: Double
      -- i.e. current tick expressed in cycles
      logicalNow = logicalTime change cycleD
      -- i.e. POSIX time of current tick
      logicalPeriod = (logicalTime change (cycleD + (1/tpc))) - logicalNow
      -- POSIX time increment to next tick
      -- so overall return value is:
      -- POSIX time of current tick + o * duration of tick + an offset

in Tempo.hs:
logicalTime :: Tempo -> Double -> Double
given a tempo and a beat, returns the POSIX time of that beat

-}
