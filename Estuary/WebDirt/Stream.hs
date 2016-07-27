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

webDirtStream :: IO (ParamPattern -> IO ())
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
  E.catch (mapM_ (tidalEventToWebDirt webDirt) events) (\msg -> putStrLn $ "exception: " ++ show (msg :: E.SomeException))

valueToJSVal :: Value -> T.JSVal
valueToJSVal (VI x) = P.pToJSVal x
valueToJSVal (VF x) = P.pToJSVal x
valueToJSVal (VS x) = P.pToJSVal x

-- ParamMap = Map Param (Maybe Value)

tidalEventToWebDirt :: T.JSVal -> (Double,ParamMap) -> IO ()
tidalEventToWebDirt webDirt (t,e) = do
  putStrLn $ show t ++ show (keys e)
  let e' = mapMaybe (id) e :: Map Param Value
      s = maybe (P.pToJSVal "bd") (P.pToJSVal) (Data.Map.lookup s_p e')
      n = maybe (P.pToJSVal 0) (P.pToJSVal) (Data.Map.lookup n_p e')
  playSample' webDirt t s n
  return ()
