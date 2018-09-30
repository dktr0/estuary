module Estuary.WebDirt.SampleEngine where

import Reflex
import Reflex.Dom
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Sound.Tidal.Context as Tidal
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Loops
import Data.Functor (void)
import Estuary.Types.Context

class SampleEngine e where
  getClockDiff :: e -> IO Double -- diff btw clock used to play sample events and POSIX
  playSample :: e -> (Double,Tidal.ParamMap) -> IO ()
  getPeakLevels :: e -> IO [Double]
  getRmsLevels :: e -> IO [Double]

renderTidalPattern :: UTCTime -> NominalDiffTime -> Tidal.Tempo -> Tidal.ParamPattern -> [(UTCTime,Tidal.ParamMap)]
renderTidalPattern start range t p = Prelude.map (\(o,_,m) -> (addUTCTime (realToFrac o*range) start,m)) events
  where
    start' = (realToFrac $ diffUTCTime start (Tidal.at t)) * Tidal.cps t + Tidal.beat t -- start time in cycles since beginning of tempo
    end = realToFrac range * Tidal.cps t + start' -- end time in cycles since beginning of tempo
    events = Tidal.seqToRelOnsetDeltas (toRational start',toRational end) p -- times expressed as fractions of start->range

sendSounds :: SampleEngine e => e -> Tidal.Tempo -> [(UTCTime,Tidal.ParamMap)] -> IO ()
sendSounds e t sounds = do
  clockDiff <- getClockDiff e
  let latency = Tidal.clockLatency t
  let sounds' = fmap (\(x,y) -> (realToFrac (utcTimeToPOSIXSeconds x) - clockDiff + latency,y)) sounds
  putStrLn $ show sounds'
  catch (mapM_ (playSample e) sounds')
    (\msg -> putStrLn $ "exception: " ++ show (msg :: SomeException))


type Renderer = UTCTime -> IO ()

render :: (SampleEngine wd, SampleEngine sd) => wd -> sd -> Context -> Renderer
render wd sd c logicalTime = do
  let sounds = renderTidalPattern logicalTime (0.1::NominalDiffTime) (tempo c) (pattern c)
  if webDirtOn c then sendSounds wd (tempo c) sounds else return ()
  if superDirtOn c then sendSounds sd (tempo c) sounds else return ()

dynamicRender :: (SampleEngine wd, SampleEngine sd, MonadWidget t m) => MVar Renderer -> wd -> sd -> Dynamic t Context -> m () 
dynamicRender r wd sd c = performEvent_ $ fmap (liftIO . void . swapMVar r .  render wd sd) $ updated c

runRender :: MVar Renderer -> UTCTime -> IO UTCTime
runRender r t = do
  r' <- readMVar r
  r' t
  now <- getCurrentTime
  let next = addUTCTime (0.1::NominalDiffTime) t
  let diff = diffUTCTime next now
  let delay = floor $ realToFrac diff * 1000000 - 10000 -- ie. wakeup ~ 10 milliseconds before next logical time   
  -- putStrLn $ "now=" ++ show now ++ "  next=" ++ show next ++ "  diff=" ++ show diff ++ "  delay=" ++ show delay
  threadDelay delay
  return next

renderThread :: MVar Renderer -> IO ()
renderThread r = do
  renderStart <- getCurrentTime
  forkIO $ iterateM_ (runRender r) renderStart
  return ()

