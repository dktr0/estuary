module Estuary.WebDirt.SampleEngine where

import Reflex
import Reflex.Dom
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Sound.Tidal.Context as Tidal
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Loops
import Data.Functor (void)
import Data.Map
import Data.Maybe

import Estuary.Types.Live
import Estuary.Types.Context
import Estuary.Types.Definition
import Estuary.Types.TextNotation
import Estuary.Tidal.Types

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
  -- putStrLn $ show sounds'
  catch (mapM_ (playSample e) sounds')
    (\msg -> putStrLn $ "exception: " ++ show (msg :: SomeException))

type RenderM = StateT RenderState IO

flushEvents :: (SampleEngine wd, SampleEngine sd) => wd -> sd -> Context -> RenderM ()
flushEvents wd sd c = do
  events <- gets dirtEvents
  liftIO $ if webDirtOn c then sendSounds wd (tempo c) events else return ()
  liftIO $ if superDirtOn c then sendSounds sd (tempo c) events else return ()
  return ()

-- the old Estuary renderer re-parses every definition every frame - not very efficient!...
oldRenderer :: (SampleEngine wd, SampleEngine sd) => wd -> sd -> Context -> RenderM ()
oldRenderer wd sd c = do
  oldDefsToPatterns c
  t1 <- liftIO $ getCurrentTime
  modify $ (\x -> x { parseEndTime = t1 })
  oldPatternsToDirtEvents c
  t2 <- liftIO $ getCurrentTime
  modify $ (\x -> x { patternsToEventsEndTime = t2 })
  flushEvents wd sd c

oldDefsToPatterns :: Context -> RenderM ()
oldDefsToPatterns c = do
  s <- get
  let patterns = Data.Map.mapMaybe definitionToPattern $ definitions c
  put $ s { paramPatterns = patterns }

oldPatternsToDirtEvents :: Context -> RenderM ()
oldPatternsToDirtEvents c = do
  s <- get
  let patterns = paramPatterns s
  let sounds = concat $ fmap (renderTidalPattern (logicalTime s) (0.1::NominalDiffTime) (tempo c)) patterns
  put $ s { dirtEvents = sounds }

-- the new Estuary renderer re-parses only when a definition changes, caching results
newRenderer :: (SampleEngine wd, SampleEngine sd) => wd -> sd -> Context -> RenderM ()
newRenderer wd sd c = do
  defsToPatterns c
  t1 <- liftIO $ getCurrentTime
  modify $ (\x -> x { parseEndTime = t1 })
  patternsToDirtEvents c
  t2 <- liftIO $ getCurrentTime
  modify $ (\x -> x { patternsToEventsEndTime = t2 })
  flushEvents wd sd c

defaultRenderer :: (SampleEngine wd, SampleEngine sd) => wd -> sd -> Context -> RenderM ()
defaultRenderer = newRenderer

defsToPatterns :: Context -> RenderM ()
defsToPatterns c = do
  s <- get
  let prevDefs = cachedDefs s
  let prevPatterns = paramPatterns s
  let newDefs = definitions c
  let additionsChanges = differenceWith (\x y -> if x == y then Nothing else Just x) newDefs prevDefs
  let newPatterns = Data.Map.mapMaybe definitionToPattern additionsChanges
  let newPatterns' = union newPatterns prevPatterns
  let deletions = difference prevDefs newDefs
  let newPatterns'' = difference newPatterns' deletions
  put $ s { paramPatterns = newPatterns'', cachedDefs = newDefs }

patternsToDirtEvents :: Context -> RenderM ()
patternsToDirtEvents c = do
  s <- get
  let lt = logicalTime s
  let tempo' = tempo c
  let ps = paramPatterns s
  let events = concat $ fmap (renderTidalPattern lt (0.1::NominalDiffTime) tempo') ps
  put $ s { dirtEvents = events }

data RenderState = RenderState {
  logicalTime :: UTCTime,
  cachedDefs :: DefinitionMap,
  paramPatterns :: Map Int Tidal.ParamPattern,
  -- defStatusMap :: Map Int DefinitionStatus,
  dirtEvents :: [(UTCTime,Tidal.ParamMap)],
  renderStartTime :: UTCTime,
  parseEndTime :: UTCTime,
  patternsToEventsEndTime :: UTCTime,
  renderEndTime :: UTCTime,
  renderTimes :: [NominalDiffTime],
  avgRenderTime :: NominalDiffTime
  }

initialRenderState :: UTCTime -> RenderState
initialRenderState t = RenderState {
  logicalTime = t,
  cachedDefs = empty,
  paramPatterns = empty,
  dirtEvents = [],
  renderStartTime = t,
  parseEndTime = t,
  patternsToEventsEndTime = t,
  renderEndTime = t,
  renderTimes = [],
  avgRenderTime = 0
  }

type Renderer = RenderM ()

dynamicRender :: (SampleEngine wd, SampleEngine sd, MonadWidget t m) => MVar Renderer -> wd -> sd -> Dynamic t Context -> m ()
dynamicRender r wd sd c = performEvent_ $ fmap (liftIO . void . swapMVar r .  defaultRenderer wd sd) $ updated c

runRender :: MVar Renderer -> RenderM ()
runRender r = do
  t1 <- liftIO $ getCurrentTime
  modify $ \x -> x { renderStartTime = t1 }
  currentRenderer <- liftIO $ readMVar r
  currentRenderer
  t2 <- liftIO $ getCurrentTime
  modify $ \x -> x { renderEndTime = t2 }
  calculateRenderTimes
  sleepUntilNextRender

sleepUntilNextRender :: RenderM ()
sleepUntilNextRender = do
  s <- get
  let next = addUTCTime (0.1::NominalDiffTime) (logicalTime s)
  let diff = diffUTCTime next (renderEndTime s)
  let delay = floor $ realToFrac diff * 1000000 - 10000 -- ie. wakeup ~ 10 milliseconds before next logical time
  liftIO $ threadDelay delay
  put $ s { logicalTime = next }

calculateRenderTimes :: RenderM ()
calculateRenderTimes = do
  s <- get
  let renderTime = diffUTCTime (renderEndTime s) (renderStartTime s)
  let newRenderTimes = take 50 $ renderTime:(renderTimes s)
  let newAvgRenderTime = sum newRenderTimes / (fromIntegral $ length newRenderTimes)
  -- liftIO $ putStrLn $ show newAvgRenderTime
  put $ s {
    renderTimes = newRenderTimes,
    avgRenderTime = newAvgRenderTime
    }

forkRenderThread :: MVar Renderer -> IO ()
forkRenderThread r = do
  renderStart <- getCurrentTime
  void $ forkIO $ iterateM_ (execStateT $ runRender r) (initialRenderState renderStart)
