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
import Data.Map (elems)

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

oldRenderer :: (SampleEngine wd, SampleEngine sd) => wd -> sd -> Context -> RenderState -> IO RenderState
oldRenderer wd sd c s = do
  let defs = elems $ definitions c
  let patterns1 = (fmap toParamPattern . justStructures) defs
  let patterns2 = (fmap (tidalTextToParamPattern . forRendering) . justTextPrograms) defs
  let patterns = patterns1 ++ patterns2
  let sounds = concat $ fmap (renderTidalPattern (logicalTime s) (0.1::NominalDiffTime) (tempo c)) patterns
  if webDirtOn c then sendSounds wd (tempo c) sounds else return ()
  if superDirtOn c then sendSounds sd (tempo c) sounds else return ()
  return s

newRenderer :: (SampleEngine wd, SampleEngine sd) => wd -> sd -> Context -> RenderState -> IO RenderState
newRenderer wd sd c s = do
  let s' = execState (purePartOfNewRender c) s
  if webDirtOn c then sendSounds wd (tempo c) (dirtEvents s') else return ()
  if superDirtOn c then sendSounds sd (tempo c) (dirtEvents s') else return ()
  return s' { dirtEvents = [] }

purePartOfNewRender :: Context -> State RenderState ()
purePartOfNewRender c = defsToPatterns c >> patternsToDirtEvents c

defsToPatterns :: Context -> State RenderState ()
defsToPatterns c = do
  prevDefs <- gets cachedDefs

  let changes = differenceWith (\old new -> if old == new   ) prevDefs newDefs
  differenceWith :: Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a


  let patterns1 = (fmap toParamPattern . justStructures) changed
  let patterns2 = (fmap (tidalTextToParamPattern . forRendering) . justTextPrograms) changed
  let patterns3 =

patternsToDirtEvents :: Context -> RenderState

data RenderState = RenderState {
  logicalTime :: UTCTime,
  cachedDefs :: DefinitionMap, -- only defs that have changed affect newRender
  paramPatterns :: Map Int Tidal.ParamPattern,
  -- defStatusMap :: Map Int DefinitionStatus
  dirtEvents :: [(UTCTime,Tidal.ParamMap)] -- for now just a pile, but later we may want to keep separate by source
  renderTimes :: [NominalDiffTime],
  avgRenderTime :: NominalDiffTime
  }



type Renderer = RenderState -> IO RenderState

dynamicRender :: (SampleEngine wd, SampleEngine sd, MonadWidget t m) => MVar Renderer -> wd -> sd -> Dynamic t Context -> m ()
dynamicRender r wd sd c = performEvent_ $ fmap (liftIO . void . swapMVar r .  oldRenderer wd sd) $ updated c

runRender :: MVar Renderer -> RenderState -> IO RenderState
runRender r s = do
  r' <- readMVar r
  t1 <- getCurrentTime
  let s' = r' s
  t2 <- getCurrentTime
  let renderTime = diffUTCTime t2 t1
  let newRenderTimes = take 10 $ renderTime:renderTimes s'
  let newAvgRenderTime = sum newRenderTimes / (fromIntegral $ length newRenderTimes)
  putStrLn $ show newAvgRenderTime
  let next = addUTCTime (0.1::NominalDiffTime) (logicalTime s')
  let diff = diffUTCTime next t2
  let delay = floor $ realToFrac diff * 1000000 - 10000 -- ie. wakeup ~ 10 milliseconds before next logical time
  -- putStrLn $ "now=" ++ show now ++ "  next=" ++ show next ++ "  diff=" ++ show diff ++ "  delay=" ++ show delay
  threadDelay delay
  return $ s' {
    logicalTime = next,
    renderTimes = newRenderTimes,
    avgRenderTime = newAvgRenderTime
    }

renderThread :: MVar Renderer -> IO ()
renderThread r = do
  renderStart <- getCurrentTime
  let initialRenderState = RenderState {
    logicalTime = renderStart,
    renderTimes = [],
    avgRenderTime = 0::NominalDiffTime
    }
  forkIO $ iterateM_ (runRender r) initialRenderState
  return ()
