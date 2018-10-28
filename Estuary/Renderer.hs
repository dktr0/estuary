module Estuary.Renderer where

import Data.Time.Clock
import qualified Sound.Tidal.Context as Tidal
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Loops
import Data.Functor (void)
import Data.IntMap.Strict as IntMap
import Data.Maybe

import Estuary.Types.Context
import Estuary.Types.Definition
import Estuary.Types.TextNotation
import Estuary.Tidal.Types
import Estuary.Tidal.ParamPatternable
import Estuary.Types.Live
import Estuary.Languages.TidalParsers
import Estuary.WebDirt.SampleEngine
import Estuary.RenderInfo
import Estuary.RenderState
import Estuary.Types.Tempo

type Renderer = StateT RenderState IO ()

renderPeriod :: NominalDiffTime
renderPeriod = 0.2

flushEvents :: Context -> Renderer
flushEvents c = do
  events <- gets dirtEvents
  liftIO $ if webDirtOn c then sendSounds (webDirt c) events else return ()
  liftIO $ if superDirtOn c then sendSounds (superDirt c) events else return ()
  return ()

renderTidalPattern :: UTCTime -> NominalDiffTime -> Tempo -> Tidal.ParamPattern -> [(UTCTime,Tidal.ParamMap)]
renderTidalPattern start range t p = Prelude.map (\(o,_,m) -> (addUTCTime (realToFrac o*range) start,m)) events
  where
    start' = (realToFrac $ diffUTCTime start (at t)) * cps t + beat t -- start time in cycles since beginning of tempo
    end = realToFrac range * cps t + start' -- end time in cycles since beginning of tempo
    events = Tidal.seqToRelOnsetDeltas (toRational start',toRational end) p -- times expressed as fractions of start->range

-- definitionToPattern is here rather than in Estuary.Types.Definition so that the
-- server does not depend on mini-language parsers
definitionToPattern :: Definition -> Either String (Maybe Tidal.ParamPattern)
definitionToPattern (Structure x) = Right $ Just $ toParamPattern x
definitionToPattern (TextProgram x) = either Left (Right . Just) $ tidalTextToParamPattern $ forRendering x
definitionToPattern _ = Right $ Nothing

-- this is here rather than in Estuary.Types.TextNotation so that the server does not
-- depend on mini-language parsers
tidalTextToParamPattern :: (TextNotation,String) -> Either String Tidal.ParamPattern
tidalTextToParamPattern (TidalTextNotation x,y) = either (Left . show) Right $ tidalParser x y
tidalTextToParamPattern _ = Left "internal error: tidalTextToParamPattern called on unrecognized notation"

render :: Context -> Renderer
render c = do
  defsToPatterns c
  t1 <- liftIO $ getCurrentTime
  modify' $ (\x -> x { parseEndTime = t1 })
  patternsToDirtEvents c
  t2 <- liftIO $ getCurrentTime
  modify' $ (\x -> x { patternsEndTime = t2 })
  flushEvents c

defsToPatterns :: Context -> Renderer
defsToPatterns c = do
  s <- get
  let prevDefs = cachedDefs s
  let prevPatterns = paramPatterns s
  let prevErrors = errors (info s)
  let newDefs = fmap definitionForRendering $ definitions c
  modify' $ \x -> x { cachedDefs = newDefs }
  -- determine which definitions (for rendering purposes) have either changed or been deleted
  let additionsChanges = differenceWith (\x y -> if x == y then Nothing else Just x) newDefs prevDefs
  let deletions = difference prevDefs newDefs
  -- parse definitions into ParamPatterns or errors, add new ParamPatterns to previous patterns, delete patterns when defs deleted
  let (newErrors,newPatterns) = IntMap.mapEither definitionToPattern additionsChanges
  let newPatterns' = union (IntMap.mapMaybe id newPatterns) prevPatterns
  let newPatterns'' = difference newPatterns' deletions
  modify' $ \x -> x { paramPatterns = newPatterns'' }
  -- maintain map of errors by adding new errors, subtracting deleted defs and subtracting any for new successful ParamPatterns
  let newErrors' = union newErrors prevErrors
  let newErrors'' = difference newErrors' deletions
  let newErrors''' = difference newErrors'' newPatterns
  modify' $ \x -> x { info = (info s) { errors = newErrors''' } }

patternsToDirtEvents :: Context -> Renderer
patternsToDirtEvents c = do
  s <- get
  let lt = logicalTime s
  let tempo' = tempo c
  let ps = paramPatterns s
  let events = concat $ fmap (renderTidalPattern lt renderPeriod tempo') ps
  modify' $ \x -> x { dirtEvents = events }

runRender :: MVar Context -> MVar RenderInfo -> Renderer
runRender c ri = do
  t1 <- liftIO $ getCurrentTime
  modify' $ \x -> x { renderStartTime = t1 }
  c' <- liftIO $ readMVar c
  render c'
  t2 <- liftIO $ getCurrentTime
  modify' $ \x -> x { renderEndTime = t2 }
  calculateRenderTimes
  ri' <- gets info -- RenderInfo from the state maintained by this iteration...
  liftIO $ swapMVar ri ri' -- ...is copied to an MVar so it can be read elsewhere.
  sleepUntilNextRender

sleepUntilNextRender :: Renderer
sleepUntilNextRender = do
  s <- get
  let next = addUTCTime renderPeriod (logicalTime s)
  let diff = diffUTCTime next (renderEndTime s)
  next' <- liftIO $ if diff > 0 then return next else do
    putStrLn "*** logical time too far behind clock time - fast forwarding"
    return $ addUTCTime (diff * (-1) + 0.01) next -- fast forward so next logical time is 10 milliseconds after clock time
  let diff' = diffUTCTime next' (renderEndTime s)
  next'' <- liftIO $ if diff' < (renderPeriod*2) then return next' else do -- not allowed to get more than 1 render period ahead
    putStrLn "*** logical time too far ahead of clock time - rewinding"
    return $ addUTCTime renderPeriod $ renderEndTime s
  let diff'' = diffUTCTime next'' (renderEndTime s)
  let delay = floor $ realToFrac diff'' * 1000000 - 10000 -- ie. wakeup ~ 10 milliseconds before next logical time
  liftIO $ threadDelay delay
  put $ s { logicalTime = next'' }

calculateRenderTimes :: Renderer
calculateRenderTimes = do
  s <- get
  --
  let renderTime = diffUTCTime (renderEndTime s) (renderStartTime s)
  let newRenderTimes = take 20 $ renderTime:(renderTimes s)
  let newAvgRenderTime = sum newRenderTimes / (fromIntegral $ length newRenderTimes)
  let newPeakRenderTime = maximum newRenderTimes
  let newAvgRenderLoad = ceiling (newAvgRenderTime * 100 / renderPeriod)
  let newPeakRenderLoad = ceiling (newPeakRenderTime * 100 / renderPeriod)
  modify' $ \x -> x { renderTimes = newRenderTimes }
  modify' $ \x -> x { info = (info x) { avgRenderLoad = newAvgRenderLoad }}
  modify' $ \x -> x { info = (info x) { peakRenderLoad = newPeakRenderLoad }}
  --
  let parseTime = diffUTCTime (parseEndTime s) (renderStartTime s)
  let newParseTimes = take 20 $ parseTime:(parseTimes s)
  let newAvgParseTime = sum newParseTimes / (fromIntegral $ length newParseTimes)
  let newPeakParseTime = maximum newParseTimes
  let newAvgParseLoad = ceiling (newAvgParseTime * 100 / renderPeriod)
  let newPeakParseLoad = ceiling (newPeakParseTime * 100 / renderPeriod)
  modify' $ \x -> x { parseTimes = newParseTimes }
  modify' $ \x -> x { info = (info x) { avgParseLoad = newAvgParseLoad }}
  modify' $ \x -> x { info = (info x) { peakParseLoad = newPeakParseLoad }}
  --
  let patternsTime = diffUTCTime (patternsEndTime s) (parseEndTime s)
  let newPatternsTimes = take 20 $ patternsTime:(patternsTimes s)
  let newAvgPatternsTime = sum newPatternsTimes / (fromIntegral $ length newPatternsTimes)
  let newPeakPatternsTime = maximum newPatternsTimes
  let newAvgPatternsLoad = ceiling (newAvgPatternsTime * 100 / renderPeriod)
  let newPeakPatternsLoad = ceiling (newPeakPatternsTime * 100 / renderPeriod)
  modify' $ \x -> x { patternsTimes = newPatternsTimes }
  modify' $ \x -> x { info = (info x) { avgPatternsLoad = newAvgPatternsLoad }}
  modify' $ \x -> x { info = (info x) { peakPatternsLoad = newPeakPatternsLoad }}

forkRenderThread :: MVar Context -> MVar RenderInfo -> IO ()
forkRenderThread c ri = do
  renderStart <- getCurrentTime
  void $ forkIO $ iterateM_ (execStateT $ runRender c ri) (initialRenderState renderStart)
