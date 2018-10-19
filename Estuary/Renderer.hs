module Estuary.Renderer where

import Data.Time.Clock
import qualified Sound.Tidal.Context as Tidal
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Loops
import Data.Functor (void)
import Data.Map
import Data.Maybe

import Estuary.Types.Context
import Estuary.Types.Definition
import Estuary.Types.TextNotation
import Estuary.Tidal.Types
import Estuary.Types.Live
import Estuary.Languages.TidalParsers
import Estuary.WebDirt.SampleEngine
import Estuary.RenderState

type Renderer = StateT RenderState IO ()

flushEvents :: Context -> Renderer
flushEvents c = do
  events <- gets dirtEvents
  liftIO $ if webDirtOn c then sendSounds (webDirt c) (tempo c) events else return ()
  liftIO $ if superDirtOn c then sendSounds (superDirt c) (tempo c) events else return ()
  return ()

renderTidalPattern :: UTCTime -> NominalDiffTime -> Tidal.Tempo -> Tidal.ParamPattern -> [(UTCTime,Tidal.ParamMap)]
renderTidalPattern start range t p = Prelude.map (\(o,_,m) -> (addUTCTime (realToFrac o*range) start,m)) events
  where
    start' = (realToFrac $ diffUTCTime start (Tidal.at t)) * Tidal.cps t + Tidal.beat t -- start time in cycles since beginning of tempo
    end = realToFrac range * Tidal.cps t + start' -- end time in cycles since beginning of tempo
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
  modify $ (\x -> x { parseEndTime = t1 })
  patternsToDirtEvents c
  t2 <- liftIO $ getCurrentTime
  modify $ (\x -> x { patternsToEventsEndTime = t2 })
  flushEvents c

defsToPatterns :: Context -> Renderer
defsToPatterns c = do
  s <- get
  let prevDefs = cachedDefs s
  let prevPatterns = paramPatterns s
  let prevErrors = errors s
  let newDefs = definitions c
  --
  let additionsChanges = differenceWith (\x y -> if x == y then Nothing else Just x) newDefs prevDefs
  let deletions = difference prevDefs newDefs
  --
  let (newErrors,newPatterns) = Data.Map.mapEither definitionToPattern additionsChanges
  let newPatterns' = union (Data.Map.mapMaybe id newPatterns) prevPatterns
  let newPatterns'' = difference newPatterns' deletions
  let newErrors' = union newErrors prevErrors
  let newErrors'' = difference newErrors' deletions
  let newErrors''' = difference newErrors'' newPatterns''
  put $ s { paramPatterns = newPatterns'', errors = newErrors''', cachedDefs = newDefs }

patternsToDirtEvents :: Context -> Renderer
patternsToDirtEvents c = do
  s <- get
  let lt = logicalTime s
  let tempo' = tempo c
  let ps = paramPatterns s
  let events = concat $ fmap (renderTidalPattern lt (0.1::NominalDiffTime) tempo') ps
  put $ s { dirtEvents = events }

runRender :: MVar Context -> MVar RenderState -> Renderer
runRender c s = do
  t1 <- liftIO $ getCurrentTime
  modify $ \x -> x { renderStartTime = t1 }
  c' <- liftIO $ readMVar c
  render c'
  t2 <- liftIO $ getCurrentTime
  modify $ \x -> x { renderEndTime = t2 }
  calculateRenderTimes
  s' <- get -- get the final state...
  liftIO $ swapMVar s s' -- and copy it into MVar so widgets can reflect it as necessary
  sleepUntilNextRender

sleepUntilNextRender :: Renderer
sleepUntilNextRender = do
  s <- get
  let next = addUTCTime (0.1::NominalDiffTime) (logicalTime s)
  let diff = diffUTCTime next (renderEndTime s)
  let delay = floor $ realToFrac diff * 1000000 - 10000 -- ie. wakeup ~ 10 milliseconds before next logical time
  liftIO $ threadDelay delay
  put $ s { logicalTime = next }

calculateRenderTimes :: Renderer
calculateRenderTimes = do
  s <- get
  let renderTime = diffUTCTime (renderEndTime s) (renderStartTime s)
  let newRenderTimes = take 50 $ renderTime:(renderTimes s)
  let newAvgRenderTime = sum newRenderTimes / (fromIntegral $ length newRenderTimes)
  put $ s { renderTimes = newRenderTimes, avgRenderTime = newAvgRenderTime }

forkRenderThread :: MVar Context -> MVar RenderState -> IO ()
forkRenderThread c s = do
  renderStart <- getCurrentTime
  void $ forkIO $ iterateM_ (execStateT $ runRender c s) (initialRenderState renderStart)
