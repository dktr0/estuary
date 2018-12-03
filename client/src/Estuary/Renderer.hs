module Estuary.Renderer where

import Data.Time.Clock
import qualified Sound.Tidal.Context as Tidal
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Loops
import Data.Functor (void)
import Data.List (intercalate,zipWith4)
import Data.IntMap.Strict as IntMap
import Data.Maybe
import Data.Either

import qualified Sound.Punctual.PunctualW as Punctual
import qualified Sound.Punctual.Evaluation as Punctual
import qualified Sound.Punctual.Types as Punctual
import qualified Sound.Punctual.Parser as Punctual
import qualified Sound.Punctual.Sample as Punctual
import qualified Estuary.Languages.SvgOp as SvgOp
import qualified Estuary.Languages.CanvasOp as CanvasOp
import qualified Estuary.Types.CanvasOp as CanvasOp
import Estuary.Types.Color

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
  -- flush events for SuperDirt and WebDirt
  events <- gets dirtEvents
  liftIO $ if webDirtOn c then sendSounds (webDirt c) events else return ()
  liftIO $ if superDirtOn c then sendSounds (superDirt c) events else return ()
  -- flush CanvasOps to an MVar queue (list)
  oldOps <- liftIO $ takeMVar $ canvasOpsQueue c
  newOps <- gets canvasOps
  liftIO $ putMVar (canvasOpsQueue c) (oldOps ++ newOps)
  modify' $ \x -> x { dirtEvents = [], canvasOps = []}
  return ()

renderTidalPattern :: UTCTime -> NominalDiffTime -> Tempo -> Tidal.ControlPattern -> [(UTCTime,Tidal.ControlMap)]
renderTidalPattern start range t p = events''
  where
    start' = (realToFrac $ diffUTCTime start (at t)) * cps t + beat t -- start time in cycles since beginning of tempo
    end = realToFrac range * cps t + start' -- end time in cycles since beginning of tempo
    events = Tidal.queryArc p (toRational start',toRational end) -- events with t in cycles
    events' = Prelude.filter Tidal.eventHasOnset events
    events'' = f <$> events'
    f (((w1,_),(_,_)),cMap) = (addUTCTime (realToFrac ((fromRational w1 - beat t)/cps t)) (at t),cMap)

sequenceToControlPattern :: (String,[Bool]) -> Tidal.ControlPattern
sequenceToControlPattern (sampleName,pat) = Tidal.s $ parseBP' $ intercalate " " $ fmap f pat
  where f False = "~"
        f True = sampleName


render :: Context -> Renderer
render c = do
  traverseWithKey (renderZone c) (definitions c)
  flushEvents c


renderZone :: Context -> Int -> Definition -> Renderer
renderZone c z d = do
  s <- get
  let prevDef = IntMap.lookup z $ cachedDefs s
  let d' = definitionForRendering d
  when (prevDef /= (Just d')) $ renderZoneChanged c z d'
  modify' $ \x -> x { cachedDefs = insert z d' (cachedDefs s) }
  renderZoneAlways c z d'


renderZoneChanged :: Context -> Int -> Definition -> Renderer
renderZoneChanged c z (Structure x) = do
  let newParamPattern = toParamPattern x
  s <- get
  modify' $ \x -> x { paramPatterns = insert z newParamPattern (paramPatterns s) }
renderZoneChanged c z (TextProgram x) = renderTextProgramChanged c z $ forRendering x
renderZoneChanged c z (Sequence xs) = do
  let newParamPattern = Tidal.stack $ fmap sequenceToControlPattern xs
  s <- get
  modify' $ \x -> x { paramPatterns = insert z newParamPattern (paramPatterns s) }
renderZoneChanged _ _ _ = return ()

renderZoneAlways :: Context -> Int -> Definition -> Renderer
renderZoneAlways c z (Structure _) = renderControlPattern c z
renderZoneAlways c z (TextProgram x) = renderTextProgramAlways c z $ forRendering x
renderZoneAlways c z (Sequence _) = renderControlPattern c z
renderZoneAlways _ _ _ = return ()


renderTextProgramChanged :: Context -> Int -> (TextNotation,String) -> Renderer
renderTextProgramChanged c z (TidalTextNotation x,y) = do
  s <- get
  let parseResult = tidalParser x y -- :: Either ParseError ControlPattern
  let newParamPatterns = either (const $ paramPatterns s) (\p -> insert z p (paramPatterns s)) parseResult
  liftIO $ either (putStrLn . show) (const $ return ()) parseResult -- print new errors to console
  let newErrors = either (\e -> insert z (show e) (errors (info s))) (const $ delete z (errors (info s))) parseResult
  modify' $ \x -> x { paramPatterns = newParamPatterns, info = (info s) { errors = newErrors} }


renderTextProgramChanged c z (PunctualAudio,x) = do
  s <- get
  let parseResult = Punctual.runPunctualParser x
  if isLeft parseResult then return () else do
    let exprs = either (const []) id parseResult
    t <- liftIO $ getCurrentTime
    let eval = (exprs,t)
    let prevPunctualW = findWithDefault (Punctual.emptyPunctualW t) z (punctuals s)
    newPunctualW <- liftIO $ Punctual.updatePunctualW prevPunctualW eval
    modify' $ \x -> x { punctuals = insert z newPunctualW (punctuals s)}
  let newErrors = either (\e -> insert z (show e) (errors (info s))) (const $ delete z (errors (info s))) parseResult
  modify' $ \x -> x { info = (info s) { errors = newErrors }}

renderTextProgramChanged c z (PunctualVideo,x) = do
  s <- get
  let parseResult = Punctual.runPunctualParser x
  if isLeft parseResult then return () else do
    let exprs = either (const []) id parseResult
    t <- liftIO $ getCurrentTime
    let eval = (exprs,t)
    let prevPunctualVideo = findWithDefault (Punctual.emptyPunctualState t) z (punctualVideo s)
    let newPunctualVideo = Punctual.updatePunctualState prevPunctualVideo eval
    modify' $ \x -> x { punctualVideo = insert z newPunctualVideo (punctualVideo s)}
  let newErrors = either (\e -> insert z (show e) (errors (info s))) (const $ delete z (errors (info s))) parseResult
  modify' $ \x -> x { info = (info s) { errors = newErrors }}

renderTextProgramChanged c z (SvgOp,x) = do
  s <- get
  let parseResult = SvgOp.svgOp x
  let ops = either (const Nothing) Just parseResult
  let errs = either (\e -> insert z (show e) (errors (info s))) (const $ delete z (errors (info s))) parseResult
  modify' $ \x -> x { info = (info s) { errors = errs, svgOps = ops }}

renderTextProgramChanged c z (CanvasOp,x) = do
  s <- get
  let parseResult = CanvasOp.canvasOp x
  let ops = either (const []) (fmap (\op -> (logicalTime s, op))) parseResult
  let errs = either (\e -> insert z (show e) (errors (info s))) (const $ delete z (errors (info s))) parseResult
  modify' $ \x -> x { info = (info s) { errors = errs }, canvasOps = canvasOps s ++ ops }

renderTextProgramChanged _ _ _ = return ()

renderTextProgramAlways :: Context -> Int -> (TextNotation,String) -> Renderer
renderTextProgramAlways c z (TidalTextNotation _,_) = renderControlPattern c z
renderTextProgramAlways c z (PunctualVideo,_) = renderPunctualVideo c z
renderTextProgramAlways _ _ _ = return ()

renderPunctualVideo :: Context -> Int -> Renderer
renderPunctualVideo c z = do
  s <- get
  let pv = IntMap.lookup z $ punctualVideo s
  let lt = logicalTime s
  let newOps = maybe [] id $ fmap (punctualVideoToOps lt renderPeriod) pv
  let newOps' = fmap (\(t,e) -> (addUTCTime 0.2 t,e)) newOps -- add latency
  modify' $ \x -> x { canvasOps = canvasOps s ++ newOps' }

punctualVideoToOps :: UTCTime -> NominalDiffTime -> Punctual.PunctualState -> [(UTCTime,CanvasOp.CanvasOp)]
punctualVideoToOps lt p s = concat $ zipWith4 (\c d e f -> [c,d,e,f]) clears strokes fills rects
  where
    n = 30 :: Int -- how many sampling/drawing operations per renderPeriod
    ts = fmap (flip addUTCTime $ lt) $ fmap ((*(renderPeriod/(fromIntegral n :: NominalDiffTime))) . fromIntegral) [0 .. n]
    clear = fmap biPolarToPercent $! sampleWithDefault "clear" (-1) s ts
    r = fmap biPolarToPercent $! sampleWithDefault "r" 1 s ts
    g = fmap biPolarToPercent $! sampleWithDefault "g" 1 s ts
    b = fmap biPolarToPercent $! sampleWithDefault "b" 1 s ts
    a = fmap biPolarToPercent $! sampleWithDefault "a" 1 s ts
    x = fmap biPolarToPercent $! sampleWithDefault "x" 0 s ts
    y = fmap biPolarToPercent $! sampleWithDefault "y" 0 s ts
    w = fmap biPolarToPercent $! sampleWithDefault "w" (-0.995) s ts
    h = fmap biPolarToPercent $! sampleWithDefault "h" (-0.995) s ts
    clears = zip ts $ fmap CanvasOp.Clear clear
    strokes = zip ts $ fmap CanvasOp.StrokeStyle $ zipWith4 RGBA r g b a
    fills = zip ts $ fmap CanvasOp.FillStyle $ zipWith4 RGBA r g b a
    rects = zip ts $ zipWith4 CanvasOp.Rect x y w h

prependLogicalTime :: UTCTime -> a -> (UTCTime,a)
prependLogicalTime lt a = (lt,a)

sampleWithDefault :: String -> Double -> Punctual.PunctualState -> [UTCTime] -> [Double]
sampleWithDefault targetName d s ts = maybe (replicate (length ts) d) f $ Punctual.findGraphForTarget targetName s
  where f g = fmap (\t -> Punctual.sampleGraph (Punctual.startTime s) t 0 g) ts

biPolarToPercent :: Double -> Double
biPolarToPercent x = (x + 1) * 50

renderControlPattern :: Context -> Int -> Renderer
renderControlPattern c z = do
  s <- get
  let controlPattern = IntMap.lookup z $ paramPatterns s -- :: Maybe ControlPattern
  let lt = logicalTime s
  let tempo' = tempo c
  let events = maybe [] id $ fmap (renderTidalPattern lt renderPeriod tempo') controlPattern
  modify' $ \x -> x { dirtEvents = (dirtEvents s) ++ events }


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

forkRenderThread :: MVar Context -> MVar RenderInfo -> IO ()
forkRenderThread c ri = do
  renderStart <- getCurrentTime
  void $ forkIO $ iterateM_ (execStateT $ runRender c ri) (initialRenderState renderStart)
