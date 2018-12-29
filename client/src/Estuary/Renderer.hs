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
import qualified Data.Map as Map

import qualified Sound.Punctual.PunctualW as Punctual
import qualified Sound.Punctual.Evaluation as Punctual
import qualified Sound.Punctual.Types as Punctual
import qualified Sound.Punctual.Parser as Punctual
import qualified Sound.Punctual.Sample as Punctual
import qualified Estuary.Languages.SuperContinent as SuperContinent
import qualified Estuary.Languages.SvgOp as SvgOp
import qualified Estuary.Languages.CanvasOp as CanvasOp
import qualified Estuary.Types.CanvasOp as CanvasOp
import Estuary.Types.CanvasState
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
renderPeriod = 0.032

flushEvents :: Context -> Renderer
flushEvents c = do
  -- flush events for SuperDirt and WebDirt
  events <- gets dirtEvents
  liftIO $ if webDirtOn c then sendSounds (webDirt c) events else return ()
  liftIO $ if superDirtOn c then sendSounds (superDirt c) events else return ()
  -- flush CanvasOps to an MVar queue (list)
  when (canvasOn c) $ do
    oldCvsState <- liftIO $ takeMVar $ canvasState c
    newOps <- gets canvasOps
    liftIO $ putMVar (canvasState c) $ pushCanvasOps newOps oldCvsState
  modify' $ \x -> x { dirtEvents = [], canvasOps = []}
  return ()

renderTidalPattern :: UTCTime -> NominalDiffTime -> Tempo -> Tidal.ControlPattern -> [(UTCTime,Tidal.ControlMap)]
renderTidalPattern start range t p = events''
  where
    start' = (realToFrac $ diffUTCTime start (at t)) * cps t + beat t -- start time in cycles since beginning of tempo
    end = realToFrac range * cps t + start' -- end time in cycles since beginning of tempo
    events = Tidal.queryArc p (Tidal.Arc (toRational start') (toRational end)) -- events with t in cycles
    events' = Prelude.filter Tidal.eventHasOnset events
    events'' = f <$> events'
    f e = (utcTime,Tidal.event e)
      where
        utcTime = addUTCTime (realToFrac ((fromRational w1 - beat t)/cps t)) (at t)
        w1 = Tidal.start $ Tidal.whole e

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
  let newParamPattern = Tidal.stack $ Map.elems $ Map.map sequenceToControlPattern xs
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

renderTextProgramChanged c z (SuperContinent,x) = do
  s <- get
  let parseResult = SuperContinent.parseSuperContinent x
  let newProgram = either (const $ superContinentProgram s) id parseResult
  let newErrors = either (\e -> insert z (show e) (errors (info s))) (const $ delete z (errors (info s))) parseResult
  modify' $ \x -> x { superContinentProgram = newProgram, info = (info s) { errors = newErrors } }

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
renderTextProgramAlways c z (SuperContinent,_) = renderSuperContinent c z
renderTextProgramAlways c z (PunctualVideo,_) = renderPunctualVideo c z
renderTextProgramAlways _ _ _ = return ()

renderSuperContinent :: Context -> Int -> Renderer
renderSuperContinent c z = when (canvasOn c) $ do
  s <- get
  let audio = 0.5 -- placeholder
  let program = superContinentProgram s
  let scState = superContinentState s
  scState1 <- liftIO $ SuperContinent.runProgram audio program scState
--  scState2 <- liftIO $ SuperContinent.runProgram audio program scState1
--  scState3 <- liftIO $ SuperContinent.runProgram audio program scState2
--  scState4 <- liftIO $ SuperContinent.runProgram audio program scState3
--  scState5 <- liftIO $ SuperContinent.runProgram audio program scState4
--  scState6 <- liftIO $ SuperContinent.runProgram audio program scState5
  let newOps1 = SuperContinent.stateToCanvasOps scState1
--  let newOps2 = SuperContinent.stateToCanvasOps scState2
--  let newOps3 = SuperContinent.stateToCanvasOps scState3
--  let newOps4 = SuperContinent.stateToCanvasOps scState4
--  let newOps5 = SuperContinent.stateToCanvasOps scState5
--  let newOps6 = SuperContinent.stateToCanvasOps scState6
  let newOps1' = fmap (\o -> (addUTCTime 0.2 (logicalTime s),o)) newOps1
--  let newOps2' = fmap (\o -> (addUTCTime (0.2 + (renderPeriod*0.166)) (logicalTime s),o)) newOps2
--  let newOps3' = fmap (\o -> (addUTCTime (0.2 + (renderPeriod*0.333)) (logicalTime s),o)) newOps3
--  let newOps4' = fmap (\o -> (addUTCTime (0.2 + (renderPeriod*0.499))(logicalTime s),o)) newOps4
--  let newOps5' = fmap (\o -> (addUTCTime (0.2 + (renderPeriod*0.667)) (logicalTime s),o)) newOps5
--  let newOps6' = fmap (\o -> (addUTCTime (0.2 + (renderPeriod*0.833)) (logicalTime s),o)) newOps6
--  let newOps = newOps1' ++ newOps2' ++ newOps3' ++ newOps4' ++ newOps5' ++ newOps6'
  modify' $ \x -> x { superContinentState = scState1, canvasOps = canvasOps s ++ newOps1' }

renderPunctualVideo :: Context -> Int -> Renderer
renderPunctualVideo c z = when (canvasOn c) $ do
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
    rects = zip ts $ zipWith4 (\x' y' w' h' -> CanvasOp.Rect (x' - (w' * 0.5)) (y' - (h'*0.5)) w' h') x y w h

prependLogicalTime :: UTCTime -> a -> (UTCTime,a)
prependLogicalTime lt a = (lt,a)

sampleWithDefault :: String -> Double -> Punctual.PunctualState -> [UTCTime] -> [Double]
sampleWithDefault targetName d s ts = maybe (replicate (length ts) d) f $ Punctual.findGraphForTarget targetName s
  where f g = fmap (\t -> Punctual.sampleGraph (Punctual.startTime s) t 0 g) ts

biPolarToPercent :: Double -> Double
biPolarToPercent x = (x + 1) * 50

renderControlPattern :: Context -> Int -> Renderer
renderControlPattern c z = when (webDirtOn c || superDirtOn c) $ do
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
  when (diff'' > (renderPeriod / 4 )) $ do -- if next render cycle is more than a quarter of a render period away then sleep
   let delay = floor $ realToFrac diff'' * 1000000 - 2000 -- ie. wakeup ~ 2 milliseconds before next logical time
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
  irs <- initialRenderState renderStart
  void $ forkIO $ iterateM_ (execStateT $ runRender c ri) irs
