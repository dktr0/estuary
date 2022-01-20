{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Estuary.Render.Renderer where

import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Sound.Tidal.Context as Tidal
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (evaluate,catch,SomeException,try)
import Control.DeepSeq
import Control.Monad.Loops
import Data.Functor (void)
import Data.List (intercalate,zipWith4)
import Data.IntMap.Strict as IntMap
import Data.Maybe
import Data.Either
import qualified Data.Map.Strict as Map
import JavaScript.Web.AnimationFrame
import GHCJS.Concurrent
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Bifunctor
import TextShow
import Sound.OSC.Datum
import Text.Parsec (ParseError)
import qualified Data.ByteString as B
import GHCJS.DOM.Types (HTMLCanvasElement)
import Data.Witherable
import Data.Char
import Data.IORef

import Sound.MusicW.AudioContext
import qualified Sound.Punctual.Program as Punctual
import qualified Sound.Punctual.PunctualW as Punctual
import qualified Sound.Punctual.GL as Punctual
import qualified Sound.Punctual.WebGL as Punctual
import qualified Sound.Punctual.AsyncProgram as Punctual
import qualified Sound.Punctual.Parser as Punctual
import qualified Sound.Punctual.Resolution as Punctual
import qualified Sound.TimeNot.AST as TimeNot
import qualified Sound.TimeNot.Parsers as TimeNot
import qualified Sound.TimeNot.Render as TimeNot
import qualified Sound.Seis8s.Parser as Seis8s

import qualified Estuary.Languages.Hydra.Types as Hydra
import qualified Estuary.Languages.Hydra.Parser as Hydra
import qualified Estuary.Languages.Hydra.Render as Hydra
import qualified Estuary.Languages.CineCer0.CineCer0State as CineCer0
import qualified Estuary.Languages.CineCer0.Spec as CineCer0
import qualified Estuary.Languages.CineCer0.Parser as CineCer0
import Estuary.Types.Ensemble
import Estuary.Types.EnsembleC
import Estuary.Types.Context
import Estuary.Types.Definition
import Estuary.Types.TextNotation
import Estuary.Types.TidalParser
import Estuary.Tidal.Types
import Estuary.Types.ParamPatternable
import Estuary.Types.Live
import Estuary.Languages.TidalParsers
import qualified Estuary.Languages.JSoLang as JSoLang
import qualified Estuary.Render.WebDirt as WebDirt
import qualified Estuary.Render.SuperDirt as SuperDirt
import Estuary.Types.NoteEvent
import Estuary.Types.RenderInfo
import Estuary.Types.RenderState
import Estuary.Types.Tempo
import Estuary.Types.MovingAverage
import Estuary.Render.DynamicsMode
import Estuary.Render.R
import Estuary.Types.UriOptions as Uri


clockRatioThreshold :: Double
clockRatioThreshold = 0.8

maxRenderLatency :: NominalDiffTime
maxRenderLatency = 0.360

maxRenderPeriod :: NominalDiffTime
maxRenderPeriod = 0.360

minRenderLatency :: NominalDiffTime
minRenderLatency = 0.070

minRenderPeriod :: NominalDiffTime
minRenderPeriod = 0.120

-- should be somewhat larger than maxRenderLatency
waitThreshold :: NominalDiffTime
waitThreshold = 0.380

rewindThreshold :: NominalDiffTime
rewindThreshold = 1.0

earlyWakeUp :: NominalDiffTime
earlyWakeUp = 0.002


-- flush events for SuperDirt and WebDirt
flushEvents :: Context -> R ()
flushEvents c = do
  irc <- ask
  s <- get
  let unsafe = unsafeMode c
  when (webDirtOn c) $ liftIO $ do
    let cDiff = (wakeTimeSystem s,wakeTimeAudio s)
    noteEvents' <- witherM (WebDirt.noteEventToWebDirtJSVal unsafe (resources irc) cDiff) $ noteEvents s
    tidalEvents' <- witherM (WebDirt.tidalEventToWebDirtJSVal unsafe (resources irc) cDiff) $ tidalEvents s
    mapM_ (WebDirt.playSample (webDirt irc)) $ noteEvents' ++ tidalEvents'
  when (superDirtOn c) $ liftIO $ do
    noteEvents' <- mapM SuperDirt.noteEventToSuperDirtJSVal $ noteEvents s
    tidalEvents' <- mapM SuperDirt.tidalEventToSuperDirtJSVal $ tidalEvents s
    mapM_ (SuperDirt.playSample (superDirt irc)) $ noteEvents' ++ tidalEvents'
  modify' $ \x -> x { noteEvents = [], tidalEvents = [] }
  return ()

renderTidalPattern :: Tidal.ValueMap -> UTCTime -> NominalDiffTime -> Tempo -> Tidal.ControlPattern -> [(UTCTime,Tidal.ValueMap)]
renderTidalPattern vMap start range t p = events''
  where
    start' = (realToFrac $ diffUTCTime start (time t)) * freq t + count t -- start time in cycles since beginning of tempo
    end = realToFrac range * freq t + start' -- end time in cycles since beginning of tempo
    a = Tidal.Arc (toRational start') (toRational end)
    events = Tidal.query p $ Tidal.State a vMap
    events' = Prelude.filter Tidal.eventHasOnset events
    events'' = f <$> events'
    f e = (utcTime,Tidal.value e)
      where
        utcTime = addUTCTime (realToFrac ((fromRational w1 - count t)/freq t)) (time t)
        w1 = Tidal.start $ fromJust $ Tidal.whole e

sequenceToControlPattern :: (Text,[Bool]) -> Tidal.ControlPattern
sequenceToControlPattern (sampleName,pat) = Tidal.s $ parseBP' $ intercalate " " $ fmap f pat
  where f False = "~"
        f True = T.unpack sampleName

render :: Context -> R ()
render c = do
  irc <- ask
  s <- get
  -- check if audio clock has advanced same amount as system clock
  t1System <- liftIO $ getCurrentTime
  t1Audio <- liftAudioIO $ audioTime
  let elapsedSystem = (realToFrac $ diffUTCTime t1System $ wakeTimeSystem s) :: Double
  let elapsedAudio = t1Audio - wakeTimeAudio s
  let cr = elapsedAudio / elapsedSystem
  let crProblem = cr < clockRatioThreshold && cr > 0
  modify' $ \x -> x {
    wakeTimeSystem = t1System,
    wakeTimeAudio = t1Audio,
    info = (info x) { clockRatio = cr, clockRatioProblem = crProblem }
  }
  when crProblem $ liftIO $ T.putStrLn $ "audio clock slower, ratio=" <> showt cr

  -- four possible timing scenarios to account for...
  let diff = diffUTCTime (renderEnd s) t1System
  -- 1. Fast Forward
  when (diff < minRenderLatency) $ do
    liftIO $ T.putStrLn "FAST-FORWARD"
    modify' $ \x -> x {
      renderStart = addUTCTime minRenderLatency t1System,
      renderPeriod = minRenderPeriod,
      renderEnd = addUTCTime (minRenderPeriod+minRenderLatency) t1System
    }

  -- 2. Normal Advance
  when (diff >= minRenderLatency && diff <= waitThreshold) $ do
    let ratio0 = (diff - minRenderLatency) / (maxRenderLatency - minRenderLatency)
    let ratio = min 1 ratio0
    let adaptivePeriod = ratio * (maxRenderPeriod - minRenderPeriod) + minRenderPeriod
    -- liftIO $ T.putStrLn $ "NORMAL " <> showt (realToFrac ratio :: Double) <> " " <> showt (realToFrac adaptivePeriod :: Double)
    modify' $ \x -> x {
      renderStart = renderEnd s,
      renderPeriod = adaptivePeriod,
      renderEnd = addUTCTime adaptivePeriod (renderEnd s)
    }

  -- 3. Wait
  let wait = (diff > waitThreshold && diff < rewindThreshold)
  when wait $ liftIO $ T.putStrLn $ "WAIT " <> showt (realToFrac diff :: Double)

  -- 4. Rewind
  let rewind = (diff >= rewindThreshold)
  when rewind $ do
    liftIO $ T.putStrLn $ "REWIND"
    modify' $ \x -> x {
      renderStart = addUTCTime minRenderLatency t1System,
      renderPeriod = minRenderPeriod,
      renderEnd = addUTCTime (minRenderPeriod+minRenderLatency) t1System
    }

  -- if there is no reason not to traverse/render zones, then do so
  -- using renderStart and renderEnd from the state as the window to render
  when (not wait && not rewind) $ do
    updateTidalValueMap
    let newDefs = zones $ ensemble $ ensembleC c
    clearDeletedZones newDefs
    traverseWithKey (renderZone c) newDefs
    flushEvents c
    updatePunctualResolutionAndBrightness c
    -- calculate how much time this render cycle took and update load measurements
    t2System <- liftIO $ getCurrentTime
    t2Audio <- liftAudioIO $ audioTime
    let mostRecentRenderTime = diffUTCTime t2System t1System
    let newRenderTime = updateAverage (renderTime s) $ realToFrac mostRecentRenderTime
    let newAvgRenderLoad = ceiling (getAverage newRenderTime * 100 / realToFrac maxRenderPeriod)
    modify' $ \x -> x { renderTime = newRenderTime }
    modify' $ \x -> x { info = (info x) { avgRenderLoad = newAvgRenderLoad }}
    traverseWithKey calculateZoneRenderTimes $ zoneRenderTimes s -- *** SHOULDN'T BE HERE
    traverseWithKey calculateZoneAnimationTimes $ zoneAnimationTimes s -- *** SHOULDN'T BE HERE
    return ()


renderZone :: Context -> Int -> Definition -> R ()
renderZone c z d = do
  t1 <- liftIO $ getCurrentTime
  s <- get
  let prevDef = IntMap.lookup z $ cachedDefs s
  let d' = definitionForRendering d
  when (prevDef /= (Just d')) $ do
    maybeClearChangedZone z prevDef d'
    renderZoneChanged c z d'
    modify' $ \x -> x { cachedDefs = insert z d' (cachedDefs s) }
  renderZoneAlways c z d'
  t2 <- liftIO $ getCurrentTime
  let prevZoneRenderTimes = findWithDefault (newAverage 20) z $ zoneRenderTimes s
  let newZoneRenderTimes = updateAverage prevZoneRenderTimes (realToFrac $ diffUTCTime t2 t1)
  modify' $ \x -> x { zoneRenderTimes = insert z newZoneRenderTimes (zoneRenderTimes s) }


clearDeletedZones :: IntMap.IntMap Definition -> R ()
clearDeletedZones newDefs = do
  prevDefs <- gets cachedDefs
  IntMap.traverseWithKey clearZone $ IntMap.difference prevDefs newDefs
  return ()

maybeClearChangedZone :: Int -> Maybe Definition -> Definition -> R ()
maybeClearChangedZone _ Nothing y = return ()
maybeClearChangedZone z (Just x) y
  | defsSameRender x y = return ()
  | otherwise = clearZone z x

defsSameRender :: Definition -> Definition -> Bool
defsSameRender (TextProgram x) (TextProgram y) = textProgramsSameRender (forRendering x) (forRendering y)
defsSameRender (Sequence _) (Sequence _) = True
defsSameRender (TidalStructure _) (TidalStructure _) = True
defsSameRender _ _ = False

textProgramsSameRender :: (TextNotation,Text,UTCTime) -> (TextNotation,Text,UTCTime) -> Bool
textProgramsSameRender (x,_,_) (y,_,_) = x==y

clearZone :: Int -> Definition -> R ()
clearZone z (TidalStructure _) = clearParamPattern z
clearZone z (TextProgram x) = clearTextProgram z $ forRendering x
clearZone z (Sequence _) = clearParamPattern z
clearZone _ _ = return ()

clearTextProgram :: Int -> (TextNotation,Text,UTCTime) -> R ()
clearTextProgram z (Punctual,_,_) = do
  s <- get
  case (IntMap.lookup z $ punctuals s) of
    Just x -> liftAudioIO $ Punctual.deletePunctualW x
    Nothing -> return ()
  newPunctualWebGL <- liftIO $ Punctual.deletePunctualWebGL (glContext s) z $ punctualWebGL s
  modify' $ \x -> x {
    punctuals = IntMap.delete z $ punctuals x,
    punctualWebGL = newPunctualWebGL
  }
clearTextProgram z (CineCer0,_,_) = do
  s <- get
  case (IntMap.lookup z $ cineCer0States s) of
    Just x -> liftIO $ CineCer0.deleteCineCer0State x
    Nothing -> return ()
  modify' $ \x -> x {
    cineCer0Specs = IntMap.delete z $ cineCer0Specs x,
    cineCer0States = IntMap.delete z $ cineCer0States x
    }
clearTextProgram z (Hydra,_,_) = modify' $ \x -> x { hydras = IntMap.delete z $ hydras x }
clearTextProgram _ _ = return ()

clearParamPattern :: Int -> R ()
clearParamPattern z = modify' $ \s -> s { paramPatterns = IntMap.delete z (paramPatterns s) }


renderAnimation :: R ()
renderAnimation = do
  t1 <- liftIO $ getCurrentTime
  wta <- gets wakeTimeAnimation
  fpsl <- gets animationFpsLimit
  let okToRender = case fpsl of Nothing -> True; Just x -> diffUTCTime t1 wta > x
  when okToRender $ do
    ns <- baseNotations <$> get
    traverseWithKey (renderZoneAnimation t1) ns
    s  <- get
    newWebGL <- liftIO $
       Punctual.displayPunctualWebGL (glContext s) (punctualWebGL s)
       `catch` (\e -> putStrLn (show (e :: SomeException)) >> return (punctualWebGL s))
    t2 <- liftIO $ getCurrentTime
    let newAnimationDelta = updateAverage (animationDelta s) (realToFrac $ diffUTCTime t1 wta)
    let newAnimationTime = updateAverage (animationTime s) (realToFrac $ diffUTCTime t2 t1)
    let newAnimationFPS = round $ 1 / getAverage newAnimationDelta
    let newAnimationLoad = round $ getAverage newAnimationTime * 1000
    modify' $ \x -> x {
      punctualWebGL = newWebGL,
      wakeTimeAnimation = t1,
      animationDelta = newAnimationDelta,
      animationTime = newAnimationTime,
      info = (info x) {
        animationFPS = newAnimationFPS,
        animationLoad = newAnimationLoad
        }
      }

renderZoneAnimation :: UTCTime -> Int -> TextNotation -> R ()
renderZoneAnimation tNow z n = do
  t1 <- liftIO $ getCurrentTime
  renderZoneAnimationTextProgram tNow z n
  t2 <- liftIO $ getCurrentTime
  prevTimes <- zoneAnimationTimes <$> get
  let prevZoneAnimationTimes = findWithDefault (newAverage 20) z prevTimes
  let newZoneAnimationTimes = updateAverage prevZoneAnimationTimes (realToFrac $ diffUTCTime t2 t1)
  modify' $ \x -> x { zoneAnimationTimes = insert z newZoneAnimationTimes prevTimes }
  return ()

renderZoneAnimationTextProgram :: UTCTime -> Int -> TextNotation -> R ()
renderZoneAnimationTextProgram tNow z Punctual = renderPunctualWebGL tNow z
renderZoneAnimationTextProgram tNow z CineCer0 = renderCineCer0 tNow z
renderZoneAnimationTextProgram tNow z Hydra = renderHydra tNow z
renderZoneAnimationTextProgram  _ _ _ = return ()

updatePunctualResolutionAndBrightness :: Context -> R ()
updatePunctualResolutionAndBrightness ctx = do
  s <- get
  newWebGL <- liftIO $
    do
      x <- Punctual.setResolution (glContext s) (resolution ctx) (punctualWebGL s)
      Punctual.setBrightness (brightness ctx) x
    `catch` (\e -> putStrLn (show (e :: SomeException)) >> return (punctualWebGL s))
  modify' $ \x -> x { punctualWebGL = newWebGL }

renderPunctualWebGL :: UTCTime -> Int -> R ()
renderPunctualWebGL tNow z = do
  s <- get
  newWebGL <- liftIO $
    Punctual.drawPunctualWebGL (glContext s) (tempoCache s) tNow z (punctualWebGL s)
    `catch` (\e -> putStrLn (show (e :: SomeException)) >> return (punctualWebGL s))
  modify' $ \x -> x { punctualWebGL = newWebGL }

renderCineCer0 :: UTCTime -> Int -> R ()
renderCineCer0 tNow z = do
  s <- get
  case videoDivCache s of
    Nothing -> return ()
    Just theDiv -> do
      let spec = IntMap.findWithDefault (CineCer0.emptySpec $ renderStart s) z (cineCer0Specs s)
      let prevState = IntMap.findWithDefault (CineCer0.emptyCineCer0State theDiv) z $ cineCer0States s
      newState <- liftIO $ CineCer0.updateCineCer0State (tempoCache s) tNow spec prevState
      modify' $ \x -> x { cineCer0States = insert z newState (cineCer0States s) }

renderHydra :: UTCTime -> Int -> R ()
renderHydra tNow z = do
  s <- get
  let wta = wakeTimeAnimation s
  let elapsed = realToFrac $ diffUTCTime tNow wta * 1000
  let x = IntMap.lookup z $ hydras s
  case x of
    Just hydra -> liftIO $ Hydra.tick hydra elapsed
    Nothing -> return ()

renderZoneChanged :: Context -> Int -> Definition -> R ()
renderZoneChanged c z (TidalStructure x) = do
  let newParamPattern = toParamPattern x
  s <- get
  modify' $ \x -> x { paramPatterns = insert z newParamPattern (paramPatterns s) }
renderZoneChanged c z (TextProgram x) = do
  renderTextProgramChanged c z $ forRendering x
renderZoneChanged c z (Sequence xs) = do
  let newParamPattern = Tidal.stack $ Map.elems $ Map.map sequenceToControlPattern xs
  s <- get
  modify' $ \x -> x { paramPatterns = insert z newParamPattern (paramPatterns s) }
renderZoneChanged _ _ _ = return ()

renderZoneAlways :: Context -> Int -> Definition -> R ()
renderZoneAlways c z (TidalStructure _) = renderControlPattern c z
renderZoneAlways c z (TextProgram x) = do
  let (_,_,evalTime) = forRendering x
  renderTextProgramAlways c z evalTime
renderZoneAlways c z (Sequence _) = renderControlPattern c z
renderZoneAlways _ _ _ = return ()


renderTextProgramChanged :: Context -> Int -> TextProgram -> R ()

renderTextProgramChanged c z (UnspecifiedNotation,x,eTime) = do
  ns <- (Map.keys . jsoLangs) <$> get
  liftIO $ T.putStrLn $ T.pack $ show ns
  case determineTextNotation x ns of
    Left err -> do
      setZoneError z (T.pack $ show err)
      setBaseNotation z UnspecifiedNotation
    Right (x',n) -> do
      case n of
        UnspecifiedNotation -> do
          case T.filter (\c -> not (isControl c) && not (isSpace c)) x' of
            "" -> do -- notation is unspecified but
              clearZoneError z
              setBaseNotation z UnspecifiedNotation
            _ -> do
              setZoneError z "no base notation specified"
              setBaseNotation z UnspecifiedNotation
        _-> renderTextProgramChanged c z (n,x',eTime)

renderTextProgramChanged c z (TidalTextNotation x,y,eTime) = do
  s <- get
  parseResult <- liftIO $ (return $! force (tidalParser x y)) `catch` (return . Left . (show :: SomeException -> String))
  case parseResult of
    Right p -> do
      clearZoneError z
      setBaseNotation z (TidalTextNotation x)
      setEvaluationTime z eTime
      modify' $ \xx -> xx { paramPatterns = insert z p $ paramPatterns xx }
    Left err -> setZoneError z $ T.pack err

renderTextProgramChanged c z (Punctual,x,eTime) = parsePunctualNotation c z x eTime

renderTextProgramChanged c z (Hydra,x,_) = parseHydra c z x

renderTextProgramChanged c z (CineCer0,x,eTime) = do
  let parseResult :: Either String CineCer0.Spec = CineCer0.cineCer0 eTime $ T.unpack x -- Either String CineCer0Spec
  case parseResult of
    Right spec -> do
      clearZoneError z
      setBaseNotation z CineCer0
      setEvaluationTime z eTime
      modify' $ \xx -> xx { cineCer0Specs = insert z spec $ cineCer0Specs xx }
    Left err -> setZoneError z (T.pack err)

renderTextProgramChanged c z (TimeNot,x,eTime) = do
  let parseResult = TimeNot.runCanonParser $ T.unpack x
  case parseResult of
    Right p -> do
      clearZoneError z
      setBaseNotation z TimeNot
      setEvaluationTime z eTime
      modify' $ \xx -> xx { timeNots = insert z p (timeNots xx) }
    Left e -> setZoneError z (T.pack $ show e)

renderTextProgramChanged c z (Seis8s,x,eTime) = do
  let parseResult = Seis8s.parseLang $ T.unpack x
  case parseResult of
    Right p -> do
      clearZoneError z
      setBaseNotation z Seis8s
      setEvaluationTime z eTime
      modify' $ \xx -> xx { seis8ses = insert z p $ seis8ses xx }
    Left e -> setZoneError z (T.pack $ show e)

renderTextProgramChanged c z (JSoLang x,y,eTime) = do
  parseResult <- liftIO $ JSoLang.define y
  case parseResult of
    Right j -> do
      clearZoneError z
      setBaseNotation z (JSoLang x)
      setEvaluationTime z eTime
      modify' $ \xx -> xx { jsoLangs = Map.insert x j $ jsoLangs xx }
      liftIO $ T.putStrLn $ "defined JSoLang " <> x
    Left e -> setZoneError z (T.pack $ show e)

renderTextProgramChanged c z (EphemeralNotation x,y,eTime) = do
  maybeJSoLang <- (Map.lookup x . jsoLangs) <$> get
  case maybeJSoLang of
    Just j -> do
      parseResult <- liftIO $ JSoLang.parse j y
      case parseResult of
        Right x' -> do
          liftIO $ T.putStrLn $ "result of parsing " <> x <> ":"
          liftIO $ T.putStrLn x'
          renderTextProgramChanged c z (UnspecifiedNotation,x',eTime)
        Left e -> setZoneError z e
    Nothing -> setZoneError z $ "no ephemeral notation called " <> y <> " exists"

renderTextProgramChanged c z _ = setZoneError z "renderTextProgramChanged: no match for base notation"

parsePunctualNotation :: Context -> Int -> Text -> UTCTime -> R ()
parsePunctualNotation c z t eTime = do
  s <- get
  parseResult <- liftIO $ try $ return $! Punctual.parse eTime t
  parseResult' <- case parseResult of
    Right (Right punctualProgram) -> do
      setBaseNotation z Punctual
      setEvaluationTime z eTime
      punctualProgramChanged c z punctualProgram
      return (Right punctualProgram)
    Right (Left parseErr) -> return (Left $ T.pack $ show parseErr)
    Left exception -> return (Left $ T.pack $ show (exception :: SomeException))
  let newErrors = either (\e -> insert z e (errors (info s))) (const $ delete z (errors (info s))) parseResult'
  modify' $ \x -> x { info = (info s) { errors = newErrors }}

parseHydra :: Context -> Int -> Text -> R ()
parseHydra c z t = do
 s <- get
 parseResult <- liftIO $ try $ return $! Hydra.parseHydra t
 case parseResult of
   Right (Right stmts) -> do
     clearZoneError z
     setBaseNotation z Hydra
     -- setEvaluationTime z eTime ???
     let x = IntMap.lookup z $ hydras s
     hydra <- case x of
       Just h -> return h
       Nothing -> do
         h <- liftIO $ Hydra.newHydra $ hydraCanvas s
         modify' $ \x -> x { hydras = IntMap.insert z h (hydras x)}
         return h
     -- liftIO $ Hydra.setResolution hydra 1280 720
     liftIO $ Hydra.evaluate hydra stmts
   Right (Left parseErr) -> setZoneError z (T.pack $ show parseErr)
   Left exception -> setZoneError z (T.pack $ show (exception :: SomeException))

punctualProgramChanged :: Context -> Int -> Punctual.Program -> R ()
punctualProgramChanged c z p = do
  irc <- ask
  s <- get
  -- A. update PunctualW (audio state) in response to new, syntactically correct program
  pIn <- liftIO $ getPunctualInput $ mainBus irc
  pOut <- liftIO $ getMainBusInput $ mainBus irc
  ac <- liftAudioIO $ audioContext
  t <- liftAudioIO $ audioTime
  nchnls <- liftIO $ getAudioOutputs $ mainBus irc
  let prevPunctualW = Punctual.setPunctualWChannels nchnls $ findWithDefault (Punctual.emptyPunctualW ac pIn pOut nchnls (Punctual.evalTime p)) z (punctuals s)
  let tempo' = tempo $ ensemble $ ensembleC c
  let beat0 = utcTimeToAudioSeconds (wakeTimeSystem s, wakeTimeAudio s) $ origin tempo'
  let cps' = freq tempo'
  newPunctualW <- liftIO $ do
    runAudioContextIO ac $ Punctual.updatePunctualW prevPunctualW (tempoCache s) p
    `catch` (\e -> putStrLn (show (e :: SomeException)) >> return prevPunctualW)
  modify' $ \x -> x { punctuals = insert z newPunctualW (punctuals s)}
  -- B. update Punctual WebGL state in response to new, syntactically correct program
  pWebGL <- gets punctualWebGL
  newWebGL <- liftIO $
    Punctual.evaluatePunctualWebGL (glContext s) (tempoCache s) z p pWebGL
    `catch` (\e -> putStrLn (show (e :: SomeException)) >> return pWebGL)
  modify' $ \x -> x { punctualWebGL = newWebGL }

renderTextProgramAlways :: Context -> Int -> UTCTime -> R ()
renderTextProgramAlways c z eTime = do
  s <- get
  let baseNotation = IntMap.lookup z $ baseNotations s
  renderBaseProgramAlways c z eTime $ baseNotation

renderBaseProgramAlways :: Context -> Int -> UTCTime -> Maybe TextNotation -> R ()
renderBaseProgramAlways c z _ (Just (TidalTextNotation _)) = renderControlPattern c z
renderBaseProgramAlways c z _ (Just TimeNot) = do
  s <- get
  let p = IntMap.lookup z $ timeNots s
  case p of
    (Just p') -> do
      let theTempo = (tempo . ensemble . ensembleC) c
      let eTime = IntMap.findWithDefault (wakeTimeSystem s) z $ evaluationTimes s
      let wStart = renderStart s
      let wEnd = renderEnd s
      let oTime = firstCycleStartAfter theTempo eTime
      pushNoteEvents $ fmap TimeNot.mapForEstuary $ TimeNot.render oTime p' wStart wEnd
    Nothing -> return ()
renderBaseProgramAlways c z _ (Just Seis8s) = do
  s <- get
  let p = IntMap.lookup z $ seis8ses s
  case p of
    Just p' -> do
      let theTempo = (tempo . ensemble . ensembleC) c
      let wStart = renderStart s
      let wEnd = renderEnd s
      pushNoteEvents $ Seis8s.render p' theTempo wStart wEnd
    Nothing -> return ()
renderBaseProgramAlways _ _ _ _ = return ()


renderControlPattern :: Context -> Int -> R ()
renderControlPattern c z = when (webDirtOn c || superDirtOn c) $ do
  s <- get
  let controlPattern = IntMap.lookup z $ paramPatterns s -- :: Maybe ControlPattern
  let vMap = valueMap s
  case controlPattern of
    Just controlPattern' -> do
      let lt = renderStart s
      let rp = renderPeriod s
      let tempo' = tempo $ ensemble $ ensembleC c
      newEvents <- liftIO $ (return $! force $ renderTidalPattern vMap lt rp tempo' controlPattern')
        `catch` (\e -> putStrLn (show (e :: SomeException)) >> return [])
      pushTidalEvents newEvents
    Nothing -> return ()

updateTidalValueMap :: R ()
updateTidalValueMap = do
  rEnv <- ask
  m <- liftIO $ readIORef $ ccMap rEnv
  modify' $ \x -> x { valueMap = fmap Tidal.toValue $ Map.mapKeys T.unpack m}

calculateZoneRenderTimes :: Int -> MovingAverage -> R ()
calculateZoneRenderTimes z zrt = do
  s <- get
  let newAvgMap = insert z (getAverage zrt) (avgZoneRenderTime $ info s)
  modify' $ \x -> x { info = (info x) { avgZoneRenderTime = newAvgMap }}

calculateZoneAnimationTimes :: Int -> MovingAverage -> R ()
calculateZoneAnimationTimes z zat = do
  s <- get
  let newAvgMap = insert z (getAverage zat) (avgZoneAnimationTime $ info s)
  modify' $ \x -> x { info = (info x) { avgZoneAnimationTime = newAvgMap }}

sleepIfNecessary :: R ()
sleepIfNecessary = do
  s <- get
  let targetTime = addUTCTime (maxRenderLatency * (-1) - earlyWakeUp) (renderEnd s)
  tNow <- liftIO $ getCurrentTime
  let diff = diffUTCTime targetTime tNow
  when (diff > 0) $ liftIO $ threadDelay $ floor $ realToFrac $ diff * 1000000

forkRenderThreads :: Uri.UriOptions -> MVar Context -> HTMLCanvasElement -> Punctual.GLContext -> HTMLCanvasElement -> MVar RenderInfo -> IO RenderEnvironment
forkRenderThreads uriOptions ctxM cvsElement glCtx hCanvas riM = do
  rEnv <- initialRenderEnvironment uriOptions
  t0Audio <- liftAudioIO $ audioTime
  t0System <- getCurrentTime
  pIn <- getPunctualInput $ mainBus rEnv
  pOut <- getMainBusInput $ mainBus rEnv
  putStrLn "about to initialRenderState"
  irs <- initialRenderState pIn pOut cvsElement glCtx hCanvas t0System t0Audio
  putStrLn "returned from initialRenderState"
  rsM <- newMVar irs
  putStrLn "about to fork mainRenderThread..."
  void $ forkIO $ mainRenderThread rEnv ctxM riM rsM
  putStrLn "returned from forking mainRenderThread"
  void $ forkIO $ animationThread rEnv rsM
  putStrLn "returned from forking animationThread"
  return rEnv

mainRenderThread :: RenderEnvironment -> MVar Context -> MVar RenderInfo -> MVar RenderState -> IO ()
mainRenderThread rEnv ctxM riM rsM = do
  ctx <- readMVar ctxM
  rs <- takeMVar rsM
  rs' <- runR (render ctx) rEnv rs
  let rs'' = rs' {
    animationFpsLimit = fpsLimit ctx,
    tempoCache = tempo $ ensemble $ ensembleC ctx,
    videoDivCache = videoDivElement ctx
    }
  putMVar rsM rs''
  swapMVar riM (info rs'') -- copy RenderInfo from state into MVar for instant reading elsewhere
  _ <- runR sleepIfNecessary rEnv rs''
  mainRenderThread rEnv ctxM riM rsM

animationThread :: RenderEnvironment -> MVar RenderState -> IO ()
animationThread rEnv rsM = void $ inAnimationFrame ContinueAsync $ \_ -> do
  rs <- readMVar rsM
  animOn <- readIORef $ animationOn rEnv
  when animOn $ do
    rs' <- takeMVar rsM
    rs'' <- runR renderAnimation rEnv rs'
    putMVar rsM rs''
  animationThread rEnv rsM
