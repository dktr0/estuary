module Estuary.Languages.Punctual (punctual) where

import Data.Time
import Control.Monad.Except
import Data.IntMap as IntMap
import Data.Text as T
import Control.Exception
import Control.Monad.State.Strict
import Sound.MusicW.AudioContext
import Control.Monad.Reader

import qualified Sound.Punctual.Program as Punctual
import qualified Sound.Punctual.PunctualW as Punctual
import qualified Sound.Punctual.GL as Punctual
import qualified Sound.Punctual.WebGL as Punctual
import qualified Sound.Punctual.AsyncProgram as Punctual
import qualified Sound.Punctual.Parser as Punctual
import qualified Sound.Punctual.Resolution as Punctual

import Estuary.Render.R
import Estuary.Render.TextNotationRenderer
import Estuary.Types.RenderState
import Estuary.Types.RenderInfo
import Estuary.Types.Context
import Estuary.Types.TextNotation
import Estuary.Render.MainBus
import Estuary.Types.Tempo
import Estuary.Types.Ensemble
import Estuary.Types.EnsembleC

punctual :: TextNotationRenderer
punctual = TextNotationRenderer {
  parseZone = _parseZone,
  clearZone' = _clearZone,
  preAnimationFrame = _preAnimationFrame,
  zoneAnimationFrame = _zoneAnimationFrame,
  postAnimationFrame = _postAnimationFrame
  }


_parseZone :: Context -> Int -> Text -> UTCTime -> R ()
_parseZone c z t eTime = do
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


_clearZone :: Int -> R ()
_clearZone z = do
  s <- get
  case (IntMap.lookup z $ punctuals s) of
    Just x -> liftAudioIO $ Punctual.deletePunctualW x
    Nothing -> return ()
  newPunctualWebGL <- liftIO $ Punctual.deletePunctualWebGL (glContext s) z $ punctualWebGL s
  modify' $ \x -> x {
    punctuals = IntMap.delete z $ punctuals x,
    punctualWebGL = newPunctualWebGL
  }

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


_preAnimationFrame :: R ()
_preAnimationFrame = do
  s <- get
  res <- resolution
  b <- brightness
  newWebGL <- liftIO $ do
    x <- Punctual.setResolution (glContext s) res (punctualWebGL s)
    Punctual.setBrightness b x
    `catch` (\e -> putStrLn (show (e :: SomeException)) >> return (punctualWebGL s))
  modify' $ \x -> x { punctualWebGL = newWebGL }


_zoneAnimationFrame :: UTCTime -> Int -> R ()
_zoneAnimationFrame tNow z = do
  s <- get
  newWebGL <- liftIO $
    Punctual.drawPunctualWebGL (glContext s) (tempoCache s) tNow z (punctualWebGL s)
    `catch` (\e -> putStrLn (show (e :: SomeException)) >> return (punctualWebGL s))
  modify' $ \x -> x { punctualWebGL = newWebGL }


_postAnimationFrame :: R ()
_postAnimationFrame = do
  s  <- get
  newWebGL <- liftIO $
    Punctual.displayPunctualWebGL (glContext s) (punctualWebGL s)
    `catch` (\e -> putStrLn (show (e :: SomeException)) >> return (punctualWebGL s))
  modify' $ \x -> x { punctualWebGL = newWebGL }
