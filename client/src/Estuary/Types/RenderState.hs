module Estuary.Types.RenderState where

import Data.Time
import Data.Tempo
import Data.IntMap.Strict
import Sound.MusicW.AudioContext
import Sound.MusicW.Node as MusicW
import qualified Sound.Tidal.Context as Tidal
import qualified Sound.Punctual.PunctualW as Punctual
import qualified Sound.Punctual.WebGL as Punctual
import Sound.Punctual.GL

import Estuary.Types.Definition
import Estuary.Types.RenderEnvironment
import Estuary.Types.NoteEvent
import Estuary.Types.MovingAverage
import Estuary.Types.TextNotation
import qualified Estuary.Languages.CineCer0.CineCer0State as CineCer0
import qualified Estuary.Languages.CineCer0.Spec as CineCer0
import qualified Estuary.Languages.CineCer0.Parser as CineCer0
import qualified Sound.TimeNot.AST as TimeNot
import qualified Sound.Seis8s.Program as Seis8s
import qualified Estuary.Languages.Hydra.Render as Hydra

data RenderState = RenderState {

  -- the prevailing tempo and live coding programs to render
  tempo :: Tempo,
  defs :: !DefinitionMap,

  -- fundamental time parameters of the current render
  wakeTimeAudio :: !Double,
  wakeTimeSystem :: !UTCTime,
  wakeTimeAnimation :: !UTCTime,
  renderStart :: !UTCTime,
  renderEnd :: !UTCTime,
  renderPeriod :: !NominalDiffTime,

  -- statistics re: render time demands
  renderTime :: !MovingAverage,
  animationDelta :: !MovingAverage, -- time between frame starts, ie. 1/FPS
  animationTime :: !MovingAverage, -- time between frame start and end of drawing operations
  zoneRenderTimes :: !(IntMap MovingAverage),
  zoneAnimationTimes :: !(IntMap MovingAverage),

  -- state and intermediate results for various languages
  paramPatterns :: !(IntMap Tidal.ControlPattern),
  noteEvents :: ![NoteEvent],
  tidalEvents :: ![(UTCTime,Tidal.ControlMap)],
  baseNotations :: !(IntMap TextNotation),
  punctuals :: !(IntMap Punctual.PunctualW),
  punctualWebGL :: Punctual.PunctualWebGL,
  cineCer0Specs :: !(IntMap CineCer0.Spec),
  cineCer0States :: !(IntMap CineCer0.CineCer0State),
  timeNots :: IntMap TimeNot.Program,
  seis8ses :: IntMap Seis8s.Program,
  hydras :: IntMap Hydra.Hydra
  }

initialRenderState :: RenderEnvironment -> IO RenderState
initialRenderState rEnv t0System t0Audio = do
  wakeTimeAudio' <- liftAudioIO $ audioTime
  wakeTimeSystem' <- getCurrentTime
  glCtx <- newGLContext $ punctualCanvas rEnv
  pWebGL <- Punctual.newPunctualWebGL (Just $ mic rEnv) (Just $ out rEnv) Punctual.HD 1.0 glCtx
  return $ RenderState {
    tempo = Tempo { freq = 0.5, time = wakeTimeSystem', count = 0 },
    defs = empty,
    wakeTimeAudio = wakeTimeAudio',
    wakeTimeSystem = wakeTimeSystem',
    wakeTimeAnimation = wakeTimeSystem',
    renderStart = wakeTimeSystem',
    renderEnd = wakeTimeSystem',
    renderPeriod = 0,
    renderTime = newAverage 20,
    animationDelta = newAverage 20,
    animationTime = newAverage 20,
    zoneRenderTimes = empty,
    zoneAnimationTimes = empty,
    paramPatterns = empty,
    noteEvents = [],
    tidalEvents = [],
    baseNotations = empty,
    punctuals = empty,
    punctualWebGL = pWebGL,
    cineCer0Specs = empty,
    cineCer0States = empty,
    timeNots = empty,
    seis8ses = empty,
    hydras = empty
    }
