{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.RenderState where

import Data.Time.Clock
import qualified Data.Map as Map
import Data.IntMap.Strict
import qualified Sound.Tidal.Context as Tidal
import qualified Sound.Punctual.PunctualW as Punctual
import qualified Sound.Punctual.WebGL as Punctual
import qualified Sound.Punctual.Resolution as Punctual
import Sound.MusicW.AudioContext
import Sound.MusicW.Node as MusicW
import GHCJS.DOM.Types hiding (Text)
import Data.Text (Text)
import Sound.Punctual.GL
import Data.Tempo
import Data.IORef

import Estuary.Types.Definition
import Estuary.Types.RenderInfo
import Estuary.Types.NoteEvent
import Estuary.Types.MovingAverage
import Estuary.Types.TextNotation hiding (LocoMotion)
import qualified Estuary.Languages.CineCer0.CineCer0State as CineCer0
import qualified Estuary.Languages.CineCer0.Spec as CineCer0
import qualified Estuary.Languages.CineCer0.Parser as CineCer0
import qualified Sound.Seis8s.Program as Seis8s
import qualified Estuary.Languages.Hydra.Render as Hydra
import Estuary.Languages.JSoLang
import Estuary.Languages.ExoLang
import Estuary.Render.Renderer
import qualified Estuary.Languages.MiniTidal as MiniTidal
import qualified Estuary.Languages.Punctual as Punctual

newtype LocoMotion = LocoMotion JSVal

instance PToJSVal LocoMotion where pToJSVal (LocoMotion x) = x

instance PFromJSVal LocoMotion where pFromJSVal = LocoMotion

data RenderState = RenderState {
  wakeTimeAudio :: !Double,
  wakeTimeSystem :: !UTCTime,
  renderStart :: !UTCTime,
  renderPeriod :: !NominalDiffTime,
  renderEnd :: !UTCTime,
  cachedDefs :: !DefinitionMap,
  paramPatterns :: !(IntMap Tidal.ControlPattern),
  noteEvents :: ![NoteEvent],
  webDirtEvents :: ![JSVal], -- deprecated/temporary
  baseNotations :: !(IntMap TextNotation),
  cineCer0Specs :: !(IntMap CineCer0.Spec),
  cineCer0States :: !(IntMap CineCer0.CineCer0State),
  timeNots :: IntMap JSVal,
  seis8ses :: IntMap Seis8s.Program,
  hydras :: IntMap Hydra.Hydra,
  evaluationTimes :: IntMap UTCTime, -- this is probably temporary
  renderTime :: !MovingAverage,
  wakeTimeAnimation :: !UTCTime,
  animationDelta :: !MovingAverage, -- time between frame starts, ie. 1/FPS
  animationTime :: !MovingAverage, -- time between frame start and end of drawing operations
  zoneRenderTimes :: !(IntMap MovingAverage),
  zoneAnimationTimes :: !(IntMap MovingAverage),
  info :: !RenderInfo,
  canvasElement :: HTMLCanvasElement,
  hydraCanvas :: HTMLCanvasElement,
  locoMotionCanvas :: HTMLCanvasElement,
  videoDivCache :: Maybe HTMLDivElement,
  tempoCache :: Tempo,
  jsoLangs :: Map.Map Text JSoLang,
  valueMap :: Tidal.ValueMap,
  locoMotion :: ExoLang,
  exoLangTest :: ExoLang,
  transMit :: ExoLang,
  miniTidal :: Renderer,
  punctual :: Renderer
  }


initialRenderState :: MusicW.Node -> MusicW.Node -> HTMLCanvasElement -> HTMLCanvasElement -> HTMLCanvasElement -> UTCTime -> AudioTime -> IO RenderState
initialRenderState pIn pOut cvsElement hCanvas lCanvas t0System t0Audio = do
  lm <- exoLang lCanvas "https://dktr0.github.io/LocoMotion/src/webpack-module.js"
  elt <- exoLang lCanvas "./exolang.js"
  tm <- exoLang lCanvas "https://jac307.github.io/TransMit/exolang.js"
  miniTidal' <- MiniTidal.miniTidal
  punctual' <- Punctual.punctual hCanvas
  setAudioInput punctual' pIn
  setAudioOutput punctual' pOut
  setNchnls punctual' $ numberOfOutputs pOut
  return $ RenderState {
    wakeTimeSystem = t0System,
    wakeTimeAudio = t0Audio,
    renderStart = t0System,
    renderPeriod = 0,
    renderEnd = t0System,
    cachedDefs = empty,
    paramPatterns = empty,
    noteEvents = [],
    webDirtEvents = [],
    baseNotations = empty,
    cineCer0Specs = empty,
    cineCer0States = empty,
    timeNots = empty,
    seis8ses = empty,
    hydras = empty,
    evaluationTimes = empty,
    renderTime = newAverage 20,
    wakeTimeAnimation = t0System,
    animationDelta = newAverage 20,
    animationTime = newAverage 20,
    zoneRenderTimes = empty,
    zoneAnimationTimes = empty,
    info = emptyRenderInfo,
    canvasElement = cvsElement,
    hydraCanvas = hCanvas,
    locoMotionCanvas = lCanvas,
    videoDivCache = Nothing,
    tempoCache = Tempo { freq = 0.5, time = t0System, count = 0 },
    jsoLangs = Map.empty,
    valueMap = Map.empty,
    locoMotion = lm,
    exoLangTest = elt,
    transMit = tm,
    miniTidal = miniTidal',
    punctual = punctual'
    }
