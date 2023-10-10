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
import Estuary.Languages.JSoLang
import Estuary.Render.Renderer
import Estuary.Languages.ExoLang
import qualified Estuary.Languages.MiniTidal as MiniTidal
import qualified Estuary.Languages.Punctual as Punctual
import qualified Estuary.Languages.Seis8s as Seis8s
import qualified Estuary.Languages.CineCer0 as CineCer0
import qualified Estuary.Languages.Hydra as Hydra

newtype LocoMotion = LocoMotion JSVal

instance PToJSVal LocoMotion where pToJSVal (LocoMotion x) = x

instance PFromJSVal LocoMotion where pFromJSVal = LocoMotion

data RenderState = RenderState {
  wakeTimeAudio :: !Double,
  wakeTimeSystem :: !UTCTime,
  renderStart :: !UTCTime,
  renderPeriod :: !NominalDiffTime,
  renderEnd :: !UTCTime,
  cachedDefs :: !DefinitionMap, -- the map of definitions as received from the external world via render ops
  baseDefinitions :: !DefinitionMap, -- the map of definitions as actually rendered (eg. with jsolang translations)
  paramPatterns :: !(IntMap Tidal.ControlPattern),
  noteEvents :: ![NoteEvent],
  timeNots :: IntMap JSVal,
  renderTime :: !MovingAverage,
  wakeTimeAnimation :: !UTCTime,
  animationDelta :: !MovingAverage, -- time between frame starts, ie. 1/FPS
  animationTime :: !MovingAverage, -- time between frame start and end of drawing operations
  zoneRenderTimes :: !(IntMap MovingAverage),
  zoneAnimationTimes :: !(IntMap MovingAverage),
  info :: !RenderInfo,
  tempoCache :: Tempo,
  jsoLangs :: Map.Map Text JSoLang,
  valueMap :: Tidal.ValueMap,
  exoLangTest :: Renderer,
  locoMotion :: Renderer,
  transMit :: Renderer,
  miniTidal :: Renderer,
  punctual :: Renderer,
  cineCer0 :: Renderer,
  hydra :: Renderer
  }


initialRenderState :: MusicW.Node -> MusicW.Node -> HTMLDivElement -> HTMLCanvasElement -> HTMLCanvasElement -> HTMLCanvasElement -> UTCTime -> AudioTime -> IO RenderState
initialRenderState pIn pOut cineCer0Div pCanvas lCanvas hCanvas t0System t0Audio = do
  let iTempo = Tempo { freq = 0.5, time = t0System, count = 0 }
  exoLangTest' <- exoLangRenderer "exolangtest" lCanvas "./exolang.js"
  locoMotion' <- exoLangRenderer "LocoMotion" lCanvas "https://dktr0.github.io/LocoMotion/src/webpack-module.js"
  transMit' <- exoLangRenderer "TransMit" lCanvas "https://jac307.github.io/TransMit/exolang.js"
  miniTidal' <- MiniTidal.miniTidal iTempo
  punctual' <- Punctual.punctual pCanvas iTempo
  setAudioInput punctual' pIn
  setAudioOutput punctual' pOut
  setNchnls punctual' $ numberOfOutputs pOut
  cineCer0' <- CineCer0.cineCer0 cineCer0Div iTempo
  hydra' <- Hydra.hydra hCanvas
  return $ RenderState {
    wakeTimeSystem = t0System,
    wakeTimeAudio = t0Audio,
    renderStart = t0System,
    renderPeriod = 0,
    renderEnd = t0System,
    cachedDefs = empty,
    baseDefinitions = empty,
    paramPatterns = empty,
    noteEvents = [],
    timeNots = empty,
    renderTime = newAverage 20,
    wakeTimeAnimation = t0System,
    animationDelta = newAverage 20,
    animationTime = newAverage 20,
    zoneRenderTimes = empty,
    zoneAnimationTimes = empty,
    info = emptyRenderInfo,
    tempoCache = iTempo,
    jsoLangs = Map.empty,
    valueMap = Map.empty,
    locoMotion = locoMotion',
    exoLangTest = exoLangTest',
    transMit = transMit',
    miniTidal = miniTidal',
    punctual = punctual',
    cineCer0 = cineCer0',
    hydra = hydra'
    }
