{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.RenderState where

import Data.Time.Clock
import qualified Data.Map as Map
import Data.IntMap.Strict
import qualified Sound.Tidal.Context as Tidal
import qualified Sound.Punctual.PunctualW as Punctual
import qualified Sound.Punctual.WebGL as Punctual
import qualified Sound.Punctual.Resolution as Punctual
import Sound.MusicW.AudioContext (AudioTime)
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
import qualified Estuary.Languages.TimeNot as TimeNot

newtype LocoMotion = LocoMotion JSVal

instance PToJSVal LocoMotion where pToJSVal (LocoMotion x) = x

instance PFromJSVal LocoMotion where pFromJSVal = LocoMotion

data RenderState = RenderState {
  audioTime :: !Double,
  systemTime :: !UTCTime,
  prevDrawTime :: !UTCTime,
  windowStart :: !UTCTime,
  windowPeriod :: !NominalDiffTime,
  windowEnd :: !UTCTime,

  tempo :: Tempo,
  cachedDefs :: !DefinitionMap, -- the map of definitions as received from the external world via render ops
  baseDefinitions :: !DefinitionMap, -- the map of definitions as actually rendered (eg. with jsolang translations)
  paramPatterns :: !(IntMap Tidal.ControlPattern),
  
  noteEvents :: ![NoteEvent],
  valueMap :: Tidal.ValueMap,
  
  renderTime :: !MovingAverage,
  renderedTime :: !MovingAverage,
  animationDelta :: !MovingAverage, -- time between frame starts, ie. 1/FPS
  zoneRenderTimes :: !(IntMap MovingAverage),
  zoneAnimationTimes :: !(IntMap MovingAverage),
  info :: !RenderInfo,
  
  jsoLangs :: Map.Map Text JSoLang,
  allRenderers :: !(Map.Map Text Renderer),
  activeRenderersMap :: !(IntMap Text),
  activeRenderers :: ![Renderer]
  }


initialRenderState :: MusicW.Node -> MusicW.Node -> HTMLDivElement -> HTMLCanvasElement -> HTMLCanvasElement -> HTMLCanvasElement -> UTCTime -> AudioTime -> IO RenderState
initialRenderState pIn pOut cineCer0Div pCanvas lCanvas hCanvas t0System t0Audio = do
  let iTempo = Tempo { freq = 0.5, time = t0System, count = 0 }
  exoLangTest' <- exoLangRenderer "exolangtest" lCanvas "./exolang.js"
  locoMotion' <- exoLangRenderer "LocoMotion" lCanvas "https://dktr0.github.io/LocoMotion/locoMotion.js"
  transMit' <- exoLangRenderer "TransMit" lCanvas "https://jac307.github.io/TransMit/exolang.js"
  miniTidal' <- MiniTidal.miniTidal iTempo
  punctual' <- Punctual.punctual pCanvas iTempo
  setAudioInput punctual' pIn
  setAudioOutput punctual' pOut
  setNchnls punctual' $ numberOfOutputs pOut
  cineCer0' <- CineCer0.cineCer0 cineCer0Div iTempo
  hydra' <- Hydra.hydra hCanvas
  timeNot' <- TimeNot.timeNot iTempo
  
  pure $ RenderState {
    audioTime = t0Audio,
    systemTime = t0System,
    prevDrawTime = t0System,
    windowStart = t0System,
    windowPeriod = 0,
    windowEnd = t0System,

    tempo = iTempo,
    cachedDefs = empty,
    baseDefinitions = empty,
    paramPatterns = empty,
    
    noteEvents = [],
    valueMap = Map.empty,

    renderTime = newAverage 70,
    renderedTime = newAverage 70,
    animationDelta = newAverage 70,
    zoneRenderTimes = empty,
    zoneAnimationTimes = empty,
    info = emptyRenderInfo,
    
    jsoLangs = Map.empty,
    allRenderers = Map.fromList [
      ("exolangtest",exoLangTest'),
      ("locomotion",locoMotion'),
      ("transmit",transMit'),
      ("minitidal",miniTidal'),
      ("punctual",punctual'),
      ("cinecer0",cineCer0'),
      ("hydra",hydra'),
      ("timenot",timeNot')
      ],
    activeRenderersMap = empty,
    activeRenderers = []
    }
