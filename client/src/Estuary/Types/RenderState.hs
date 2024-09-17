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
  -- baseDefinitions :: !DefinitionMap, -- the map of definitions as actually rendered (eg. with jsolang translations)
  paramPatterns :: !(IntMap Tidal.ControlPattern),
  
  noteEvents :: ![NoteEvent],
  valueMap :: Tidal.ValueMap,
  
  renderTime :: !MovingAverage,
  renderedTime :: !MovingAverage,
  animationDelta :: !MovingAverage, -- time between frame starts, ie. 1/FPS
  zoneRenderTimes :: !(IntMap MovingAverage),
  zoneAnimationTimes :: !(IntMap MovingAverage),
  
  jsoLangs :: Map.Map Text JSoLang,
  -- activeRenderersMap :: !(IntMap Text),
  -- activeRenderers :: ![Renderer],
  
  exoLangCanvas :: HTMLCanvasElement -- this is a temporary hack, will be replaced with a suitable div soon
  }


initialRenderState :: HTMLDivElement -> HTMLCanvasElement -> HTMLCanvasElement -> HTMLCanvasElement -> UTCTime -> AudioTime -> IO RenderState
initialRenderState cineCer0Div pCanvas elCanvas hCanvas t0System t0Audio = do
  let iTempo = Tempo { freq = 0.5, time = t0System, count = 0 }
  pure $ RenderState {
    audioTime = t0Audio,
    systemTime = t0System,
    prevDrawTime = t0System,
    windowStart = t0System,
    windowPeriod = 0,
    windowEnd = addUTCTime 0.3 t0System,

    tempo = iTempo,
    cachedDefs = empty,
    -- baseDefinitions = empty,
    paramPatterns = empty,
    
    noteEvents = [],
    valueMap = Map.empty,

    renderTime = newAverage 70,
    renderedTime = newAverage 70,
    animationDelta = newAverage 70,
    zoneRenderTimes = empty,
    zoneAnimationTimes = empty,
    
    jsoLangs = Map.empty,
    -- activeRenderersMap = empty,
    -- activeRenderers = [],
    
    exoLangCanvas = elCanvas
    }
