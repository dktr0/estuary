{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.Context where

import Control.Concurrent.MVar
import Data.Map as Map
import Data.Text
import Data.Time
import GHCJS.Types
import GHCJS.DOM.Blob
import GHCJS.DOM.Types (HTMLCanvasElement,HTMLDivElement)
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.Types.TranslatableText
import Estuary.Tidal.Types
import Estuary.Types.Language
import Estuary.Types.Definition
import Estuary.Render.WebDirt
import Estuary.Render.SuperDirt
import Estuary.Types.RenderState
import Estuary.Types.Tempo
import Estuary.Types.EnsembleC
import Estuary.Render.DynamicsMode
import Estuary.Resources
import Estuary.Protocol.Peer
import Sound.MusicW (Node)
import Sound.Punctual.GL (GLContext)
import qualified Sound.Punctual.Resolution as Punctual

-- things the render engine needs, but the UI doesn't need, and which never change
data ImmutableRenderContext = ImmutableRenderContext {
  mainBus :: MainBus,
  webDirt :: WebDirt,
  superDirt :: SuperDirt,
  resources :: Resources
  }

data Context = Context {
  aboutThisServer :: TranslatableText,
  announcements :: Map Day [TranslatableText],
  webDirtOn :: Bool,
  superDirtOn :: Bool,
  canvasOn :: Bool,
  cineCer0ZIndex :: Int,
  punctualZIndex :: Int,
  improvizZIndex :: Int,
  hydraZIndex :: Int,
  theme :: Text,
  unsafeMode :: Bool,
  resolution :: Punctual.Resolution,
  brightness :: Double,
  fpsLimit :: Maybe NominalDiffTime,
  dynamicsMode :: DynamicsMode,
  punctualAudioInputMode :: PunctualAudioInputMode,
  language :: Language,
  ensembleC :: EnsembleC,
  wsStatus :: Text,
  serverLatency :: NominalDiffTime,
  clientCount :: Int,
  videoDivElement :: Maybe HTMLDivElement,
  theVideoDiv :: Maybe JSVal
  }

initialContext :: UTCTime -> Context
initialContext nowUtc = Context {
  aboutThisServer = Map.fromList [(English,"")], -- non-empty in order to suppress "?"
  announcements = Map.empty,
  dynamicsMode = DefaultDynamics,
  punctualAudioInputMode = MicToPunctual,
  language = English,
  theme = "../css-custom/classic.css",
  unsafeMode = False,
  resolution = Punctual.HD,
  brightness = 1.0,
  fpsLimit = Just 0.030,
  ensembleC = emptyEnsembleC nowUtc,
  webDirtOn = True,
  superDirtOn = False,
  canvasOn = True,
  cineCer0ZIndex = -1,
  punctualZIndex = -2,
  improvizZIndex = -3,
  hydraZIndex = -10,
  wsStatus = "",
  serverLatency = 0,
  clientCount = 0,
  videoDivElement = Nothing,
  theVideoDiv = Nothing
}

type ContextChange = Context -> Context

setTheme :: Text -> ContextChange
setTheme x c = c { theme = x }

setLanguage :: Language -> ContextChange
setLanguage x c = c { language = x }

setClientCount :: Int -> ContextChange
setClientCount x c = c { clientCount = x }

modifyEnsembleC :: (EnsembleC -> EnsembleC) -> ContextChange
modifyEnsembleC f c = c { ensembleC = f (ensembleC c) }
