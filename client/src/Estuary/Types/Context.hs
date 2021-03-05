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
-- import Estuary.Types.Resources
import Estuary.Types.Samples
import Estuary.Render.WebDirt
import Estuary.Render.SuperDirt
import Estuary.Types.RenderState
import Estuary.Types.Tempo
import Estuary.Types.EnsembleC
import Estuary.Types.ResourceMap
import Estuary.Render.DynamicsMode
import Estuary.Render.LocalResources
import Estuary.Protocol.Peer
import Sound.MusicW (Node)
import Sound.Punctual.GL (GLContext)
import qualified Sound.Punctual.Resolution as Punctual

-- things the render engine needs, but the UI doesn't need, and which never change
data ImmutableRenderContext = ImmutableRenderContext {
  mainBus :: (Node,Node,Node,Node,Node,Node), -- ^ main bus input, delay, pregain, compressor, postgain
  webDirt :: WebDirt,
  superDirt :: SuperDirt,
  mic :: Node
  }

out :: ImmutableRenderContext -> Node
out (ImmutableRenderContext (_,_,_,_,x,_) _ _ _) = x

data Context = Context {
  aboutThisServer :: TranslatableText,
  announcements :: Map Day [TranslatableText],
  webDirtOn :: Bool,
  superDirtOn :: Bool,
  canvasOn :: Bool,
  theme :: Text,
  unsafeMode :: Bool,
  resolution :: Punctual.Resolution,
  brightness :: Double,
  fpsLimit :: Maybe NominalDiffTime,
  dynamicsMode :: DynamicsMode,
  language :: Language,
  audioMap :: AudioMap,
--  localResourceServers :: LocalResourceServers,
--  privateResources :: Resources, -- ^ The user uploaded, browser local, resource set.
--  resources :: Resources, -- ^ The effective resource set.
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
  language = English,
  theme = "../css-custom/classic.css",
  unsafeMode = False,
  resolution = Punctual.HD,
  brightness = 1.0,
  fpsLimit = Just 0.030,
  ensembleC = emptyEnsembleC nowUtc,
--  localResourceServers = emptyLocalResourceServers,
--  privateResources = emptyResources,
--  resources = emptyResources,
  audioMap = emptyResourceMap,
  webDirtOn = True,
  superDirtOn = False,
  canvasOn = True,
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

setAudioMap :: AudioMap -> ContextChange
setAudioMap x c = c { audioMap = x}

-- TODO the resource sets should be lenses so they can be more properly passed around
{-
addPrivateResource :: (LocalResourceServers -> LocalResourceServer m)
    -> (LocalResourceServers -> LocalResourceServer m -> LocalResourceServers)
    -> (Resources -> ResourceMap m)
    -> (Resources -> ResourceMap m -> Resources)
    -> Text
    -> Blob
    -> Resource m
    -> ContextChange
addPrivateResource getServer setServer getResourceMap setResourceMap group blob resource c = c {
  localResourceServers = setServer (localResourceServers c) $
    uploadLocal group (resourceFileName resource) blob (getServer $ localResourceServers c),
  privateResources = setResourceMap (privateResources c) $
    insertResource group resource (getResourceMap $ privateResources c)
}
-}
