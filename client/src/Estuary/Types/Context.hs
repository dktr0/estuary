{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.Context where

import Control.Concurrent.MVar

import Data.IntMap.Strict as Map

import Data.Text
import Data.Time

import GHCJS.Types

import GHCJS.DOM.Blob
import GHCJS.DOM.Types (HTMLCanvasElement,HTMLDivElement)
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.Tidal.Types
import Estuary.Types.Language
import Estuary.Types.Definition
import Estuary.Types.Resources
import Estuary.Types.Samples
import Estuary.WebDirt.WebDirt
import Estuary.WebDirt.SuperDirt
import Estuary.Types.RenderState
import Estuary.Types.Tempo
import Estuary.Types.EnsembleC
import Estuary.Render.DynamicsMode
import Estuary.Render.LocalResources
import Estuary.Protocol.Peer
import Sound.MusicW (Node)
import Sound.Punctual.GL (GLContext)

-- things the render engine needs, but the UI doesn't need, and which never change
data ImmutableRenderContext = ImmutableRenderContext {
  mainBus :: (Node,Node,Node,Node,Node,Node,JSVal), -- ^ main bus input, delay, pregain, compressor, postgain, analyser, analyserArray,
  webDirt :: WebDirt,
  superDirt :: SuperDirt
  }

data Context = Context {
  webDirtOn :: Bool,
  superDirtOn :: Bool,
  canvasOn :: Bool,
  theme :: Text,
  dynamicsMode :: DynamicsMode,
  language :: Language,
--  samples :: SampleMap,
--  localResourceServers :: LocalResourceServers,
--  privateResources :: Resources, -- ^ The user uploaded, browser local, resource set.
--  resources :: Resources, -- ^ The effective resource set.
  ensembleC :: EnsembleC,
  wsStatus :: Text,
  serverLatency :: NominalDiffTime,
  clientCount :: Int,
  canvasElement :: Maybe HTMLCanvasElement,
  videoDivElement :: Maybe HTMLDivElement,
  theVideoDiv :: Maybe JSVal
  }

initialContext :: UTCTime -> Context
initialContext nowUtc = Context {
  dynamicsMode = DefaultDynamics,
  language = English,
  theme = "../css-custom/classic.css",
  ensembleC = emptyEnsembleC nowUtc,
--  localResourceServers = emptyLocalResourceServers,
--  privateResources = emptyResources,
--  resources = emptyResources,
--  samples = emptySampleMap,
  webDirtOn = True,
  superDirtOn = False,
  canvasOn = True,
  wsStatus = "",
  serverLatency = 0,
  clientCount = 0,
  canvasElement = Nothing,
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

-- setSampleMap :: SampleMap -> ContextChange
-- setSampleMap x c = c { samples = x}

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
