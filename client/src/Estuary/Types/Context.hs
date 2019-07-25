{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.Context where

import Control.Concurrent.MVar

import Data.IntMap.Strict as Map

import Data.Text
import Data.Time

import GHCJS.Types

import GHCJS.DOM.Blob
import GHCJS.DOM.Types (HTMLCanvasElement)
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.Tidal.Types
import Estuary.Types.Language
import Estuary.Types.Definition
import Estuary.Types.Resources
import Estuary.Types.Samples
import Estuary.WebDirt.WebDirt
import Estuary.WebDirt.SuperDirt
import Estuary.RenderState
import Estuary.Types.Tempo
import Estuary.Types.CanvasState
import Estuary.Types.EnsembleState
import Estuary.Render.AudioContext
import Estuary.Render.DynamicsMode
import Estuary.Render.LocalResources
import Estuary.Protocol.Peer
import Sound.MusicW (Node)

data Context = Context {
  mainBus :: (Node,Node,Node,Node), -- ^ main bus input, pregain, compressor, postgain
  dynamicsMode :: DynamicsMode,
  webDirt :: WebDirt,
  superDirt :: SuperDirt,
  language :: Language,
  theme :: Text,
  ensembleState :: EnsembleState,
  samples :: SampleMap,
  localResourceServers :: LocalResourceServers,
  privateResources :: Resources, -- ^ The user uploaded, browser local, resource set.
  resources :: Resources, -- ^ The effective resource set.
  webDirtOn :: Bool,
  superDirtOn :: Bool,
  canvasOn :: Bool,
  wsStatus :: Text,
  serverLatency :: NominalDiffTime,
  clientCount :: Int,
  canvasState :: MVar CanvasState,
  canvasElement :: Maybe HTMLCanvasElement,
  peerProtocol :: JSVal,
  theVideoDiv :: Maybe JSVal
  }

initialContext :: UTCTime -> (Node,Node,Node,Node) -> WebDirt -> SuperDirt -> MVar CanvasState -> JSVal -> Context
initialContext now mBus wd sd mv pp = Context {
  mainBus = mBus,
  dynamicsMode = DefaultDynamics,
  webDirt = wd,
  superDirt = sd,
  language = English,
  theme = "../css-custom/classic.css",
  ensembleState = newEnsembleState $ Tempo { cps = 0.5, at = now, beat = 0.0 },
  localResourceServers = emptyLocalResourceServers,
  privateResources = emptyResources,
  resources = emptyResources,
  samples = emptySampleMap,
  webDirtOn = True,
  superDirtOn = False,
  canvasOn = True,
  wsStatus = "",
  serverLatency = 0,
  clientCount = 0,
  canvasState = mv,
  canvasElement = Nothing,
  peerProtocol = pp,
  theVideoDiv = Nothing
}

type ContextChange = Context -> Context

setTheme :: Text -> ContextChange
setTheme x c = c {theme = x}

setLanguage :: Language -> ContextChange
setLanguage x c = c { language = x }

setClientCount :: Int -> ContextChange
setClientCount x c = c { clientCount = x }

modifyEnsemble :: (EnsembleState -> EnsembleState) -> ContextChange
modifyEnsemble f c = c { ensembleState = f (ensembleState c) }

setSampleMap :: SampleMap -> ContextChange
setSampleMap x c = c { samples = x}

-- TODO the resource sets should be lenses so they can be more properly passed around
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
