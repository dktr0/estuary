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

data Context = Context {
  mainBus :: (Node,Node,Node,Node,Node), -- ^ main bus input, delay, pregain, compressor, postgain
  dynamicsMode :: DynamicsMode,
  webDirt :: WebDirt,
  superDirt :: SuperDirt,
  language :: Language,
  theme :: Text,
  ensembleC :: EnsembleC,
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
  canvasElement :: Maybe HTMLCanvasElement,
  videoDivElement :: Maybe HTMLDivElement,
  peerProtocol :: PeerProtocol,
  theVideoDiv :: Maybe JSVal
  }

initialContext :: UTCTime -> Double -> (Node,Node,Node,Node,Node) -> WebDirt -> SuperDirt -> PeerProtocol -> Context
initialContext nowUtc nowAudio mBus wd sd pp = Context {
  mainBus = mBus,
  dynamicsMode = DefaultDynamics,
  webDirt = wd,
  superDirt = sd,
  language = English,
  theme = "../css-custom/classic.css",
  ensembleC = emptyEnsembleC nowUtc,
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
  canvasElement = Nothing,
  videoDivElement = Nothing,
  peerProtocol = pp,
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
