{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.Context where

import Data.Time
import Data.IntMap.Strict
import Control.Concurrent.MVar
import GHCJS.Types
import GHCJS.DOM.Types (HTMLCanvasElement)
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.Tidal.Types
import Estuary.Types.Language
import Estuary.Types.Definition
import Estuary.Types.Samples
import Estuary.WebDirt.WebDirt
import Estuary.WebDirt.SuperDirt
import Estuary.RenderState
import Estuary.Types.Tempo
import Estuary.Types.CanvasState
import Estuary.Types.EnsembleState
import Estuary.Render.AudioContext
import Estuary.Render.DynamicsMode
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
--  tempo :: Tempo, -- factoring these three out since they are part of ensembleState
--  activeDefsEnsemble :: Text, -- ^ The name of the ensemble that the current definitions in the context belong to.
--  definitions :: DefinitionMap,
  samples :: SampleMap,
  webDirtOn :: Bool,
  superDirtOn :: Bool,
  canvasOn :: Bool,
  wsStatus :: Text,
  serverLatency :: NominalDiffTime,
  clientCount :: Int,
  canvasState :: MVar CanvasState,
  canvasElement :: Maybe HTMLCanvasElement,
  peerProtocol :: JSVal
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
--  activeDefsEnsemble = "",
--  definitions = empty,
  samples = emptySampleMap,
  webDirtOn = True,
  superDirtOn = False,
  canvasOn = True,
  wsStatus = "",
  serverLatency = 0,
  clientCount = 0,
  canvasState = mv,
  canvasElement = Nothing,
  peerProtocol = pp
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

{- setDefinitions :: (Text, DefinitionMap) -> ContextChange
setDefinitions (x, y) c = c {
  activeDefsEnsemble = x,
  definitions = y
} -}

setSampleMap :: SampleMap -> ContextChange
setSampleMap x c = c { samples = x}
