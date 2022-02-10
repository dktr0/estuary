{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.Context where

import Data.Map as Map
import Data.Text
import Data.Time
import GHCJS.Types
import GHCJS.DOM.Blob
import GHCJS.DOM.Types (HTMLCanvasElement,HTMLDivElement)
import Data.Text (Text)

import Estuary.Types.TranslatableText
import Estuary.Types.EnsembleC
import Estuary.Types.Language

data Context = Context {
  aboutThisServer :: TranslatableText,
  announcements :: Map Day [TranslatableText],
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
  ensembleC = emptyEnsembleC nowUtc,
  wsStatus = "",
  serverLatency = 0,
  clientCount = 0,
  videoDivElement = Nothing,
  theVideoDiv = Nothing
}

type ContextChange = Context -> Context

setClientCount :: Int -> ContextChange
setClientCount x c = c { clientCount = x }

modifyEnsembleC :: (EnsembleC -> EnsembleC) -> ContextChange
modifyEnsembleC f c = c { ensembleC = f (ensembleC c) }
