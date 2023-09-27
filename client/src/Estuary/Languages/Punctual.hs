{-# LANGUAGE OverloadedStrings #-}

module Estuary.Languages.Punctual (punctual) where

import Data.Time
import Control.Monad.Except
import Data.IntMap as IntMap
import Data.Text as T
import Control.Exception
import Control.Monad.State.Strict
import Sound.MusicW as MusicW
import Control.Monad.Reader
import GHCJS.DOM.Types (HTMLCanvasElement)

import qualified Sound.Punctual as Punctual
-- import qualified Sound.Punctual.Resolution as Punctual

-- import Estuary.Render.R
-- import Estuary.Types.TextNotation
-- import Estuary.Render.MainBus
-- import Estuary.Types.Tempo
-- import Estuary.Types.Ensemble
-- import Estuary.Types.EnsembleC
import Estuary.Render.Renderer
import Estuary.Types.Definition


punctual :: HTMLCanvasElement -> IO Renderer
punctual canvas = do
  punctual' <- Punctual.new canvas
  pure $ emptyRenderer {
    defineZone = _defineZone punctual',
    renderZone = \tNow _ _ canDraw z -> Punctual.render punctual' canDraw z tNow >> pure [],
    postRender = Punctual.postRender punctual',
    clearZone = Punctual.clear punctual',
    setTempo = Punctual.setTempo punctual',
    setResolution = Punctual.setResolution punctual',
    setBrightness = Punctual.setBrightness punctual',
    setAudioInput = Punctual.setAudioInput punctual',
    setAudioOutput = Punctual.setAudioOutput punctual',
    setNchnls = Punctual.setNchnls punctual'
    }
    
_defineZone :: Punctual.Punctual -> Int -> Definition -> IO (Either Text Text)
_defineZone p z d = do
  case definitionToRenderingTextProgram d of 
    Nothing -> pure $ Left "internal error in Estuary.Languages.Punctual: defineZone called for a definition that doesn't pertain to a text program"
    Just ("Punctual",txt,eTime) -> Punctual.evaluate p z txt eTime
    Just _ -> pure $ Left "internal error in Estuary.Languages.Punctual: defineZone called for a definition that pertains to a language other than Punctual"

