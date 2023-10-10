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
import Estuary.Render.Renderer
import Estuary.Types.Definition
import Estuary.Types.Tempo


punctual :: HTMLCanvasElement -> Tempo -> IO Renderer
punctual canvas iTempo = do
  punctual' <- Punctual.new canvas
  Punctual.setTempo punctual' iTempo
  pure $ emptyRenderer {
    define = _define punctual',
    render = \tNow _ _ _ canDraw z -> Punctual.render punctual' canDraw z tNow >> pure [],
    postRender = \canDraw _ _ -> Punctual.postRender punctual' canDraw,
    clear = Punctual.clear punctual',
    setTempo = Punctual.setTempo punctual',
    setResolution = Punctual.setResolution punctual',
    setBrightness = Punctual.setBrightness punctual',
    setAudioInput = Punctual.setAudioInput punctual',
    setAudioOutput = Punctual.setAudioOutput punctual',
    setNchnls = Punctual.setNchnls punctual'
    }
    
_define :: Punctual.Punctual -> Int -> Definition -> IO (Either Text Text)
_define p z d = do
  case definitionToRenderingTextProgram d of 
    Nothing -> pure $ Left "internal error in Estuary.Languages.Punctual: defineZone called for a definition that doesn't pertain to a text program"
    Just (_,txt,eTime) -> Punctual.evaluate p z txt eTime

