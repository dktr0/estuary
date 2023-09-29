{-# LANGUAGE OverloadedStrings #-}
module Estuary.Render.Renderer (Renderer(..),emptyRenderer,exoLangToRenderer) where

import Data.Text
import Data.Time
import qualified Sound.Tidal.Context as Tidal
import qualified Sound.Punctual.Resolution as Punctual
import qualified Sound.MusicW as MusicW

import Estuary.Types.Tempo
import Estuary.Types.NoteEvent
import Estuary.Types.Definition
import Estuary.Types.TextNotation
import qualified Estuary.Languages.ExoLang as ExoLang

data Renderer = Renderer {
  define :: Int -> Definition -> IO (Either Text Text), -- left=error, right=info
  clear :: Int -> IO (),
  preRender :: Bool -> UTCTime -> IO (),
  render :: UTCTime -> UTCTime -> UTCTime -> Bool -> Int -> IO [NoteEvent],
  postRender :: Bool -> UTCTime -> IO (),
  setTempo :: Tempo -> IO (),
  setBrightness :: Double -> IO (),
  setResolution :: Punctual.Resolution -> IO (),
  setValueMap :: Tidal.ValueMap -> IO (),
  setAudioInput :: MusicW.Node -> IO (),
  setAudioOutput :: MusicW.Node -> IO (),
  setNchnls :: Int -> IO ()
  }

emptyRenderer :: Renderer
emptyRenderer = Renderer {
  define = \_ _ -> pure (Left "Estuary internal error: defineZone is empty default from Estuary.Render.Renderer"),
  clear = \_ -> pure (),
  preRender = \_ _ -> pure (),
  render = \_ _ _ _ _ -> pure [],
  postRender = \_ _ -> pure (),
  setTempo = \_ -> pure (),
  setBrightness = \_ -> pure (),
  setResolution = \_ -> pure (),
  setValueMap = \_ -> pure (),
  setAudioInput = \_ -> pure (),
  setAudioOutput = \_ -> pure (),
  setNchnls = \_ -> pure ()
  }

exoLangToRenderer :: TextNotation -> ExoLang.ExoLang -> Renderer
exoLangToRenderer tn e = emptyRenderer {
  define = define' tn e,
  clear = ExoLang.clear e,
  preRender = ExoLang.preRender e,
  render = ExoLang.render e,
  postRender = ExoLang.postRender e,
  setTempo = ExoLang.setTempo e  
  -- TODO: open pathways between ExoLang and the remaining setters of Renderer
  }

define' :: TextNotation -> ExoLang.ExoLang -> Int -> Definition -> IO (Either Text Text)
define' tn exoLang z d = do
  case definitionToRenderingTextProgram d of 
    Nothing -> pure $ Left $ "internal error in Estuary.Render.Renderer: defineZone called for a definition that doesn't pertain to a text program passed to exolang for text notation " <> tn
    Just (_,txt,eTime) -> ExoLang.define exoLang z txt eTime

