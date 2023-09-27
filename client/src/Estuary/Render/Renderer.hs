{-# LANGUAGE OverloadedStrings #-}
module Estuary.Render.Renderer where

import Data.Text
import Data.Time
import qualified Sound.Tidal.Context as Tidal
import qualified Sound.Punctual.Resolution as Punctual
import qualified Sound.MusicW as MusicW

import Estuary.Types.Tempo
import Estuary.Types.NoteEvent
import Estuary.Types.Definition

data Renderer = Renderer {
  defineZone :: Int -> Definition -> IO (Either Text Text), -- left=error, right=info
  preRender :: Bool -> IO (),
  renderZone :: UTCTime -> UTCTime -> UTCTime -> Bool -> Int -> IO [NoteEvent],
  postRender :: Bool -> IO (),
  clearZone :: Int -> IO (),
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
  defineZone = \_ _ -> pure (Left "Estuary internal error: defineZone is empty default from Estuary.Render.Renderer"),
  preRender = \_ -> pure (),
  renderZone = \_ _ _ _ _ -> pure [],
  postRender = \_ -> pure (),
  clearZone = \_ -> pure (),
  setTempo = \_ -> pure (),
  setBrightness = \_ -> pure (),
  setResolution = \_ -> pure (),
  setValueMap = \_ -> pure (),
  setAudioInput = \_ -> pure (),
  setAudioOutput = \_ -> pure (),
  setNchnls = \_ -> pure ()
  }

