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
import Estuary.Types.TextNotation

data Renderer = Renderer {
  define :: Int -> Definition -> IO (Either Text Text), -- left=error, right=info
  clear :: Int -> IO (),
  preRender :: Bool -> UTCTime -> UTCTime -> IO (), -- canDraw nowTime prevDrawTime
  render :: UTCTime -> UTCTime -> UTCTime -> UTCTime -> Bool -> Int -> IO [NoteEvent], -- nowTime prevDrawTime windowStartTime windowEndTime canDraw zone
  postRender :: Bool -> UTCTime -> UTCTime -> IO (), -- canDraw nowTime prevDrawTime
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
  preRender = \_ _ _ -> pure (),
  render = \_ _ _ _ _ _ -> pure [],
  postRender = \_ _ _ -> pure (),
  setTempo = \_ -> pure (),
  setBrightness = \_ -> pure (),
  setResolution = \_ -> pure (),
  setValueMap = \_ -> pure (),
  setAudioInput = \_ -> pure (),
  setAudioOutput = \_ -> pure (),
  setNchnls = \_ -> pure ()
  }

