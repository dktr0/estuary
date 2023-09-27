module Estuary.Render.Renderer where

import Data.Text
import Data.Time
import qualified Sound.Tidal.Context as Tidal

import Estuary.Types.Tempo
import Estuary.Types.NoteEvent
import Estuary.Types.Definition

data Renderer = Renderer {
  defineZone :: Int -> Definition -> IO (Maybe Text),
  preRender :: IO (),
  renderZone :: UTCTime -> UTCTime -> UTCTime -> Bool -> Int -> IO [NoteEvent],
  postRender :: IO (),
  clearZone :: Int -> IO (),
  setTempo :: Tempo -> IO (),
  setBrightness :: Double -> IO (),
  setResolution :: Resolution -> IO (),
  setValueMap :: Tidal.ValueMap -> IO (),
  setAudioInput :: MusicW.Node -> IO (),
  setAudioOutput :: MusicW.Node -> IO (),
  setNchnls :: Int -> IO ()
  }

emptyRenderer :: Renderer
emptyRenderer = Renderer {
  defineZone = \_ _ -> pure Nothing,
  preRender = pure (),
  renderZone = \_ _ _ _ _ -> pure [],
  postRender = pure (),
  clearZone = \_ -> pure (),
  setTempo = \_ -> pure (),
  setBrightness = \_ -> pure (),
  setResolution = \_ -> pure (),
  setValueMap = \_ -> pure (),
  setAudioInput = \_ -> pure (),
  setAudioOutput = \_ -> pure (),
  setNchnls = \_ -> pure (),
  }

