module Estuary.Render.Renderer where

import Data.Text
import Data.Time
import qualified Sound.Tidal.Context as Tidal

import Estuary.Types.Tempo
import Estuary.Types.NoteEvent
import Estuary.Types.Definition


data Renderer = Renderer {
  defineZone :: Int -> Definition -> IO (Maybe Text),
  preRender :: Tempo -> IO (),
  renderZone :: Int -> UTCTime -> UTCTime -> Tidal.ValueMap -> IO [NoteEvent], -- TODO: add a flag for whether we are in or out of animationFrame callback ("noDraw"?)
  postRender :: IO (),
  clearZone :: Int -> IO ()
  }


emptyRenderer :: Renderer
emptyRenderer = Renderer {
  defineZone = \_ _ -> pure Nothing,
  preRender = \_ -> pure (),
  renderZone = \_ _ _ _ -> pure [],
  postRender = pure (),
  clearZone = \_ -> pure ()
  }
