module Estuary.Render.R where

import Data.Text
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.MVar

import Estuary.Render.RenderEnvironment
import Estuary.Render.RenderState
import Estuary.Render.RenderInfo
import Estuary.Types.Setting
import Estuary.Types.ResourceMap

type R = ReaderT RenderEnvironment (StateT RenderState IO)

execR :: RenderEnvironment -> RenderState -> R a -> IO RenderState
execR = ???

askSettings :: R Settings
askSettings = do
  mv <- settings <$> ask
  liftIO $ readMVar mv

askAudioMap :: R AudioMap
askAudioMap = do
  mv <- audioMap <$> ask
  liftIO $ readMVar mv

setZoneError :: Int -> Text -> R ()
setZoneError z t = do
  s <- get
  let oldErrors = errors $ info s
  let newErrors = insert z t oldErrors
  modify' $ \x -> x { info = (info s) { errors = newErrors } }

clearZoneError :: Int -> R ()
clearZoneError z = do
  s <- get
  let oldErrors = errors $ info s
  let newErrors = delete z oldErrors
  modify' $ \x -> x { info = (info s) { errors = newErrors } }

pushNoteEvents :: [NoteEvent] -> R ()
pushNoteEvents xs = modify' $ \x -> x { noteEvents = noteEvents x ++ xs }

pushTidalEvents :: [(UTCTime,Tidal.ControlMap)] -> R ()
pushTidalEvents xs = modify' $ \x -> x { tidalEvents = tidalEvents x ++ xs }
