{-# LANGUAGE OverloadedStrings #-}
module Estuary.Languages.TimeNot (timeNot) where

import qualified Data.IntMap as IntMap
import Data.Text (Text)
import Data.Time
import qualified Data.Text as T
import Control.Monad.State.Strict
import GHCJS.Types
import GHCJS.Marshal -- .Internal
import Data.IORef
import Data.IntMap as IntMap

import Estuary.Types.NoteEvent
import Estuary.Render.Renderer
import Estuary.Types.Definition
import Estuary.Types.Tempo
import Estuary.Render.ForeignTempo
import Estuary.Languages.ExoLang


timeNot :: Tempo -> IO Renderer
timeNot iTempo = do
  zonesRef <- newIORef IntMap.empty
  tempoRef <- newIORef iTempo
  pure $ emptyRenderer {
    define = _define zonesRef,
    clear = \z -> modifyIORef zonesRef (IntMap.delete z),
    render = _render zonesRef tempoRef,
    setTempo = writeIORef tempoRef
    }
    
    
_define :: IORef (IntMap JSVal) -> Int -> Definition -> IO (Either Text Text)
_define zonesRef z d = do
  case definitionToRenderingTextProgram d of 
    Nothing -> pure $ Left "internal error in Estuary.Languages.TimeNot: defineZone called for a definition that doesn't pertain to a text program"
    Just (_,txt,eTime) -> do
      zones <- readIORef zonesRef
      timekNot <- case IntMap.lookup z zones of
        Just j -> pure j
        Nothing -> do
          x <- launch
          modifyIORef zonesRef $ IntMap.insert z x
          pure x
      exoResultToEither <$> evaluate timekNot txt (utcTimeToWhenPOSIX eTime)
   
--  render :: UTCTime -> UTCTime -> UTCTime -> UTCTime -> Bool -> Int -> IO [NoteEvent], -- nowTime prevDrawTime windowStartTime windowEndTime canDraw zone


_render :: IORef (IntMap JSVal) -> IORef Tempo -> UTCTime -> UTCTime -> UTCTime -> UTCTime -> Bool -> Int -> IO [NoteEvent]
_render zonesRef tempoRef _ _ wStart wEnd _ z = do
  let wStart' = utcTimeToWhenPOSIX wStart
  let wEnd' = utcTimeToWhenPOSIX wEnd
  zones <- readIORef zonesRef
  case IntMap.lookup z zones of
    Just timekNot -> do
      tempo <- readIORef tempoRef
      _setTempo timekNot $ toForeignTempo tempo
      j <- _scheduleEvents timekNot wStart' wEnd'
      jsValToNoteEvents j
    Nothing -> pure []


foreign import javascript safe
  "window.timekNot.launch()"
  launch :: IO JSVal

foreign import javascript safe
  "window.timekNot.evaluate($1,$2,$3)"
  evaluate :: JSVal -> Text -> Double -> IO ExoResult

foreign import javascript safe
  "window.timekNot.scheduleNoteEvents($1,$2,$3)"
  _scheduleEvents :: JSVal -> Double -> Double -> IO JSVal

foreign import javascript unsafe
  "window.timekNot.setTempo($1,$2)"
  _setTempo :: JSVal -> ForeignTempo -> IO ()

