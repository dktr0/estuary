{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Estuary.Languages.CineCer0 (cineCer0) where

import Data.Time
import Control.Monad.Except
import Data.IntMap as IntMap
import Data.Text as T
import Control.Exception
import Control.Monad.State.Strict
import Sound.MusicW.AudioContext
import Control.Monad.Reader
import Data.IORef
import GHCJS.DOM.Types hiding (Text)

import qualified Estuary.Languages.CineCer0.CineCer0State as CineCer0
import qualified Estuary.Languages.CineCer0.Spec as CineCer0
import qualified Estuary.Languages.CineCer0.Parser as CineCer0

import Estuary.Render.R
import Estuary.Render.Renderer
import Estuary.Types.RenderState
import Estuary.Types.RenderInfo
import Estuary.Types.TextNotation
import Estuary.Render.MainBus
import Estuary.Types.Tempo
import Estuary.Types.Ensemble
import Estuary.Types.EnsembleC
import Estuary.Types.Tempo
import Estuary.Types.Definition
import Estuary.Types.NoteEvent


cineCer0 :: HTMLDivElement -> Tempo -> IO Renderer
cineCer0 videoDiv tempo = do
  tempoRef <- newIORef tempo
  specsRef <- newIORef IntMap.empty -- :: IORef (IntMap CineCer0.Spec)
  statesRef <- newIORef IntMap.empty -- :: IORef (IntMap CineCer0.CineCer0State)
  pure $ emptyRenderer {
    define = _define specsRef,
    clear = _clear specsRef statesRef,
    render = _render videoDiv tempoRef specsRef statesRef,
    setTempo = writeIORef tempoRef
    }


_define :: IORef (IntMap CineCer0.Spec) -> Int -> Definition -> IO (Either Text Text)
_define specsRef z d = do
  case definitionToRenderingTextProgram d of 
    Nothing -> pure $ Left "internal error in Estuary.Languages.CineCer0: defineZone called for a definition that doesn't pertain to a text program"
    Just (_,txt,eTime) -> do
      let parseResult :: Either String CineCer0.Spec = CineCer0.cineCer0 eTime $ T.unpack txt
      case parseResult of
        Right spec -> do
          modifyIORef specsRef $ IntMap.insert z spec
          pure $ Right ""
        Left err -> pure $ Left $ T.pack err 
    

_clear :: IORef (IntMap CineCer0.Spec) -> IORef (IntMap CineCer0.CineCer0State) -> Int -> IO ()
_clear specsRef statesRef z = do
  specs <- readIORef specsRef
  states <- readIORef statesRef
  case (IntMap.lookup z states) of
    Just x -> liftIO $ CineCer0.deleteCineCer0State x
    Nothing -> return ()
  modifyIORef specsRef $ IntMap.delete z
  modifyIORef statesRef $ IntMap.delete z


_render :: HTMLDivElement -> IORef Tempo -> IORef (IntMap CineCer0.Spec) -> IORef (IntMap CineCer0.CineCer0State) -> UTCTime -> UTCTime -> UTCTime -> Bool -> Int -> IO [NoteEvent]
_render videoDiv tempoRef specsRef statesRef tNow _ _ canDraw z = do
  tempo <- readIORef tempoRef
  specs <- readIORef specsRef
  states <- readIORef statesRef
  case IntMap.lookup z specs of
    Just spec -> do
      let prevState = IntMap.findWithDefault (CineCer0.emptyCineCer0State videoDiv) z states
      newState <- CineCer0.updateCineCer0State tempo tNow spec prevState
      modifyIORef statesRef $ IntMap.insert z newState      
    Nothing -> pure ()
  pure []


