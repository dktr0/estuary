{-# LANGUAGE OverloadedStrings #-}

module Estuary.Languages.Hydra (Estuary.Languages.Hydra.hydra) where

import Data.Time
import Control.Monad.Except
import Data.IntMap as IntMap
import Data.Text as T
import Control.Exception
import Control.Monad.State.Strict
import Sound.MusicW as MusicW
import Control.Monad.Reader
import GHCJS.DOM.Types (HTMLCanvasElement)
import Data.IORef

import Estuary.Render.Renderer
import Estuary.Types.Definition
import Estuary.Types.Tempo
import Estuary.Types.NoteEvent
import Estuary.Languages.Hydra.Types as Hydra
import Estuary.Languages.Hydra.Parser as Hydra
import Estuary.Languages.Hydra.Render as Hydra


hydra :: HTMLCanvasElement -> IO Renderer
hydra canvas = do
  hydras <- newIORef IntMap.empty -- IORef (IntMap Hydra)
  pure $ emptyRenderer {
    define = _define canvas hydras,
    render = _render hydras,
    clear = _clear hydras 
    }

    
_define :: HTMLCanvasElement -> IORef (IntMap Hydra) -> Int -> Definition -> IO (Either Text Text)
_define hCanvas hydras z d = do
  case definitionToRenderingTextProgram d of 
    Nothing -> pure $ Left "internal error in Estuary.Languages.Hydra: defineZone called for a definition that doesn't pertain to a text program"
    Just (_,txt,eTime) -> do
      parseResult <- liftIO $ try $ return $! Hydra.parseHydra txt
      case parseResult of
        Right (Right stmts) -> do
          maybeH <- IntMap.lookup z <$> readIORef hydras
          h <- case maybeH of 
            Just x -> pure x
            Nothing -> newHydra hCanvas
          modifyIORef hydras $ IntMap.insert z h
          Hydra.evaluate h stmts
          pure $ Right ""
        Right (Left parseError) -> pure $ Left $ T.pack $ show parseError
        Left exception -> pure $ Left $ T.pack $ show (exception :: SomeException)
        

_render :: IORef (IntMap Hydra) -> UTCTime -> UTCTime -> UTCTime -> UTCTime -> Bool -> Int -> IO [NoteEvent]
_render hydras tNow tPrevDraw wStart wEnd canDraw z = do
  case canDraw of
    False -> pure []
    True -> do
      hydras' <- readIORef hydras
      case IntMap.lookup z hydras' of 
        Nothing -> pure []
        Just h -> do
          let elapsed = realToFrac $ diffUTCTime tNow tPrevDraw * 1000
          tick h elapsed
          pure []
        

_clear :: IORef (IntMap Hydra) -> Int -> IO ()
_clear zonesRef z = modifyIORef zonesRef $ IntMap.delete z -- TODO: ensure Hydra canvas is also cleared somehow


