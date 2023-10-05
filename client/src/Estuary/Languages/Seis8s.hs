{-# LANGUAGE OverloadedStrings #-}

module Estuary.Languages.Seis8s (seis8s) where

import Data.Time
import Control.Monad.Except
import Data.IntMap as IntMap
import Data.Map as Map
import Data.Text as T
import Control.Exception hiding (evaluate)
import Control.Monad.State.Strict
import Sound.MusicW.AudioContext
import Control.Monad.Reader
import qualified Sound.Tidal.Context as Tidal
-- import Control.DeepSeq
import Data.Maybe
import Sound.Tidal.Parse
import Data.IORef
import Data.Time

import qualified Sound.Seis8s.Parser as Seis8s
import Sound.Seis8s.Program

import Estuary.Types.Tempo as Tempo
import Estuary.Types.NoteEvent
import Estuary.Types.Definition
import Estuary.Types.Live
import Estuary.Types.TextNotation
import Estuary.Render.Renderer
  
  
seis8s :: Tempo -> IO Renderer 
seis8s iTempo = do
  now <- getCurrentTime
  zonesRef <- newIORef IntMap.empty
  tempoRef <- newIORef iTempo
  pure $ emptyRenderer {
    define = _define zonesRef,
    clear = _clear zonesRef,
    render = _render zonesRef tempoRef,
    setTempo = writeIORef tempoRef
    }
  
  
_define :: IORef (IntMap Program) -> Int -> Definition -> IO (Either Text Text)
_define zonesRef z d = do
  case definitionToRenderingTextProgram d of 
    Nothing -> pure $ Left "internal error in Estuary.Languages.Seis8s: defineZone called for a definition that doesn't pertain to a text program"
    Just (_,txt,eTime) -> do
      parseResult <- liftIO $ (return $! Seis8s.parseLang $ T.unpack txt) `catch` (return . Left . (show :: SomeException -> String))
      case parseResult of
        Right p -> do
          modifyIORef zonesRef $ IntMap.insert z p
          pure $ Right ""
        Left err -> pure $ Left $ T.pack err


_clear :: IORef (IntMap Program) -> Int -> IO ()
_clear zonesRef z = modifyIORef zonesRef $ IntMap.delete z


_render :: IORef (IntMap Program) -> IORef Tempo -> UTCTime -> UTCTime -> UTCTime -> Bool -> Int -> IO [NoteEvent]
_render zonesRef tempoRef _ wStart wEnd _ z = do
  pMaybe <- IntMap.lookup z <$> readIORef zonesRef
  case pMaybe of
    Just p -> do
      tempo <- readIORef tempoRef
      let rp = diffUTCTime wEnd wStart
      ns <- liftIO $ (return $! Seis8s.render p tempo wStart wEnd)
        `catch` (\e -> putStrLn (show (e :: SomeException)) >> return [])
      mapM datumMapEventToNoteEvent ns
    Nothing -> pure []
    

