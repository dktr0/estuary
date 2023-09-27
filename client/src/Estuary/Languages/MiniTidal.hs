{-# LANGUAGE OverloadedStrings #-}

module Estuary.Languages.MiniTidal (Renderer(..),miniTidal,renderTidalPattern) where

import Data.Time
import Control.Monad.Except
import Data.IntMap as IntMap
import Data.Text as T
import Control.Exception hiding (evaluate)
import Control.Monad.State.Strict
import Sound.MusicW.AudioContext
import Control.Monad.Reader
import qualified Sound.Tidal.Context as Tidal
import Control.DeepSeq
import Data.Maybe
import Sound.Tidal.Parse
import Data.IORef
import Data.Time

import Estuary.Types.Tempo as Tempo
import Estuary.Types.NoteEvent
import Estuary.Types.Definition
import Estuary.Types.Live
import Estuary.Types.TextNotation
import Estuary.Render.Renderer
  
    
miniTidal :: IO Renderer 
miniTidal = do
  now <- getCurrentTime
  zonesRef <- newIORef IntMap.empty
  tempoRef <- newIORef $ Tempo {freq=0.5, time=now, Tempo.count=0}
  valueMapRef <- newIORef ???/
  pure $ emptyRenderer {
    defineZone = _defineZone zonesRef,
    renderZone = _renderZone zonesRef tempoRef valueMapRef,
    postRender = pure (),
    clearZone = _clearZone zonesRef,
    setTempo = writeIORef tempoRef,
    setValueMap = writeIORef valueMapRef
    }
  
  
_defineZone :: IORef (IntMap Tidal.ControlPattern) -> Int -> Definition -> IO (Maybe Text) -- Just values represent evaluation errors
_defineZone zonesRef z d = do
  case definitionToRenderingTextProgram d of 
    Nothing -> pure $ Just "internal error in Estuary.Languages.MiniTidal: defineZone called for a definition that doesn't pertain to a text program"
    Just (_,txt,eTime) -> do
      parseResult <- liftIO $ (return $! force (tidalParser txt)) `catch` (return . Left . (show :: SomeException -> String))
      case parseResult of
        Right p -> do
          -- setEvaluationTime z eTime -- TODO: confirm that recording evaluation time and base notation are handled by render engine instead of a render module
          modifyIORef zonesRef $ IntMap.insert z p
          pure Nothing
        Left err -> pure $ Just $ T.pack err


tidalParser :: Text -> Either String Tidal.ControlPattern
tidalParser = parseTidal . T.unpack


_renderZone :: IORef (IntMap Tidal.ControlPattern) -> IORef Tempo -> IORef Tidal.ValueMap -> UTCTime -> UTCTime -> UTCTime -> Bool -> Int -> IO [NoteEvent]
_renderZone zonesRef tempoRef valueMapRef z _ wStart wEnd _ = do
  controlPattern <- (IntMap.lookup z . fst) <$> readIORef zonesRef -- :: Maybe ControlPattern
  case controlPattern of
    Just controlPattern' -> do
      tempo <- readIORef tempoRef
      valueMap <- readIORef valueMapRef
      let rp = diffUTCTime wEnd wStart
      ns <- liftIO $ (return $! force $ renderTidalPattern valueMap wStart rp tempo controlPattern')
        `catch` (\e -> putStrLn (show (e :: SomeException)) >> return [])
      pure $ tidalEventToNoteEvent <$> ns
    Nothing -> pure []
    
renderTidalPattern :: Tidal.ValueMap -> UTCTime -> NominalDiffTime -> Tempo -> Tidal.ControlPattern -> [(UTCTime,Tidal.ValueMap)]
renderTidalPattern vMap start range t p = events''
  where
    start' = (realToFrac $ diffUTCTime start (time t)) * freq t + Tempo.count t -- start time in cycles since beginning of tempo
    end = realToFrac range * freq t + start' -- end time in cycles since beginning of tempo
    a = Tidal.Arc (toRational start') (toRational end)
    events = Tidal.query p $ Tidal.State a vMap
    events' = Prelude.filter Tidal.eventHasOnset events
    events'' = f <$> events'
    f e = (utcTime,Tidal.value e)
      where
        utcTime = addUTCTime (realToFrac ((fromRational w1 - Tempo.count t)/freq t)) (time t)
        w1 = Tidal.start $ fromJust $ Tidal.whole e    
        
        
_clearZone :: IORef (IntMap Tidal.ControlPattern) -> Int -> IO ()
_clearZone zonesRef z = modifyIORef zonesRef $ IntMap.delete z

