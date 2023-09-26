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
  
  
type MiniTidalState = IORef (IntMap Tidal.ControlPattern, Tempo)
  
miniTidal :: IO Renderer 
miniTidal = do
  now <- getCurrentTime
  let iTempo = Tempo { freq=0.5, time=now, Tempo.count=0 }
  miniTidalState <- newIORef (IntMap.empty,iTempo)
  pure $ Renderer {
    defineZone = _defineZone miniTidalState,
    preRender = _preRender miniTidalState,
    renderZone = _renderZone miniTidalState,
    postRender = pure (),
    clearZone = _clearZone miniTidalState
  }
  
  
_defineZone :: MiniTidalState -> Int -> Definition -> IO (Maybe Text) -- Just values represent evaluation errors
_defineZone mts z d = do
  case definitionToTidal d of 
    Nothing -> pure $ Just "internal error in Estuary.Languages.MiniTidal: evaluate called for a definition that doesn't pertain to TidalCycles"
    Just (_,txt,eTime) -> do
      parseResult <- liftIO $ (return $! force (tidalParser txt)) `catch` (return . Left . (show :: SomeException -> String))
      case parseResult of
        Right p -> do
          -- setEvaluationTime z eTime -- TODO: recording evaluation time and base notation for zone should be handled by render engine instead of a render module
          mts' <- readIORef mts
          modifyIORef mts $ \(m,t) -> (IntMap.insert z p m,t)
          pure Nothing
        Left err -> pure $ Just $ T.pack err

definitionToTidal :: Definition -> Maybe (TextNotation,Text,UTCTime)
definitionToTidal x = do
  liveTxtProgram <- maybeTextProgram x
  pure $ forRendering liveTxtProgram

tidalParser :: Text -> Either String Tidal.ControlPattern
tidalParser = parseTidal . T.unpack



_preRender :: MiniTidalState -> Tempo -> IO ()
_preRender mts t = modifyIORef mts $ \(m,_) -> (m,t)


-- note/TODO: obviously having a Tidal.ValueMap argument is wrong here - just a temporary hack
_renderZone :: MiniTidalState -> Int -> UTCTime -> UTCTime -> Tidal.ValueMap -> IO [NoteEvent]
_renderZone mts z wStart wEnd vMap = do
  -- s <- get
  -- let lt = renderStart s
  -- let rp = renderPeriod s
  -- let vMap = valueMap s
  controlPattern <- (IntMap.lookup z . fst) <$> readIORef mts -- :: Maybe ControlPattern
  theTempo <- snd <$> readIORef mts
  case controlPattern of
    Just controlPattern' -> do
      let rp = diffUTCTime wEnd wStart
      ns <- liftIO $ (return $! force $ renderTidalPattern vMap wStart rp theTempo controlPattern')
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
        
        
_clearZone :: MiniTidalState -> Int -> IO ()
_clearZone mts z = modifyIORef mts $ \(m,t) -> (IntMap.delete z m, t)

