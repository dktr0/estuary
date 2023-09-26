{-# LANGUAGE OverloadedStrings #-}

module Estuary.Languages.Punctual (punctual) where

import Data.Time
import Control.Monad.Except
import Data.IntMap as IntMap
import Data.Text as T
import Control.Exception
import Control.Monad.State.Strict
import Sound.MusicW.AudioContext
import Control.Monad.Reader

import qualified Sound.Punctual as Punctual
import qualified Sound.Punctual.Resolution as Punctual

import Estuary.Render.R
import Estuary.Render.RenderEnvironment
import Estuary.Render.TextNotationRenderer
import Estuary.Types.RenderState
import Estuary.Types.RenderInfo
import Estuary.Types.TextNotation
import Estuary.Render.MainBus
import Estuary.Types.Tempo
import Estuary.Types.Ensemble
import Estuary.Types.EnsembleC
import Estuary.Render.Renderer


punctual :: Tempo -> MusicW.Node -> MusicW.Node -> HTMLCanvasElement -> IO TextNotationRenderer
punctual iTempo audioIn audioOut canvas = do
  punctual <- Punctual.new iTempo audioIn audioOut 2 canvas
  pure $ emptyTextNotationRenderer {
    parseZone = _parseZone punctual,
    clearZone' = _clearZone punctual,
    preAnimationFrame = _preAnimationFrame punctual,
    zoneAnimationFrame = _zoneAnimationFrame punctual,
    postAnimationFrame = _postAnimationFrame punctual
    }


_parseZone :: Punctual.Punctual -> Int -> Text -> UTCTime -> R ()
_parseZone p z txt eTime = do
  rEnv <- ask
  liftIO $ do
    pIn <- getPunctualInput $ mainBus rEnv
    pOut <- getMainBusInput $ mainBus rEnv
    nChnls <- getAudioOutputs $ mainBus rEnv
    Punctual.setAudioInput p pIn
    Punctual.setAudioOutput p pOut
    Punctual.setNchnls p nChnls
  x <- liftIO $ Punctual.evaluate p z txt eTime
  case x of 
    Right _ -> do
      setBaseNotation z "Punctual"
      setEvaluationTime z eTime
      clearZoneError z
    Left err -> setZoneError z err
    

_clearZone :: Punctual.Punctual -> Int -> R ()
_clearZone p z = do
  liftIO $ Punctual.clear p z
  clearZoneError z
  setBaseNotation z ""
  

_preAnimationFrame :: Punctual -> R ()
_preAnimationFrame p = do
  s <- get
  res <- resolution
  b <- brightness
  liftIO $ Punctual.setResolution p res
  liftIO $ Punctual.setBrightness p b
  

_zoneAnimationFrame :: Punctual -> UTCTime -> Int -> R ()
_zoneAnimationFrame p tNow z = do
  t <- gets tempoCache
  liftIO $ Punctual.setTempo p t
  liftIO $ Punctual.render p True z tNow
  

_postAnimationFrame :: Punctual -> R ()
_postAnimationFrame p = liftIO $ Punctual.postRender p True

