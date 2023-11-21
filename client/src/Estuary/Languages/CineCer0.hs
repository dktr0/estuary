{-# LANGUAGE ScopedTypeVariables #-}

module Estuary.Languages.CineCer0 (cineCer0) where

import Data.Time
import Control.Monad.Except
import Data.IntMap as IntMap
import Data.Text as T
import Control.Exception
import Control.Monad.State.Strict
import Sound.MusicW.AudioContext
import Control.Monad.Reader

import qualified Estuary.Languages.CineCer0.CineCer0State as CineCer0
import qualified Estuary.Languages.CineCer0.Spec as CineCer0
import qualified Estuary.Languages.CineCer0.Parser as CineCer0

import Estuary.Render.R
import Estuary.Types.RenderInfo
import Estuary.Types.TextNotation
import Estuary.Render.MainBus
import Estuary.Types.Tempo
import Estuary.Types.Ensemble
import Estuary.Types.EnsembleC


cineCer0 :: TextNotationRenderer
cineCer0 = emptyTextNotationRenderer {
  parseZone = _parseZone,
  clearZone' = _clearZone,
  preAnimationFrame = return (),
  zoneAnimationFrame = _zoneAnimationFrame,
  postAnimationFrame = return ()
  }


_parseZone :: Int -> Text -> UTCTime -> R ()
_parseZone z x eTime = do
  let parseResult :: Either String CineCer0.Spec = CineCer0.cineCer0 eTime $ T.unpack x -- Either String CineCer0Spec
  case parseResult of
    Right spec -> do
      clearZoneError z
      setBaseNotation z CineCer0
      setEvaluationTime z eTime
      modify' $ \xx -> xx { cineCer0Specs = insert z spec $ cineCer0Specs xx }
    Left err -> setZoneError z (T.pack err)


_clearZone :: Int -> R ()
_clearZone z = do
  s <- get
  case (IntMap.lookup z $ cineCer0States s) of
    Just x -> liftIO $ CineCer0.deleteCineCer0State x
    Nothing -> return ()
  modify' $ \x -> x {
    cineCer0Specs = IntMap.delete z $ cineCer0Specs x,
    cineCer0States = IntMap.delete z $ cineCer0States x
    }


_zoneAnimationFrame :: UTCTime -> Int -> R ()
_zoneAnimationFrame tNow z = do
  s <- get
  case videoDivCache s of
    Nothing -> return ()
    Just theDiv -> do
      let spec = IntMap.findWithDefault (CineCer0.emptySpec $ renderStart s) z (cineCer0Specs s)
      let prevState = IntMap.findWithDefault (CineCer0.emptyCineCer0State theDiv) z $ cineCer0States s
      newState <- liftIO $ CineCer0.updateCineCer0State (tempoCache s) tNow spec prevState
      modify' $ \x -> x { cineCer0States = insert z newState (cineCer0States s) }
