{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Tempo where

import Reflex
import Reflex.Dom
import Control.Monad.Trans
import Text.Read
import Data.Text
import Data.Time

import Sound.MusicW.AudioContext

import Estuary.Types.Tempo
import Estuary.Types.Context
import Estuary.Types.EnsembleResponse
import Estuary.Widgets.Text
import Estuary.Render.AudioContext
import Estuary.Utility (lastOrNothing)

tempoWidget :: MonadWidget t m => Dynamic t Context -> Event t [EnsembleResponse]
  -> m (Event t Tempo,Event t Tempo) -- (all tempo changes, just tempo edits)
tempoWidget ctx deltas = divClass "ensembleTempo" $ mdo
  iTempo <- tempo <$> (sample . current) ctx
  let deltas' = fmapMaybe (lastOrNothing . fmapMaybe justTempoChanges) deltas -- Event t (Tempo,UTCTime)
  tempoDelta <- performEvent $ fmap (liftIO . adjustTempoDelta) deltas'
  let initialText = show (cps iTempo)
  (tValue,_,tEval) <- textAreaWidgetForPatternChain 1 initialText $ fmap (show . cps) tempoDelta
  b <- button "set new tempo" -- *** needs to be localized
  let cpsEvent = fmapMaybe (readMaybe :: String -> Maybe Double) $ tagDyn tValue $ leftmost [b,tEval]
  tempoEdit <- performEvent $ fmap liftIO $ attachDynWith adjustTempoEdit currentTempo cpsEvent
  currentTempo <- holdDyn iTempo $ leftmost [tempoDelta,tempoEdit]
  return (updated currentTempo,tempoEdit)

justTempoChanges :: EnsembleResponse -> Maybe (Tempo,UTCTime)
justTempoChanges (NewTempo theTempo theTime) = Just (theTempo,theTime)
justTempoChanges _ = Nothing

adjustTempoDelta :: (Tempo,UTCTime) -> IO Tempo
adjustTempoDelta (newTempo,timeStamp) = do
  now <- liftAudioIO $ audioUTCTime
  return $ Tempo {
    cps = cps newTempo,
    at = addUTCTime (-0.075) now, -- TODO: later use Cristian's algorithm, for now assume server sent 75 msec ago
    beat = elapsedCycles newTempo timeStamp
    }

adjustTempoEdit :: Tempo -> Double -> IO Tempo
adjustTempoEdit oldTempo newCps = do
  now <- liftAudioIO $ audioUTCTime
  return $ adjustCps newCps oldTempo now
