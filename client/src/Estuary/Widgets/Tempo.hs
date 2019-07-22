{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Tempo where

import Reflex
import Reflex.Dom
import Control.Monad.Trans
import Text.Read
import Data.Text
import Data.Time

import Sound.MusicW.AudioContext
import Data.Text (Text)
import qualified Data.Text as T
import TextShow

import Estuary.Types.Tempo
import Estuary.Types.Context
import Estuary.Types.EnsembleResponse
import Estuary.Widgets.Text
import Estuary.Render.AudioContext
import Estuary.Utility (lastOrNothing)
import Estuary.Reflex.Utility
import qualified Estuary.Types.Term as Term
import Estuary.Types.Language

tempoWidget :: MonadWidget t m => Dynamic t Context -> Event t [EnsembleResponse]
  -> m (Event t Tempo,Event t Tempo) -- (all tempo changes, just tempo edits)
tempoWidget ctx deltas = divClass "ensembleTempo ui-font primary-color" $ mdo
  iTempo <- tempo <$> (sample . current) ctx
  let deltas' = fmapMaybe (lastOrNothing . fmapMaybe justTempoChanges) deltas -- Event t (Tempo,UTCTime)
  tempoDelta <- performEvent $ fmap (liftIO . adjustTempoDelta) deltas'
  let initialText = showt (cps iTempo)
  (tValue,_,tEval) <- textWidget 1 initialText $ fmap (showt . cps) tempoDelta
  b <- dynButton =<< translateDyn Term.NewTempo ctx
  let cpsEvent = fmapMaybe ((readMaybe :: String -> Maybe Double) . T.unpack) $ tagPromptlyDyn tValue $ leftmost [b,tEval]
  tempoEdit <- performEvent $ fmap liftIO $ attachPromptlyDynWith adjustTempoEdit currentTempo cpsEvent
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
