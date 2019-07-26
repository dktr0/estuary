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
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Widgets.EstuaryWidget
import Estuary.Types.Variable

tempoWidget :: MonadWidget t m => EstuaryWidget t m (Event t Tempo)
tempoWidget = do
  ctx <- askContext
  let tempoDyn = fmap (tempo . ensemble . ensembleC) ctx
  v <- reflexVariable tempoDyn $ \a eventA -> divClass "ensembleTempo ui-font primary-color" $ mdo
    let initialText = showt (cps a)
    let updatedText = fmap (showt . cps) eventA
    (tValue,_,tEval) <- textWidget 1 initialText updatedText
    b <- dynButton =<< translateDyn Term.NewTempo ctx
    let evalEvent = tagPromptlyDyn tValue $ leftmost [b,tEval]
    let cpsEvent = fmapMaybe ((readMaybe :: String -> Maybe Rational) . T.unpack) evalEvent
    performEvent $ fmap liftIO $ attachPromptlyDynWith adjustTempoEdit tempoDyn cpsEvent -- *** attachPromptlyDynWith here might not be right!!!
  return $ localEdits v

adjustTempoEdit :: Tempo -> Rational -> IO Tempo
adjustTempoEdit oldTempo newCps = do
  now <- liftAudioIO $ audioUTCTime
  return $ adjustCps newCps oldTempo now
