{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.Tempo where

import Reflex
import Reflex.Dom
import Control.Monad.Trans
import Text.Read

import Estuary.Types.Tempo
import Estuary.Types.Context
import Estuary.Widgets.Text

tempoWidget :: MonadWidget t m => Dynamic t Context -> Tempo -> Event t Tempo -> m (Event t Tempo)
tempoWidget ctx i delta = divClass "ensembleTempo" $ mdo
  let initialText = show (cps i)
  (tValue,_,tEval) <- textAreaWidgetForPatternChain 1 initialText $ fmap (show . cps) delta
  b <- button "set new tempo" -- *** needs to be localized
  let cpsEvent = fmapMaybe (readMaybe :: String -> Maybe Double) $ tagDyn tValue $ leftmost [b,tEval]
  tempoEdit <- performEvent $ fmap (liftIO . uncurry adjustCpsNow) $ attachDyn currentTempo cpsEvent
  currentTempo <- holdDyn i $ leftmost [delta,tempoEdit]
  return tempoEdit
