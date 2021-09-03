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
import Estuary.Widgets.Reflex
import qualified Estuary.Types.Term as Term
import Estuary.Types.Language
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Widgets.W

tempoWidget :: MonadWidget t m => Dynamic t Tempo -> W t m (Event t Tempo)
tempoWidget tempoDyn = do
  v <- variableWidget tempoDyn $ \a eventA -> divClass "ensembleTempo ui-font primary-color" $ mdo
    let initialText = showt (freq a)
    let updatedText = fmap (showt . freq) eventA
    (tValue,_,tEval) <- textWidget 1 (constDyn False) initialText updatedText
    b <- dynButton =<< term Term.NewTempo
    let evalEvent = tagPromptlyDyn tValue $ leftmost [b,tEval]
    let cpsEvent = fmapMaybe ((readMaybe :: String -> Maybe Rational) . T.unpack) evalEvent
    edits <- performEvent $ fmap liftIO $ attachPromptlyDynWith (flip changeTempoNow) tempoDyn cpsEvent -- *** attachPromptlyDynWith here might not be right!!!
    return edits
  return $ localEdits v

-- context :: Monad m => W t m (Dynamic t Context)

visualiseTempoWidget:: MonadWidget t m => Dynamic t Tempo -> W t m (Variable t Tempo)
visualiseTempoWidget delta = divClass "tempoVisualiser" $  mdo
  c <- context
  let currentTempo = fmap (tempo . ensemble . ensembleC) c
  widgetBuildTime <- liftIO $ getCurrentTime
  tick <- tickLossy 0.01 widgetBuildTime
  elapsedBeatsRunning <- performEvent $ attachWith getElapsedBeats (current currentTempo) $ fmap _tickInfo_lastUTC tick 

  dynTempo <- holdDyn 0 elapsedBeatsRunning

  out <- visualiseTempo dynTempo

  v <- variable delta never
  return v

visualiseTempo :: MonadWidget t m => Dynamic t Rational -> W t m ()
visualiseTempo delta = do
  let class' = constDyn $ "class" =: "human-to-human-comm code-font"
  let style = constDyn $ "style" =: "height: auto;"
  let attrs = mconcat [class',style]
  elDynAttr "stopwatch" attrs $ dynText $ fmap (showt) $ fmap (showt) delta

   elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do

    -- create circular dial
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" attrsCircle $ return () 
    -- manecilla
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "line" attrsLine $ return () 

  return ()

createLines

-- <svg class="cyclesVisualiser" id="manecillas">
-- 	<line x1="0" y1="0" x2="0" y2="-1" stroke="#2e3436" stroke-width="0.0078125px" transform="rotate(  360)"/>
-- </svg>


getElapsedBeats :: MonadIO m => Tempo -> UTCTime -> m Rational
getElapsedBeats t now = do
  let x = timeToCount t now 
-- here is where I draw the thing

  return x 

-- tempoMetre:: Tempo -> Rational -> Tempo
-- tempoMetre
