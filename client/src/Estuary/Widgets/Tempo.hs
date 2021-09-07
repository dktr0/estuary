{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Tempo where

import Reflex
import Reflex.Dom
import Control.Monad.Trans
import Text.Read
import Data.Text
import Data.Time
import Data.Map

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

visualiseCycles :: MonadWidget t m => Dynamic t Rational -> W t m ()
visualiseCycles delta = do
  let class' = constDyn $ "class" =: "human-to-human-comm code-font"
  let style = constDyn $ "style" =: "height: auto;"
  let vB = constDyn $ "viewBox" =: "-1.5 -1.5 3 3"
  let w' = constDyn $ "width" =: "100"
  let h' = constDyn $ "height" =: "100"
  let attrs = mconcat [class',w',h',style,vB]

  let (cx,cy) = (constDyn $ "cx" =: "0", constDyn $ "cy" =: "0")
  let r = constDyn $ "r" =: "1"
  let stroke = constDyn $ "stroke" =: "white"
  let strokeWidth = constDyn $ "stroke-width" =: "0.05"
  let attrsCircle = mconcat [cx,cy,r,stroke,strokeWidth]

  let (x1,x2) = (constDyn $ "x1" =: "0",constDyn $ "x2" =: "0")
  let (y1,y2) = (constDyn $ "y1" =: "0",constDyn $ "y2" =: "-1")
  let transform = beatToRotation <$> delta 
  let attrsLine = mconcat [x1,y1,x2,y2,stroke,strokeWidth,transform]

  let attrsLine' = mconcat [x1,y1,x2,y2,stroke,strokeWidth]

 -- elDynAttr "stopwatch" attrs $ dynText $ fmap (showt) $ fmap (showt) delta
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    -- create circular dial
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" attrsCircle $ return () 
    -- manecilla
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "line" attrsLine $ return ()
    -- mark
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "line" attrsLine' $ return () 
  return ()


beatToRotation:: Rational -> Map Text Text
beatToRotation r = "transform" =: ("rotate(" <> (showt radio) <> ")")
  where radio = fromIntegral (round $ radio' * 360) :: Double
        radio' = r - (realToFrac $ floor r)

-- <svg viewBox="-6 -6 12 12" class="dial" id="foobar">
-- 	<circle cx="0" cy="0" r="1" stroke="green" stroke-width="0.1" fill="yellow" />
-- 	<line x1="0" y1="0" x2="0" y2="-1" stroke="#2e3436" stroke-width="0.0078125px" transform="rotate(  360)"/>
-- </svg>


getElapsedBeats :: MonadIO m => Tempo -> UTCTime -> m Rational
getElapsedBeats t now = do
  let x = timeToCount t now 
-- here is where I draw the thing

  return x 

-- tempoMetre:: Tempo -> Rational -> Tempo
-- tempoMetre
