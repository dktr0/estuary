{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Main where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Protocol.JSON
import Estuary.Protocol.Foreign
import Estuary.Reflex.Utility
import Estuary.Widgets.Generic
import Estuary.Widgets.StackedPatterns
import Estuary.Widgets.PatternChain as P
import Estuary.Widgets.GeneralPattern as G -- for testing the Refactor of general container
import Estuary.Widgets.TransformedPattern
import Estuary.Widgets.Text
import Control.Monad (liftM)
import Sound.Tidal.Context (ParamPattern)
import Estuary.WebDirt.Foreign
import Estuary.WebDirt.Stream
import Estuary.Widgets.SpecificPattern
import Estuary.Widgets.WebDirt
import Data.Map
import Control.Monad.IO.Class (liftIO)
import Estuary.Widgets.WebSocket
import Text.JSON
import Data.Time
import Text.Read
import qualified GHCJS.Types as T


main :: IO ()
main = do
  wd <- webDirt
  stream <- webDirtStream wd
  protocol <- estuaryProtocol
  now <- Data.Time.getCurrentTime
  mainWidget $ estuaryWidget wd stream protocol now


estuaryWidget :: MonadWidget t m => T.JSVal -> WebDirtStream -> EstuaryProtocolObject -> UTCTime -> m ()
estuaryWidget wd stream protocol now = divClass "estuary" $ do
  muted <- header
  divClass "page" $ mdo
    (values,deltasUp,hints) <- mainPage deltasDown'
    values' <- mapDyn (toParamPattern . StackedPatterns . elems) values
    values'' <- combineDyn f values' muted
    let values''' = updated values''
    deltasDown <- webSocketWidget protocol now deltasUp
    let deltasDown' = ffilter (not . Prelude.null) deltasDown
    diagnostics values deltasUp deltasDown' hints
    performEvent_ $ fmap (liftIO . (doHint wd)) hints
    performEvent_ $ fmap (liftIO . stream) values'''
    where f x False = x
          f _ True = toParamPattern EmptyTransformedPattern


header :: (MonadWidget t m) => m (Dynamic t Bool)
header = divClass "header" $ do
  divClass "logo" $ text "estuary (based on TidalCycles and Reflex)"
  muted' <- divClass "webDirt" $ do
    muted <- divClass "webDirtMute" $ do
      text "WebDirt Mute"
      checkbox False $ def
    return $ _checkbox_value muted
  -- newPageIndex <- divClass "pageMenu" $ do
  --  let pageNames = Prelude.map (fst) pages
  --  let pageList = zipWith (\x y -> (y,x)) pageNames ([0..]::[Int])
  --  let pageMap = constDyn $ fromList pageList
  --  menu <- dropdown 0 pageMap def
  --  return $ _dropdown_change menu
  -- divClass "hintArea" $ text " "
  -- return newPageIndex
  return muted'

 
mainPage :: MonadWidget t m => Event t [EstuaryProtocol]
  -> m
    (Dynamic t (Map Int TransformedPattern), -- values for local use
     Event t EstuaryProtocol, -- edit events for broadcast
     Event t Hint) -- hint events for local use
mainPage deltasDown = do
  let deltaA = fmap ( (Prelude.filter isEstuaryEdit) . (Prelude.filter (matchesNumber 2)) ) deltasDown
  let deltaB = fmap ( (Prelude.filter isEstuaryEdit) . (Prelude.filter (matchesNumber 4)) ) deltasDown
  let deltaC = fmap ( (Prelude.filter isEstuaryEdit) . (Prelude.filter (matchesNumber 6)) ) deltasDown
  let deltaD = fmap ( (Prelude.filter isEstuaryEdit) . (Prelude.filter (matchesNumber 8)) ) deltasDown
  let deltaE = fmap ( (Prelude.filter isEstuaryEdit) . (Prelude.filter (matchesNumber 10)) ) deltasDown
  let deltaF = fmap ( (Prelude.filter isEstuaryEdit) . (Prelude.filter (matchesNumber 12)) ) deltasDown
  let deltaG = fmap ( (Prelude.filter isTextEdit) . (Prelude.filter (matchesNumber 14)) ) deltasDown
  let deltaH = fmap ( (Prelude.filter isEstuaryEdit) . (Prelude.filter (matchesNumber 16)) ) deltasDown
  let deltaA' = fmap justEstuaryCode $ fmapMaybe lastOrNothing deltaA
  let deltaB' = fmap justEstuaryCode $ fmapMaybe lastOrNothing deltaB
  let deltaC' = fmap justEstuaryCode $ fmapMaybe lastOrNothing deltaC
  let deltaD' = fmap justEstuaryCode $ fmapMaybe lastOrNothing deltaD
  let deltaE' = fmap justEstuaryCode $ fmapMaybe lastOrNothing deltaE
  let deltaF' = fmap justEstuaryCode $ fmapMaybe lastOrNothing deltaF
  let deltaG' = fmap justTextCode $ fmapMaybe lastOrNothing deltaG
  let deltaH' = fmap justEstuaryCode $ fmapMaybe lastOrNothing deltaH
  (aLabel,aValue,aEdits,aHints) <- divClass "eightL" $ do 
    a <- labelWidget 1 deltasDown
    (b,c,d) <- topLevelTransformedPatternWidget deltaA'
    return (a,b,c,d)
  (bLabel,bValue,bEdits,bHints) <- divClass "eightR" $ do
    a <- labelWidget 3 deltasDown
    (b,c,d) <- topLevelTransformedPatternWidget deltaB'
    return (a,b,c,d)
  (cLabel,cValue,cEdits,cHints) <- divClass "eightL" $ do
	a <- labelWidget 5 deltasDown
	(b,c,d) <- textPatternChainWidget deltaC'
	return (a,b,c,d)
  (dLabel,dValue,dEdits,dHints) <- divClass "eightR" $ do
	a <- labelWidget 7 deltasDown
	(b,c,d) <- textPatternChainWidget deltaD'
	return (a,b,c,d)
  (eLabel,eValue,eEdits,eHints) <- divClass "eightL" $ do
	a <- labelWidget 9 deltasDown
	(b,c,d) <- textPatternChainWidget deltaE'
	return (a,b,c,d)
  (fLabel,fValue,fEdits,fHints) <- divClass "eightR" $ do
	a <- labelWidget 11 deltasDown
	(b,c,d) <- textPatternChainWidget deltaF'
	return (a,b,c,d)
  (gLabel,gEdits,gEvals) <- divClass "eightL" $ do
	a <- labelWidget 13 deltasDown
	(b,c) <- textWidget deltaG'
	return (a,b,c)
  (hLabel,hValue,hEdits,hHints) <- divClass "eightR" $ do 
	a <- labelWidget 15 deltasDown
	(b,c,d) <- textPatternChainWidget deltaH'
	return (a,b,c,d)
  aValue' <- mapDyn (singleton 2) aValue
  bValue' <- mapDyn (singleton 4) bValue
  cValue' <- mapDyn (singleton 6) cValue
  dValue' <- mapDyn (singleton 8) dValue
  eValue' <- mapDyn (singleton 10) eValue
  fValue' <- mapDyn (singleton 12) fValue
  hValue' <- mapDyn (singleton 16) hValue
  valuesB <- combineDyn (union) aValue' bValue'
  valuesC <- combineDyn (union) valuesB cValue'
  valuesD <- combineDyn (union) valuesC dValue'
  valuesE <- combineDyn (union) valuesD eValue'
  valuesF <- combineDyn (union) valuesE fValue'
  values <- combineDyn (union) valuesF hValue'
  let aDeltaUp = fmap (EstuaryEdit "" 2) aEdits
  let bDeltaUp = fmap (EstuaryEdit "" 4) bEdits
  let cDeltaUp = fmap (EstuaryEdit "" 6) cEdits
  let dDeltaUp = fmap (EstuaryEdit "" 8) dEdits
  let eDeltaUp = fmap (EstuaryEdit "" 10) eEdits
  let fDeltaUp = fmap (EstuaryEdit "" 12) fEdits
  let gEditsUp = fmap (TextEdit "" 14) gEdits
  let gEvalsUp = fmap (TextEval "" 14) gEvals
  let gDeltaUp = leftmost [gEditsUp,gEvalsUp]
  let hDeltaUp = fmap (EstuaryEdit "" 16) hEdits
  let labelsUp = leftmost [aLabel,bLabel,cLabel,dLabel,eLabel,fLabel,gLabel,hLabel]
  let deltasUp = leftmost [aDeltaUp,bDeltaUp,cDeltaUp,dDeltaUp,eDeltaUp,fDeltaUp,gDeltaUp,hDeltaUp,labelsUp]
  let hints = leftmost [aHints,bHints,cHints,dHints,eHints,fHints,hHints]
  return (values,deltasUp,hints)


tempoWidget :: MonadWidget t m => Event t [EstuaryProtocol] -> m (Event t EstuaryProtocol)
tempoWidget deltas = do
  text "CPS:"
  let delta' = fmap (Prelude.filter isCps) deltas
  let delta'' = fmapMaybe lastOrNothing delta'
  let delta''' = fmapMaybe getCps delta''
  let delta'''' = fmap show delta'''
  t <- textInput $ def & textInputConfig_setValue .~ delta''''
  let t' = fmapMaybe (readMaybe) $ _textInput_input t
  let edits = fmap (TempoChange "") t'
  return edits


diagnostics :: MonadWidget t m =>
  Dynamic t (Map Int TransformedPattern) ->
  Event t EstuaryProtocol ->
  Event t [EstuaryProtocol] ->
  Event t Hint ->
  m ()
diagnostics values deltasUp deltasDown hints = do
  el "div" $ do
    text "Values:"
    mapDyn encode values >>= display
  el "div" $ do
    text "DeltasUp:"
    (holdDyn "" $ fmap encode deltasUp) >>= display
  el "div" $ do
    text "DeltasDown:"
    (holdDyn "" $ fmap encode deltasDown) >>= display
  el "div" $ do
    text "Hints:"
    (holdDyn "" $ fmap show hints) >>= display

