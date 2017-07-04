{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Navigation where

import Reflex
import Reflex.Dom
import Estuary.Protocol.JSON
import Estuary.WebDirt.Foreign
import Estuary.Tidal.Types
import Estuary.Widgets.Text
import Estuary.Widgets.TransformedPattern
import Control.Monad (liftM)
import Data.Map
import Text.Read
import Text.JSON


data Navigation =
  Splash |
  TutorialList |
  Tutorial |
  Solo |
  Lobby |
  Collaborate


navigation :: MonadWidget t m => Event t [ServerResponse] ->
  m (Dynamic t [TransformedPattern],Event t ServerRequest,Event t Hint)
navigation wsDown = mdo
  let initialPage = page wsDown Splash
  let rebuild = fmap (page wsDown) navEvents
  w <- widgetHold initialPage rebuild
  values <- liftM joinDyn $ mapDyn (\(x,_,_,_)->x) w
  wsUp <- liftM switchPromptlyDyn $ mapDyn (\(_,x,_,_)->x) w
  hints <- liftM switchPromptlyDyn $ mapDyn (\(_,_,x,_)->x) w
  navEvents <- liftM switchPromptlyDyn $ mapDyn (\(_,_,_,x)->x) w
  return (values,wsUp,hints)

page :: MonadWidget t m => Event t [ServerResponse] -> Navigation ->
  m (Dynamic t [TransformedPattern],Event t ServerRequest,Event t Hint,Event t Navigation)

page wsDown Splash = do
  x <- liftM (TutorialList <$)  $ button "Tutorials"
  y <- liftM (Solo <$)  $ button "Solo"
  z <- liftM (Lobby <$)  $ button "Collaborate"
  let navEvents = leftmost [x,y,z]
  return (constDyn [],never,never,navEvents)

page wsDown TutorialList = do
  text "TutorialList placeholder"
  x <- liftM (Splash <$) $ button "back to splash"
  return (constDyn [],never,never,x)

page wsDown Tutorial = do
  text "Tutorial placeholder"
  x <- liftM (Splash <$) $ button "back to splash"
  return (constDyn [],never,never,x)

page wsDown Solo = do
  text "Solo placeholder"
  x <- liftM (Splash <$) $ button "back to splash"
  return (constDyn [],never,never,x)

page wsDown Lobby = do
  text "Lobby placeholder"
  y <- liftM (Collaborate <$) $ button "collaborate"
  x <- liftM (Splash <$) $ button "back to splash"
  return (constDyn [],never,never,leftmost [x,y])

page wsDown Collaborate = do
  (patternMap,wsUp,hints) <- mainPage wsDown
  patterns <- mapDyn elems patternMap
  x <- liftM (Lobby <$) $ button "back to lobby"
  return (patterns,wsUp,hints,x)


-- stuff below this line here temporarily during testing/refactoring

mainPage :: MonadWidget t m => Event t [ServerResponse]
  -> m
    (Dynamic t (Map Int TransformedPattern), -- values for local use
     Event t ServerRequest, -- edit events for broadcast
     Event t Hint) -- hint events for local use
mainPage deltasDown = do
  let deltasDown' = fmapMaybe (justActionsInSpace "placeholder") deltasDown
  let deltaA = fmap ( (Prelude.filter isEstuaryEdit) . (Prelude.filter (matchesNumber 2)) ) deltasDown
  let deltaB = fmap ( (Prelude.filter isEstuaryEdit) . (Prelude.filter (matchesNumber 4)) ) deltasDown
  let deltaC = fmap ( (Prelude.filter isEstuaryEdit) . (Prelude.filter (matchesNumber 6)) ) deltasDown
  let deltaD = fmap ( (Prelude.filter isEstuaryEdit) . (Prelude.filter (matchesNumber 8)) ) deltasDown
  let deltaE = fmap ( (Prelude.filter isEstuaryEdit) . (Prelude.filter (matchesNumber 10)) ) deltasDown
  let deltaF = fmap ( (Prelude.filter isEstuaryEdit) . (Prelude.filter (matchesNumber 12)) ) deltasDown
  let deltaG = fmap ( (Prelude.filter isEstuaryEdit) . (Prelude.filter (matchesNumber 14)) ) deltasDown
  let deltaH = fmap ( (Prelude.filter isTextEdit) . (Prelude.filter (matchesNumber 16)) ) deltasDown
  let deltaA' = fmap justEstuaryCode $ fmapMaybe lastOrNothing deltaA
  let deltaB' = fmap justEstuaryCode $ fmapMaybe lastOrNothing deltaB
  let deltaC' = fmap justEstuaryCode $ fmapMaybe lastOrNothing deltaC
  let deltaD' = fmap justEstuaryCode $ fmapMaybe lastOrNothing deltaD
  let deltaE' = fmap justEstuaryCode $ fmapMaybe lastOrNothing deltaE
  let deltaF' = fmap justEstuaryCode $ fmapMaybe lastOrNothing deltaF
  let deltaG' = fmap justEstuaryCode $ fmapMaybe lastOrNothing deltaG
  let deltaH' = fmap justTextCode $ fmapMaybe lastOrNothing deltaH
  (aLabel,aValue,aEdits,aHints) <- divClass "eightTopL" $ do
    a <- labelWidget 1 deltasDown
    (b,c,d) <- topLevelTransformedPatternWidget deltaA'
    return (a,b,c,d)
  (bLabel,bValue,bEdits,bHints) <- divClass "eightTopR" $ do
    a <- labelWidget 3 deltasDown
    (b,c,d) <- topLevelTransformedPatternWidget deltaB'
    return (a,b,c,d)
  (cLabel,cValue,cEdits,cHints) <- divClass "eightMiddleL" $ do
        a <- labelWidget 5 deltasDown
        (b,c,d) <- textPatternChainWidget deltaC'
        return (a,b,c,d)
  (dLabel,dValue,dEdits,dHints) <- divClass "eightMiddleR" $ do
        a <- labelWidget 7 deltasDown
        (b,c,d) <- textPatternChainWidget deltaD'
        return (a,b,c,d)
  (eLabel,eValue,eEdits,eHints) <- divClass "eightBottomL" $ do
        a <- labelWidget 9 deltasDown
        (b,c,d) <- textPatternChainWidget deltaE'
        return (a,b,c,d)
  (fLabel,fValue,fEdits,fHints) <- divClass "eightBottomR" $ do
        a <- labelWidget 1 deltasDown
        (b,c,d) <- topLevelTransformedPatternWidget deltaA'
        return (a,b,c,d)
  aValue' <- mapDyn (singleton 2) aValue
  bValue' <- mapDyn (singleton 4) bValue
  cValue' <- mapDyn (singleton 6) cValue
  dValue' <- mapDyn (singleton 8) dValue
  eValue' <- mapDyn (singleton 10) eValue
  fValue' <- mapDyn (singleton 12) fValue
  valuesB <- combineDyn (union) aValue' bValue'
  valuesC <- combineDyn (union) valuesB cValue'
  valuesD <- combineDyn (union) valuesC dValue'
  valuesE <- combineDyn (union) valuesD eValue'
  values <- combineDyn (union) valuesE fValue'
  let aDeltaUp = fmap (Edit 2 . Structure) aEdits
  let bDeltaUp = fmap (Edit 4 . Structure) bEdits
  let cDeltaUp = fmap (Edit 6 . Structure) cEdits
  let dDeltaUp = fmap (Edit 8 . Structure) dEdits
  let eDeltaUp = fmap (Edit 10 . Structure) eEdits
  let fDeltaUp = fmap (Edit 12 . Structure) fEdits
  let aLabel' = fmap (Edit 1 . LabelText) aLabel
  let bLabel' = fmap (Edit 3 . LabelText) bLabel
  let cLabel' = fmap (Edit 5 . LabelText) cLabel
  let dLabel' = fmap (Edit 7 . LabelText) dLabel
  let eLabel' = fmap (Edit 9 . LabelText) eLabel
  let fLabel' = fmap (Edit 11 . LabelText) fLabel
  let labelsUp = leftmost [aLabel,bLabel,cLabel,dLabel,eLabel,fLabel]
  let deltasUp = leftmost [aDeltaUp,bDeltaUp,cDeltaUp,dDeltaUp,eDeltaUp,fDeltaUp,labelsUp]
  let deltasUp' = fmap (SpaceRequest . InSpace "placeholder") deltasUp
  let hints = leftmost [aHints,bHints,cHints,dHints,eHints,fHints]
  return (values,deltasUp',hints)


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
