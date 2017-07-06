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
  let deltasDown' = fmap (justActionsInSpace "placeholder") deltasDown
  (aLabel,aValue,aEdits,aHints) <- divClass "eightTopL" $ do
    a <- labelWidget 1 deltasDown'
    (b,c,d) <- topLevelTransformedPatternWidget 2 deltasDown'
    return (a,b,c,d)
  (bLabel,bValue,bEdits,bHints) <- divClass "eightTopR" $ do
    a <- labelWidget 3 deltasDown'
    (b,c,d) <- topLevelTransformedPatternWidget 4 deltasDown'
    return (a,b,c,d)
  (cLabel,cValue,cEdits,cHints) <- divClass "eightMiddleL" $ do
        a <- labelWidget 5 deltasDown'
        (b,c,d) <- textPatternChainWidget 6 deltasDown'
        return (a,b,c,d)
  (dLabel,dValue,dEdits,dHints) <- divClass "eightMiddleR" $ do
        a <- labelWidget 7 deltasDown'
        (b,c,d) <- textPatternChainWidget 8 deltasDown'
        return (a,b,c,d)
  (eLabel,eValue,eEdits,eHints) <- divClass "eightBottomL" $ do
        a <- labelWidget 9 deltasDown'
        (b,c,d) <- textPatternChainWidget 10 deltasDown'
        return (a,b,c,d)
  (fLabel,fValue,fEdits,fHints) <- divClass "eightBottomR" $ do
        a <- labelWidget 11 deltasDown'
        (b,c,d) <- topLevelTransformedPatternWidget 12 deltasDown'
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
  let labelsUp = leftmost [aLabel,bLabel,cLabel,dLabel,eLabel,fLabel]
  let deltasUp = leftmost [aEdits,bEdits,cEdits,dEdits,eEdits,fEdits]
  let deltasUp' = fmap (SpaceRequest  . InSpace "placeholder") deltasUp
  let hints = leftmost [aHints,bHints,cHints,dHints,eHints,fHints]
  return (values,deltasUp',hints)


{-
tempoWidget :: MonadWidget t m => Event t [ServerResponse] -> m (Event t ServerRequest)
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
-}

diagnostics :: MonadWidget t m =>
  Dynamic t (Map Int TransformedPattern) ->
  Event t ServerRequest ->
  Event t [ServerResponse] ->
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
