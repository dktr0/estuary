{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Navigation where

import Reflex
import Reflex.Dom
import Estuary.WebDirt.Foreign
import Estuary.Tidal.Types
import Estuary.Widgets.Generic
import Estuary.Widgets.Text
import Estuary.Widgets.TransformedPattern
import Control.Monad (liftM)
import Data.Map
import Text.Read
import Text.JSON

import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Hint
import Estuary.Types.View
import Estuary.Types.Definition

import Estuary.Widgets.View


data Navigation =
  Splash |
  TutorialList |
  Tutorial String |
  Solo |
  Lobby |
  CreateEnsemblePage |
  Collaborate String


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
  x <- liftM (TutorialList <$) $ el "div" $ button "Tutorials"
  y <- liftM (Solo <$)  $ el "div" $ button "Solo"
  z <- liftM (Lobby <$)  $ el "div" $ button "Collaborate"
  let navEvents = leftmost [x,y,z]
  return (constDyn [],never,never,navEvents)

page wsDown TutorialList = do
  el "div" $ text "Click on a button to select a tutorial interface:"
  t1 <- liftM (Tutorial "Structure editing" <$) $ el "div" $ button "Structure editing"
  t2 <- liftM (Tutorial "TidalCycles text editing" <$) $ el "div" $ button "TidalCycles text editing"  
  back <- liftM (Splash <$) $ button "Return to splashscreen"
  let navEvents = leftmost [t1,t2,back]
  return (constDyn [],never,never,navEvents)

page wsDown (Tutorial "Structure editing") = do
  text "Tutorial placeholder"
  x <- liftM (Splash <$) $ button "back to splash"
  return (constDyn [],never,never,x)

page wsDown (Tutorial "TidalCycles text editing") = do
  text "Tutorial placeholder"
  x <- liftM (Splash <$) $ button "back to splash"
  return (constDyn [],never,never,x)

page wsDown (Tutorial _) = do
  text "Oops... a software error has occurred and we can't bring you to the tutorial you wanted! If you have a chance, please report this as a bug on Estuary's github site"
  x <- liftM (Splash <$) $ button "back to splash"
  return (constDyn [],never,never,x)

--viewInSoloWidget :: MonadWidget t m => View -> m (Dynamic t DefinitionMap, Event t Hint)

page wsDown Solo = do
  (defMap,hints) <- viewInSoloWidget defaultView 
  patterns <- mapDyn (justStructures . elems) defMap
  x <- liftM (Splash <$) $ button "Return to splashscreen"
  return (patterns,never,hints,x)

page wsDown Lobby = do
  requestEnsembleList <- liftM (GetEnsembleList <$) getPostBuild
  spaceList <- holdDyn [] $ fmapMaybe justEnsembleList wsDown
  join <- simpleList spaceList joinButton -- m (Dynamic t [Event t Navigation])
  join' <- mapDyn leftmost join -- m (Dynamic t (Event t Navigation))
  let join'' = switchPromptlyDyn join' -- Event t Navigation
  create <- liftM (CreateEnsemblePage <$) $ el "div" $ button "Create New Ensemble"
  back <- liftM (Splash <$) $ el "div" $ button "back to splash"
  return (constDyn [],requestEnsembleList,never,leftmost [back,join'',create])


page wsDown CreateEnsemblePage = do
  el "div" $ text "Create A New Ensemble"
  el "div" $ text "Note: To successfully create an ensemble you need to know and enter the correct admin password."
  name <- el "div" $ do
    text "Ensemble Name: "
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs
  password <- el "div" $ do
    text "Ensemble Password: "
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_inputType .~ "password" & textInputConfig_attributes .~ attrs
  nameAndPassword <- combineDyn (,) name password
  confirm <- el "div" $ button "Confirm"
  let createEnsemble = fmap (\(a,b) -> CreateEnsemble a b) $ tagDyn nameAndPassword confirm
  cancel <- el "div" $ button "Cancel"
  let navEvents = fmap (const Lobby) $ leftmost [cancel,() <$ createEnsemble]
  return (constDyn [], createEnsemble, never, navEvents)

page wsDown (Collaborate w) = do
  (defMap,wsUp,hints) <- viewInEnsembleWidget w defaultView wsDown
  patterns <- mapDyn (justStructures . elems) defMap
  x <- liftM (Lobby <$) $ button "back to lobby"
  return (patterns,wsUp,hints,x)


joinButton :: MonadWidget t m => Dynamic t String -> m (Event t Navigation)
joinButton x = do
  b <- clickableDivClass'' x "placeholderClass" ()
  return $ Collaborate <$> tagDyn x b

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
