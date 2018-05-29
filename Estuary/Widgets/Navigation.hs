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
import Data.Time.Clock

import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Hint
import Estuary.Types.View
import Estuary.Types.Definition
import Estuary.Types.Terminal

import Estuary.Widgets.View


data Navigation =
  Splash |
  TutorialList |
  Tutorial String |
  Solo |
  Lobby |
  CreateEnsemblePage |
  Collaborate String


navigation :: MonadWidget t m => UTCTime -> Event t Command -> Event t [ServerResponse] ->
  m (Dynamic t [TransformedPattern],Event t ServerRequest,Event t Hint)
navigation now commands wsDown = mdo
  let initialPage = page commands wsDown now Splash
  let rebuild = fmap (page commands wsDown now) navEvents
  w <- widgetHold initialPage rebuild
  values <- liftM joinDyn $ mapDyn (\(x,_,_,_)->x) w
  wsUp <- liftM switchPromptlyDyn $ mapDyn (\(_,x,_,_)->x) w
  hints <- liftM switchPromptlyDyn $ mapDyn (\(_,_,x,_)->x) w
  navEvents <- liftM switchPromptlyDyn $ mapDyn (\(_,_,_,x)->x) w
  return (values,wsUp,hints)

page :: MonadWidget t m => Event t Command -> Event t [ServerResponse] -> UTCTime -> Navigation ->
  m (Dynamic t [TransformedPattern],Event t ServerRequest,Event t Hint,Event t Navigation)

page _ wsDown _ Splash = do
  x <- liftM (TutorialList <$) $ el "div" $ button "Tutoriales"
  y <- liftM (Solo <$)  $ el "div" $ button "Solo"
  z <- liftM (Lobby <$)  $ el "div" $ button "Colaborar"
  let navEvents = leftmost [x,y,z]
  return (constDyn [],never,never,navEvents)

page _ wsDown _ TutorialList = do
  el "div" $ text "Click on a button to select a tutorial interface:"
  t1 <- liftM (Tutorial "Structure editing" <$) $ el "div" $ button "Structure editing"
  t2 <- liftM (Tutorial "TidalCycles text editing" <$) $ el "div" $ button "TidalCycles text editing"
  back <- liftM (Splash <$) $ button  "<----"
  let navEvents = leftmost [t1,t2,back]
  return (constDyn [],never,never,navEvents)

page _ wsDown _ (Tutorial "Structure editing") = do
  text "Tutorial placeholder"
  x <- liftM (Splash <$) $ button  "<----"
  return (constDyn [],never,never,x)

page _ wsDown _ (Tutorial "TidalCycles text editing") = do
  text "Tutorial placeholder"
  x <- liftM (Splash <$) $ button "<----"
  return (constDyn [],never,never,x)

page _ wsDown _ (Tutorial _) = do
  text "Oops... a software error has occurred and we can't bring you to the tutorial you wanted! If you have a chance, please report this as a bug on Estuary's github site"
  x <- liftM (Splash <$) $ button "<----"
  return (constDyn [],never,never,x)

page _ wsDown _ Solo = do
  (defMap,hints) <- viewInSoloWidget standardView
  patterns <- mapDyn (justStructures . elems) defMap
  x <- liftM (Splash <$) $ button "<----"
  return (patterns,never,hints,x)

page _ wsDown _ Lobby = do
  requestEnsembleList <- liftM (GetEnsembleList <$) getPostBuild
  spaceList <- holdDyn [] $ fmapMaybe justEnsembleList wsDown
  join <- simpleList spaceList joinButton -- m (Dynamic t [Event t Navigation])
  join' <- mapDyn leftmost join -- m (Dynamic t (Event t Navigation))
  let join'' = switchPromptlyDyn join' -- Event t Navigation
  create <- liftM (CreateEnsemblePage <$) $ el "div" $ button "Crear nuevo ensamble"
  back <- liftM (Splash <$) $ el "div" $ button "<----"
  return (constDyn [],requestEnsembleList,never,leftmost [back,join'',create])

page _ _ _ CreateEnsemblePage = do
  el "div" $ text "Crear un nuevo ensamble"
  el "div" $ text "Nota: para crear un enamble escribe la contraseña de administrador"
  adminPwd <- el "div" $ do
    text "Contraseña del admin: "
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs & textInputConfig_inputType .~ "password"
  name <- el "div" $ do
    text "Nombre del ensamble: "
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs
  password <- el "div" $ do
    text "Contraseña del ensamble: "
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_inputType .~ "password" & textInputConfig_attributes .~ attrs
  nameAndPassword <- combineDyn (,) name password
  confirm <- el "div" $ button "Confirmar"
  let createEnsemble = fmap (\(a,b) -> CreateEnsemble a b) $ tagDyn nameAndPassword confirm
  let authenticateAdmin = fmap Authenticate $ updated adminPwd
  cancel <- el "div" $ button "Cancelar"
  let serverRequests = leftmost [createEnsemble,authenticateAdmin]
  let navEvents = fmap (const Lobby) $ leftmost [cancel,() <$ createEnsemble]
  return (constDyn [], serverRequests, never, navEvents)

page commands wsDown now (Collaborate w) = do
  (defMap,wsUp,hints) <- viewInEnsembleWidget w now commands wsDown
  patterns <- mapDyn (justStructures . elems) defMap
  x <- liftM (Lobby <$) $ button  "<----"
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
