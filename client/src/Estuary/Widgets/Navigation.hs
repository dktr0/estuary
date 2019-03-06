{-# LANGUAGE RecursiveDo, OverloadedStrings, ScopedTypeVariables, DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Estuary.Widgets.Navigation where

import Control.Monad (liftM)

import Data.IntMap.Strict
import Data.Maybe
import qualified Data.Map as Map
import Data.Time.Clock
import qualified Data.Text as T

import Estuary.Reflex.Router
import Estuary.Reflex.Utility
import Estuary.RenderInfo
import Estuary.Tidal.Types
import Estuary.Tutorials.Context
import qualified Estuary.Tutorials.Tutorial as T
import Estuary.Types.Context
import Estuary.Types.Definition
import Estuary.Types.EnsembleState(soloEnsembleName)
import Estuary.Types.Hint
import Estuary.Types.Language
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Tempo
import qualified Estuary.Types.Term as Term
import Estuary.Types.Terminal
import Estuary.Types.View
import Estuary.WebDirt.Foreign
import Estuary.Widgets.Ensemble
import Estuary.Widgets.EstuaryIcon
import Estuary.Widgets.Generic
import Estuary.Widgets.Text
import Estuary.Widgets.TransformedPattern
import Estuary.Widgets.View

import GHC.Generics

import GHCJS.Marshal

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)

import Text.JSON
import Text.Read

data Navigation =
  Splash |
  About |
  TutorialList |
  Tutorial T.TutorialId |
  Solo |
  Lobby |
  CreateEnsemblePage |
  Collaborate String
  deriving (Generic, FromJSVal, ToJSVal)

navigation :: MonadWidget t m => Navigation -> Event t Navigation -> Dynamic t Context -> Dynamic t RenderInfo -> Event t Command -> Event t [Response] -> m (Dynamic t (Maybe (String, DefinitionMap)), Event t Request, Event t Hint, Event t Tempo)
navigation initialPage stateChangeEv ctx renderInfo commands wsDown = do
  dynPage <- router initialPage stateChangeEv $ page ctx renderInfo commands wsDown

  dynPageData <- mapDyn snd dynPage
  dynValues <- liftM joinDyn           $ mapDyn (\(x, _, _, _) -> x) dynPageData
  wsUpEv    <- liftM switchPromptlyDyn $ mapDyn (\(_, x, _, _) -> x) dynPageData
  hintEv    <- liftM switchPromptlyDyn $ mapDyn (\(_, _, x, _) -> x) dynPageData
  tempoEv   <- liftM switchPromptlyDyn $ mapDyn (\(_, _, _, x) -> x) dynPageData

  return (dynValues, wsUpEv, hintEv, tempoEv)

page :: forall t m. (MonadWidget t m)
  => Dynamic t Context -> Dynamic t RenderInfo -> Event t Command -> Event t [Response] -> Navigation
  -> m (Event t Navigation, (Dynamic t (Maybe (String, DefinitionMap)), Event t Request, Event t Hint, Event t Tempo))

page ctx _ _ wsDown Splash = do
  navEv <- divClass "splash-container" $ do


    gotoAboutEv <- liftM (About <$) $ do
      divClass "splash-margin" $ do
        dynButtonWithChild "splash-panel" $ do
          divClass "splash-title" $ do
            dynText =<< translateDyn Term.About ctx
          divClass "splash-icon-container" $ do
            divClass "splash-icon" $ do
              estuaryIcon

    gotoTutorialEv <- liftM (TutorialList <$) $ do
      divClass "splash-margin" $ do
        dynButtonWithChild "splash-panel" $ do
          divClass "splash-title" $ do
            dynText =<< translateDyn Term.Tutorials ctx
          divClass "splash-icon-container" $ do
            divClass  "splash-icon" $ text "B"
            -- elAttr "img" (Map.fromList [("src", "tutorial-icon.svg"),  ("class", "splash-icon")]) blank

    gotoSoloEv <- liftM (Solo <$) $ do
      divClass "splash-margin" $ do
        dynButtonWithChild "splash-panel" $ do
          divClass "splash-title" $ do
            dynText =<< translateDyn Term.Solo ctx
          divClass "splash-icon-container" $ do
            divClass  "splash-icon" $ text "C"
            -- elAttr "img" (Map.fromList [("src", "solo-icon.png"), ("class", "splash-icon")]) blank

    gotoCollaborateEv <- liftM (Lobby <$) $ do
      divClass "splash-margin" $ do
        dynButtonWithChild "splash-panel" $ do
          divClass "splash-title" $ do
            dynText =<< translateDyn Term.Collaborate ctx
          divClass "splash-icon-container" $ do
            divClass  "splash-icon" $ text "D"
            -- elAttr "img" (Map.fromList [("src", "collaborate-icon.svg"), ("class", "splash-icon")]) blank

    return $ leftmost [gotoAboutEv, gotoTutorialEv, gotoSoloEv, gotoCollaborateEv]
  return (navEv, (constDyn Nothing, never, never, never))

page ctx _ _ wsDown TutorialList = do
  el "div" $ text "Click on a button to select a tutorial interface:"
  bs <- sequence $ fmap (\b-> liftM ((Tutorial $ T.tutorialId b) <$) $ button $ (T.pack . show) $ T.tutorialId b) (tutorials::[T.Tutorial t m])
  return (leftmost bs, (constDyn Nothing, never, never, never))

page ctx _ _ wsDown (Tutorial tid) = do
  let widget = (Map.lookup tid tutorialMap)::Maybe (Dynamic t Context -> m (Dynamic t DefinitionMap, Event t Hint))
  (dm, hint) <- maybe errMsg id (fmap (\x-> x ctx) widget)
  dm' <- mapDyn (\x-> Just ("",x)) dm
  return (never, (dm', never, hint, never))
  where
    errMsg = do
      text "Oops... a software error has occurred and we can't bring you to the tutorial you wanted! If you have a chance, please report this as a bug on Estuary's github site"
      return (constDyn empty, never)

page ctx _ _ wsDown About = do
  aboutEstuaryParagraph ctx
  return (never, (constDyn $ Just (soloEnsembleName, empty), never, never, never))

page ctx renderInfo commands wsDown Solo = do
  (values,hints,tempoEvents) <- soloView ctx renderInfo commands
  values' <- mapDyn (\x -> Just (soloEnsembleName, x)) values
  return (never, (values', never, hints, tempoEvents))

page ctx _ _ wsDown Lobby = do
  requestEnsembleList <- liftM (GetEnsembleList <$) getPostBuild
  spaceList <- holdDyn [] $ fmapMaybe justEnsembleList wsDown
  spaceList' <- mapDyn (fmap T.pack) spaceList
  join <- simpleList spaceList' joinButton -- m (Dynamic t [Event t Navigation])
  join' <- mapDyn leftmost join -- m (Dynamic t (Event t Navigation))
  let join'' = switchPromptlyDyn join' -- Event t Navigation
  create <- liftM (CreateEnsemblePage <$) $ el "div" $ dynButton =<< translateDyn Term.CreateNewEnsemble ctx
  return (leftmost [join'', create], (constDyn Nothing, requestEnsembleList, never, never))

page ctx _ _ _ CreateEnsemblePage = do
  el "div" $ dynText =<< translateDyn Term.CreateNewEnsemble ctx
  el "div" $ dynText =<< translateDyn Term.CreateNewEnsembleNote ctx
  adminPwd <- el "div" $ do
    translateDyn Term.AdministratorPassword ctx >>= dynText
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs & textInputConfig_inputType .~ "password"
  name <- el "div" $ do
    translateDyn Term.EnsembleName ctx >>= dynText
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs
  password <- el "div" $ do
    translateDyn Term.EnsemblePassword ctx >>= dynText
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_inputType .~ "password" & textInputConfig_attributes .~ attrs
  nameAndPassword <- combineDyn (,) name password
  confirm <- el "div" $ dynButton =<< translateDyn Term.Confirm ctx
  let createEnsemble = fmap (\(a,b) -> CreateEnsemble (T.unpack a) (T.unpack b)) $ tagDyn nameAndPassword confirm
  let authenticateAdmin = fmap (Authenticate . T.unpack) $ updated adminPwd
  cancel <- el "div" $ dynButton =<< translateDyn Term.Cancel ctx
  let serverRequests = leftmost [createEnsemble,authenticateAdmin]
  let navEvents = fmap (const Lobby) $ leftmost [cancel,() <$ createEnsemble]
  return (navEvents, (constDyn $ Just (soloEnsembleName, empty), serverRequests, never, never))

page ctx renderInfo commands wsDown (Collaborate ensembleName) = do
  (values,wsUp,hints,tempoEvents) <- ensembleView ctx renderInfo ensembleName commands wsDown
  values' <- mapDyn (\x -> Just (ensembleName, x)) values
  return (never, (values', wsUp, hints, tempoEvents))


joinButton :: MonadWidget t m => Dynamic t T.Text -> m (Event t Navigation)
joinButton x = do
  b <- clickableDivClass'' x "placeholderClass" ()
  return $ (Collaborate . T.unpack) <$> tagDyn x b

aboutEstuaryParagraph :: MonadWidget t m => Dynamic t Context -> m ()
aboutEstuaryParagraph ctx = divClass "aboutEstuaryParagraph" $ do
  dynText =<< translationList ctx [
    (English,"Estuary is a platform for collaboration and learning through live coding. It enables you to create sound, music, and visuals in a web browser. Key features include:"),
    (Español,"Estuary es una plataforma de colaboración y aprendizaje a través del la codificación en vivo (live coding). Estuary le permite crear sonidos, música y visuales en el explorador de internet. Algunas características importantes de esta plataforma son:")
    ]
  el "ul" $ do
    el "li" $ dynText =<< translationList ctx [
      (English,"built-in tutorials and reference materials"),
      (Español,"tutoriales y materiales de referencia")
      ]
    el "li" $ dynText =<< translationList ctx [
      (English,"a growing collection of different interfaces and live coding languages"),
      (Español,"una creciente colección de diferentes interfaces y lenguajes de codificación en vivo.")
      ]
    el "li" $ dynText =<< translationList ctx [
      (English,"support for networked ensembles (whether in the same room or distributed around the world)"),
      (Español,"soporte para ensambles en red (ya sea que esten en la misma sala o distribuidos en todo el mundo)")
      ]
    el "li" $ dynText =<< translationList ctx [
      (English,"text localization to an expanding set of natural languages"),
      (Español,"localización de texto a un conjunto creciente de lenguajes naturales.")
      ]
    el "li" $ dynText =<< translationList ctx [
      (English,"visual customization via themes (described by CSS)"),
      (Español,"personalización visual a través de temas (descritos por CSS).")
      ]
  dynText =<< translationList ctx [
    (English,"The development of Estuary is the result of ongoing collaborative work that has been \
    \supported by two grants from Canada's Social Sciences and Humanities Research Council (SSHRC) - \
    \initially for the project \"Projectional interfaces for musical live coding\", and more recently \
    \as part of the project \"Platforms  and  practices  for networked, language-neutral live coding\". \ \Estuary builds upon, and depends on, the work of many others, including but not limited to all \
    \those who contribute to Reflex and TidalCycles. Estuary is free and open source software, released \ \ under the terms of the GNU Public License (version 3)."),
    (Español,"El desarrollo de Estuary es el resultado del trabajo colaborativo que se ha realizado \
    \apoyado por dos becas del Consejo de Investigación de Ciencias Sociales y Humanidades de Canadá (SSHRC) -\
    \inicialmente para el proyecto \"Interfaces proyectivas para la codificación musical en vivo\", y más recientemente \
    \como parte del proyecto \"Plataformas y prácticas para la codificación en vivo en red y en idioma neutral\". Estuary se construye desde del trabajo de muchos otres, incluyendo pero no limitado a todes \
    \aquellos que contribuyen a Reflex y TidalCycles. Estuary es un software gratuito y de código abierto, publicado \ \ bajo los términos de la Licencia Pública GNU (versión 3).")
    ]
