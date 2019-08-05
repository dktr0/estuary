{-# LANGUAGE RecursiveDo, OverloadedStrings, ScopedTypeVariables, DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Estuary.Widgets.Navigation (Navigation(..),navigation) where

import Control.Monad
import Data.IntMap.Strict
import Data.Maybe
import qualified Data.Map as Map
import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import GHCJS.Marshal
import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Text.Read
import Control.Monad.IO.Class

import Estuary.Reflex.Router
import Estuary.Reflex.Utility
import Estuary.Types.RenderInfo
import Estuary.Tidal.Types
import Estuary.Tutorials.Context
import qualified Estuary.Tutorials.Tutorial as T
import Estuary.Types.Context
import Estuary.Types.Definition
import Estuary.Types.EnsembleC
import Estuary.Types.Hint
import Estuary.Types.Language
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.EnsembleRequest
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
import Estuary.Widgets.Editor

data Navigation =
  Splash |
  About |
  TutorialList |
  Tutorial T.TutorialId |
  Solo |
  Lobby |
  CreateEnsemblePage |
  JoinEnsemblePage Text |
  EnsemblePage Text
  deriving (Generic, FromJSVal, ToJSVal)


navigation :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo -> Event t [Response] -> m (Event t Request, Event t EnsembleRequest, Event t [Hint])
navigation ctx renderInfo wsDown = do
  dynPage <- router Splash never $ page ctx renderInfo wsDown
  let dynPageData = fmap snd dynPage
  let requestsUp = switchPromptlyDyn $ fmap (\(x,_,_) -> x) dynPageData
  let ensembleRequestsUp = switchPromptlyDyn $ fmap (\(_,x,_) -> x) dynPageData
  let hintsUp = switchPromptlyDyn $ fmap (\(_,_,x) -> x) dynPageData
  return (requestsUp,ensembleRequestsUp,hintsUp)


page :: forall t m. (MonadWidget t m)
  => Dynamic t Context -> Dynamic t RenderInfo -> Event t [Response] -> Navigation
  -> m (Event t Navigation, (Event t Request, Event t EnsembleRequest, Event t [Hint]))

page ctx _ wsDown Splash = do
  navEv <- divClass "splash-container" $ do
    gotoAboutEv <- panel ctx About Term.About estuaryIcon
    gotoTutorialEv <- panel ctx TutorialList Term.Tutorials (text "B") -- icon font: tutorial-icon.svg
    gotoSoloEv <- panel ctx Solo Term.Solo (text "C") -- icon font: solo-icon.png
    gotoCollaborateEv <- panel ctx Lobby Term.Collaborate (text "D") -- icon font: collaborate-icon.svg
    return $ leftmost [gotoAboutEv, gotoTutorialEv, gotoSoloEv, gotoCollaborateEv]
  return (navEv, (never, never, never))

page ctx _ wsDown TutorialList = do
  divClass "ui-font primary-color" $ text "Click on a button to select a tutorial interface:"
  bs <- sequence $ fmap (\b-> liftM ((Tutorial $ T.tutorialId b) <$) $ buttonWithClass $ (T.pack . show) $ T.tutorialId b) (tutorials::[T.Tutorial t m])
  return (leftmost bs, (never, never, never))

page ctx _ wsDown (Tutorial tid) = do
  let widget = (Map.lookup tid tutorialMap) :: Maybe (Dynamic t Context -> m (Dynamic t DefinitionMap, Event t [Hint]))
  (dm, hs) <- maybe errMsg id (fmap (\x-> x ctx) widget)
  return (never, (never, never, hs)) -- *** RENDERING IS THUS BROKEN IN TUTORIALS, need to make sure tutorials return edits ***
  where
    errMsg = do
      text "Oops... a software error has occurred and we can't bring you to the tutorial you wanted! If you have a chance, please report this as a bug on Estuary's github site"
      return (constDyn empty, never)

page ctx _ wsDown About = do
  aboutEstuaryParagraph ctx
  return (never, (never, never, never))

page ctx _ wsDown Lobby = do
  -- at widget build and every 3 seconds thereafter, request the list of available ensembles
  requestEnsembleList0 <- liftM (GetEnsembleList <$) getPostBuild
  now <- liftIO $ getCurrentTime
  requestEnsembleList1 <- liftM (GetEnsembleList <$) $ tickLossy (3::NominalDiffTime) now
  let requestEnsembleList = leftmost [requestEnsembleList0,requestEnsembleList1]
  -- process received ensemble lists into widgets that display info about, and let us join, ensembles
  ensembleList <- holdDyn [] $ fmapMaybe justEnsembleList wsDown
  ensembleClicked <- liftM (switchPromptlyDyn . fmap leftmost) $ simpleList ensembleList joinButton -- Event t Text
  let navToJoinEnsemble = fmap JoinEnsemblePage ensembleClicked
  navToCreateEnsemble <- liftM (CreateEnsemblePage <$) $ el "div" $ dynButton =<< translateDyn Term.CreateNewEnsemble ctx
  return (leftmost [navToJoinEnsemble, navToCreateEnsemble], (requestEnsembleList, never, never))

page ctx _ _ CreateEnsemblePage = do
  el "div" $ dynText =<< translateDyn Term.CreateNewEnsemble ctx
  el "div" $ dynText =<< translateDyn Term.CreateNewEnsembleNote ctx
  adminPwd <- el "div" $ do
    translateDyn Term.AdministratorPassword ctx >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs & textInputConfig_inputType .~ "password"
  name <- el "div" $ do
    translateDyn Term.EnsembleName ctx >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs
  password <- el "div" $ do
    translateDyn Term.EnsemblePassword ctx >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_inputType .~ "password" & textInputConfig_attributes .~ attrs
  let nameAndPassword = (,) <$> name <*> password
  confirm <- el "div" $ dynButton =<< translateDyn Term.Confirm ctx
  let createEnsemble = fmap (\(a,b) -> CreateEnsemble a b) $ tagPromptlyDyn nameAndPassword confirm
  let authenticateAdmin = fmap Authenticate $ updated adminPwd
  cancel <- el "div" $ dynButton =<< translateDyn Term.Cancel ctx
  let serverRequests = leftmost [createEnsemble,authenticateAdmin]
  let navEvents = fmap (const Lobby) $ leftmost [cancel,() <$ createEnsemble]
  return (navEvents, (serverRequests, never, never))

page ctx _ wsDown (JoinEnsemblePage ensembleName) = do
  el "div" $ do
    prefix <- translateDyn Term.JoiningEnsemble ctx
    dynText $ fmap (<> (" " <> ensembleName)) prefix
  h <- el "div" $ do
    translateDyn Term.EnsembleUserName ctx >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs
  l <- el "div" $ do
    translateDyn Term.EnsembleLocation ctx >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs
  p <- el "div" $ do
    translateDyn Term.EnsemblePassword ctx >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs & textInputConfig_inputType .~ "password"
  go <- el "div" $ dynButton =<< translateDyn Term.EnsembleLogin ctx
  let joinRequest = tagPromptlyDyn (JoinEnsemble <$> constDyn ensembleName <*> h <*> l <*> p) go
  let joinedEnsembleResponses = fmapMaybe justJoinedEnsemble wsDown
  let joinedEnsembleNav = fmap (EnsemblePage . fst) joinedEnsembleResponses
  let responseError = fmapMaybe justResponseError wsDown
  p <- el "div" $ do
    errorDisplay <- holdDyn "" responseError
    dynText errorDisplay
  cancel <- el "div" $ dynButton =<< translateDyn Term.Cancel ctx
  let cancelNav = Lobby <$ cancel
  let navEvents = leftmost [joinedEnsembleNav,cancelNav]
  return (navEvents, (joinRequest, never, never))

page ctx renderInfo rs (EnsemblePage ensembleName) = do
  let ensResponses = fmap justEnsembleResponses rs
  (ensReq,hs) <- runEditor (ensembleView ensResponses) ctx renderInfo
  return (never,(never,ensReq,hs))

page ctx renderInfo _ Solo = do
  (ensReq,hs) <- runEditor (ensembleView never) ctx renderInfo
  return (never,(never,ensReq,hs))


joinButton :: MonadWidget t m => Dynamic t Text -> m (Event t Text)
joinButton x = do
  b <- clickableDivClass'' x "ui-font primary-color" ()
  return $ tag (current x) b


panel :: MonadWidget t m => Dynamic t Context -> Navigation -> Term.Term -> m () -> m (Event t Navigation)
panel ctx targetPage title icon = do
  liftM (targetPage <$) $ do
    divClass "splash-margin" $ do
      dynButtonWithChild "splash-panel" $ do
        divClass "splash-title" $ do
          dynText =<< translateDyn title ctx
        divClass "splash-icon-container" $ do
          divClass "splash-icon" icon


aboutEstuaryParagraph :: MonadWidget t m => Dynamic t Context -> m ()
aboutEstuaryParagraph ctx = divClass "aboutEstuaryParagraph ui-font background" $ do
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
