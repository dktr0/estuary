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
import Control.Monad.Fix (MonadFix)

import Estuary.Widgets.Reflex
import Estuary.Widgets.W
import Estuary.Widgets.Router
import Estuary.Types.RenderInfo
import Estuary.Tidal.Types
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
import Estuary.Types.Tutorial
import Estuary.Widgets.Ensemble
import Estuary.Widgets.EstuaryIcon
import Estuary.Widgets.Text
import Estuary.Widgets.TransformedPattern
import Estuary.Widgets.View
import Estuary.Widgets.Tutorial
import Estuary.Widgets.AboutEstuary
import Estuary.Widgets.CreateEnsemble
import Estuary.Tutorials.TidalCyclesBasics
import Estuary.Tutorials.Punctual
import Estuary.Tutorials.CineCer0
import Estuary.Widgets.Announcements

data Navigation =
  Splash |
  About |
  TutorialList |
  TutorialNav Text |
  Solo |
  Lobby |
  CreateEnsemblePage |
  JoinEnsemblePage Text |
  EnsemblePage Text
  deriving (Generic, FromJSVal, ToJSVal)


navigation :: MonadWidget t m => Event t [Response] -> W t m (Event t Request, Event t EnsembleRequest)
navigation wsDown = do
  x <- router Splash never $ page wsDown
  let y = fmap snd x
  let a = switchDyn $ fmap fst y
  let b = switchDyn $ fmap snd y
  return (a,b)


page :: MonadWidget t m => Event t [Response] -> Navigation
  -> W t m (Event t Navigation, (Event t Request, Event t EnsembleRequest))

page wsDown Splash = do
  navEv <- splitPageWithAnnouncements $ divClass "splash-container" $ do
    gotoAboutEv <- panel "splash-margin" About Term.About (text "A") --estuaryIcon
    gotoTutorialEv <- panel "splash-margin" TutorialList Term.Tutorials (text "B") -- icon font: tutorial-icon.svg
    gotoSoloEv <- panel "splash-margin" Solo Term.Solo (text "C") -- icon font: solo-icon.png
    gotoCollaborateEv <- panel "splash-margin" Lobby Term.Collaborate (text "D") -- icon font: collaborate-icon.svg
    return $ leftmost [gotoAboutEv, gotoTutorialEv, gotoSoloEv, gotoCollaborateEv]
  leaveEnsemble <- (LeaveEnsemble <$) <$>  getPostBuild
  return (navEv, (leaveEnsemble, never))

page wsDown TutorialList = splitPageWithAnnouncements $ do
  divClass "ui-font primary-color" $ text "Select a tutorial:"
  navTidalCyclesBasics <- liftM (TutorialNav "TidalCyclesBasics" <$) $ divClass "tutorialButton" $ button "TidalCycles"
  navPunctualTutorial <- liftM (TutorialNav "Punctual" <$) $ divClass "tutorialButton" $ button "Punctual"
  navCineCer0 <- liftM (TutorialNav "CineCer0" <$) $ divClass "tutorialButton" $ button "CineCer0"
  let nav = leftmost [navTidalCyclesBasics,navPunctualTutorial,navCineCer0]
  leaveEnsemble <- (LeaveEnsemble <$) <$>  getPostBuild
  return (nav, (leaveEnsemble, never))

page wsDown (TutorialNav "TidalCyclesBasics") = do
  let ensResponses = fmapMaybe justEnsembleResponses wsDown
  ensReq <- runTutorial tidalCyclesBasics ensResponses
  leaveEnsemble <- (LeaveEnsemble <$) <$>  getPostBuild
  return (never,(leaveEnsemble,ensReq))

page wsDown (TutorialNav "Punctual") = do
  let ensResponses = fmapMaybe justEnsembleResponses wsDown
  ensReq <- runTutorial punctualTutorial ensResponses
  leaveEnsemble <- (LeaveEnsemble <$) <$>  getPostBuild
  return (never,(leaveEnsemble,ensReq))

page wsDown (TutorialNav "CineCer0") = do
  let ensResponses = fmapMaybe justEnsembleResponses wsDown
  ensReq <- runTutorial cineCer0Tutorial ensResponses
  leaveEnsemble <- (LeaveEnsemble <$) <$>  getPostBuild
  return (never,(leaveEnsemble,ensReq))

page _ (TutorialNav _) = do
  text "Oops... a software error has occurred and we can't bring you to the tutorial you wanted! If you have a chance, please report this as an 'issue' on Estuary's github site"
  return (never,(never,never))

page wsDown About = do
  aboutEstuary
  leaveEnsemble <- (LeaveEnsemble <$) <$>  getPostBuild
  return (never, (leaveEnsemble, never))

page wsDown Lobby = splitPageWithAnnouncements $ do
  -- process received ensemble lists into widgets that display info about, and let us join, ensembles
  ensembleList <- holdDyn [] $ fmapMaybe justEnsembleList wsDown
  ensembleClicked <- liftM (switchPromptlyDyn . fmap leftmost) $ simpleList ensembleList joinButton
  let navToJoinEnsemble = fmap JoinEnsemblePage ensembleClicked
  navToCreateEnsemble <- liftM (CreateEnsemblePage <$) $ el "div" $ term Term.CreateNewEnsemble >>= dynButton
  leaveEnsemble <- (LeaveEnsemble <$) <$>  getPostBuild
  now <- liftIO $ getCurrentTime
  requestEnsembleList <- liftM (GetEnsembleList <$) $ tickLossy (3::NominalDiffTime) (addUTCTime 0.25 now)
  let serverRequests = leftmost [leaveEnsemble,requestEnsembleList]
  return (leftmost [navToJoinEnsemble, navToCreateEnsemble], (requestEnsembleList, never))

page rs CreateEnsemblePage = splitPageWithAnnouncements $ do
  (navigateAway,responses) <- createEnsembleWidget rs
  return (Lobby <$ navigateAway,(responses,never))

page wsDown (JoinEnsemblePage ensembleName) = splitPageWithAnnouncements $ do
  el "div" $ do
    term Term.JoiningEnsemble >>= dynText
    text " "
    text ensembleName
  h <- el "div" $ do
    term Term.EnsembleUserName >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs
  l <- el "div" $ do
    term Term.EnsembleLocation >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs
  p <- el "div" $ do
    term Term.ParticipantPassword >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs & textInputConfig_inputType .~ "password"
  go <- el "div" $ dynButton =<< term Term.EnsembleLogin
  let joinRequest = tagPromptlyDyn (JoinEnsemble <$> constDyn ensembleName <*> h <*> l <*> p) go
  let joinedEnsembleResponses = fmapMaybe justJoinedEnsemble wsDown
  let joinedEnsembleNav = fmap (EnsemblePage . (\(x,_,_,_) -> x)) joinedEnsembleResponses
  let responseError = fmapMaybe justResponseError wsDown
  p <- el "div" $ do
    errorDisplay <- holdDyn "" responseError
    dynText errorDisplay
  cancel <- el "div" $ dynButton =<< term Term.Cancel
  let cancelNav = Lobby <$ cancel
  let navEvents = leftmost [joinedEnsembleNav,cancelNav]
  leaveEnsemble <- (LeaveEnsemble <$) <$>  getPostBuild
  let serverRequests = leftmost [leaveEnsemble,joinRequest]
  return (navEvents, (serverRequests, never))

page rs (EnsemblePage ensembleName) = do
  let ensResponses = fmapMaybe justEnsembleResponses rs
  ensReq <- ensembleView ensResponses
  return (never,(never,ensReq))

page rs Solo = do
  let ensResponses = fmapMaybe justEnsembleResponses rs
  ensReq <- ensembleView ensResponses
  return (never,(never,ensReq))


joinButton :: MonadWidget t m => Dynamic t Text -> m (Event t Text)
joinButton x = do
  b <- clickableDivClass'' x "joinButton ui-font ui-buttons other-borders" ()
  return $ tag (current x) b


panel :: (DomBuilder t m, Monad m, Reflex t, PostBuild t m, MonadHold t m, MonadFix m) => Text -> Navigation -> Term.Term -> W t m () -> W t m (Event t Navigation)
panel c targetPage title icon = divClass c $ do
  liftM (targetPage <$) $ dynButtonWithChild "splash-panel" $ do
    divClass "splash-title-container" $ divClass "splash-title" $ term title >>= dynText
    divClass "splash-icon-container" $ divClass "splash-icon" icon
