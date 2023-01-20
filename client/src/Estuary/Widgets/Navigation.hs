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


navigation :: MonadWidget t m => W t m ()
navigation = void $ router' Splash never page


page :: MonadWidget t m => Navigation -> W t m (Event t Navigation)

page Splash = do
  leaveEnsemble
  splitPageWithAnnouncements $ divClass "splash-container" $ do
    gotoAboutEv <- panel "splash-margin" About Term.About (text "A") --estuaryIcon
    gotoTutorialEv <- panel "splash-margin" TutorialList Term.Tutorials (text "B") -- icon font: tutorial-icon.svg
    gotoSoloEv <- panel "splash-margin" Solo Term.Solo (text "C") -- icon font: solo-icon.png
    gotoCollaborateEv <- panel "splash-margin" Lobby Term.Collaborate (text "D") -- icon font: collaborate-icon.svg
    return $ leftmost [gotoAboutEv, gotoTutorialEv, gotoSoloEv, gotoCollaborateEv]

page TutorialList = splitPageWithAnnouncements $ do
  leaveEnsemble
  divClass "ui-font primary-color" $ text "Select a tutorial:"
  navTidalCyclesBasics <- liftM (TutorialNav "TidalCyclesBasics" <$) $ divClass "tutorialButton" $ button "TidalCycles"
  navPunctualTutorial <- liftM (TutorialNav "Punctual" <$) $ divClass "tutorialButton" $ button "Punctual"
  navCineCer0 <- liftM (TutorialNav "CineCer0" <$) $ divClass "tutorialButton" $ button "CineCer0"
  return $ leftmost [navTidalCyclesBasics,navPunctualTutorial,navCineCer0]

page (TutorialNav "TidalCyclesBasics") = do
  leaveEnsemble
  runTutorial tidalCyclesBasics
  return never

page (TutorialNav "Punctual") = do
  leaveEnsemble
  runTutorial punctualTutorial
  return never

page (TutorialNav "CineCer0") = do
  leaveEnsemble
  runTutorial cineCer0Tutorial
  return never

page (TutorialNav _) = do
  text "Oops... a software error has occurred and we can't bring you to the tutorial you wanted! If you have a chance, please report this as an 'issue' on Estuary's github site"
  return never

page About = do
  leaveEnsemble
  aboutEstuary
  return never

page Lobby = splitPageWithAnnouncements $ do
  leaveEnsemble
  ensList <- ensembleList
  ensembleClicked <- liftM (switchPromptlyDyn . fmap leftmost) $ simpleList ensList joinButton
  let navToJoinEnsemble = fmap JoinEnsemblePage ensembleClicked
  navToCreateEnsemble <- liftM (CreateEnsemblePage <$) $ el "div" $ term Term.CreateNewEnsemble >>= dynButton
  now <- liftIO $ getCurrentTime
  every3s <- tickLossy (3::NominalDiffTime) (addUTCTime 0.25 now)
  request $ GetEnsembleList <$ every3s
  return $ leftmost [navToJoinEnsemble, navToCreateEnsemble]

page CreateEnsemblePage = splitPageWithAnnouncements $ do
  navigateAway <- createEnsembleWidget
  return $ Lobby <$ navigateAway

page (JoinEnsemblePage ensembleName) = splitPageWithAnnouncements $ do
  leaveEnsemble
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
  request $ tagPromptlyDyn (JoinEnsemble ensembleName <$> h <*> l <*> p) go
  p <- el "div" $ do
    err <- responseError
    dynText $ fmap (maybe "" id) err
  cancel <- el "div" $ dynButton =<< term Term.Cancel
  let cancelNav = Lobby <$ cancel
  je <- joinedEnsemble
  let joinedEnsembleNav = fmap EnsemblePage je
  return $ leftmost [cancelNav,joinedEnsembleNav]

page (EnsemblePage ensembleName) = ensembleView >> return never

page Solo = ensembleView >> return never


joinButton :: MonadWidget t m => Dynamic t Text -> m (Event t Text)
joinButton x = do
  b <- clickableDivClass'' x "joinButton ui-font ui-buttons other-borders" ()
  return $ tag (current x) b


panel :: (DomBuilder t m, Monad m, Reflex t, PostBuild t m, MonadHold t m, MonadFix m) => Text -> Navigation -> Term.Term -> W t m () -> W t m (Event t Navigation)
panel c targetPage title icon = divClass c $ do
  liftM (targetPage <$) $ dynButtonWithChild "splash-panel" $ do
    divClass "splash-title-container" $ divClass "splash-title" $ term title >>= dynText
    divClass "splash-icon-container" $ divClass "splash-icon" icon
