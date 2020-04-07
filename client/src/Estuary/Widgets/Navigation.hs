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
import Estuary.Types.Tutorial
import Estuary.WebDirt.Foreign
import Estuary.Widgets.Ensemble
import Estuary.Widgets.EstuaryIcon
import Estuary.Widgets.Generic
import Estuary.Widgets.Text
import Estuary.Widgets.TransformedPattern
import Estuary.Widgets.View
import Estuary.Widgets.Editor
import Estuary.Widgets.Tutorial
import Estuary.Widgets.AboutEstuary
import Estuary.Widgets.CreateEnsemble
import Estuary.Tutorials.TidalCyclesBasics
import Estuary.Tutorials.Punctual

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
    gotoAboutEv <- panel "splash-margin" ctx About Term.About (text "A") --estuaryIcon
    gotoTutorialEv <- panel "splash-margin" ctx TutorialList Term.Tutorials (text "B") -- icon font: tutorial-icon.svg
    gotoSoloEv <- panel "splash-margin" ctx Solo Term.Solo (text "C") -- icon font: solo-icon.png
    gotoCollaborateEv <- panel "splash-margin" ctx Lobby Term.Collaborate (text "D") -- icon font: collaborate-icon.svg
    return $ leftmost [gotoAboutEv, gotoTutorialEv, gotoSoloEv, gotoCollaborateEv]
  leaveEnsemble <- (LeaveEnsemble <$) <$>  getPostBuild
  return (navEv, (leaveEnsemble, never, never))

page ctx _ wsDown TutorialList = do
  divClass "ui-font primary-color" $ text "Select a tutorial:"
  navTidalCyclesBasics <- liftM (TutorialNav "TidalCyclesBasics" <$) $ divClass "tutorialButton" $ button "TidalCycles"
  navPunctualTutorial <- liftM (TutorialNav "Punctual" <$) $ divClass "tutorialButton" $ button "Punctual"
  let nav = leftmost [navTidalCyclesBasics,navPunctualTutorial]
  leaveEnsemble <- (LeaveEnsemble <$) <$>  getPostBuild
  return (nav, (leaveEnsemble, never, never))

page ctx renderInfo wsDown (TutorialNav "TidalCyclesBasics") = do
  let ensResponses = fmap justEnsembleResponses wsDown
  (ensReq,hs) <- runEditor (runTutorial tidalCyclesBasics ensResponses) ctx renderInfo
  leaveEnsemble <- (LeaveEnsemble <$) <$>  getPostBuild
  return (never,(leaveEnsemble,ensReq,hs))

page ctx renderInfo wsDown (TutorialNav "Punctual") = do
  let ensResponses = fmap justEnsembleResponses wsDown
  (ensReq,hs) <- runEditor (runTutorial punctualTutorial ensResponses) ctx renderInfo
  leaveEnsemble <- (LeaveEnsemble <$) <$>  getPostBuild
  return (never,(leaveEnsemble,ensReq,hs))

page _ _ _ (TutorialNav _) = do
  text "Oops... a software error has occurred and we can't bring you to the tutorial you wanted! If you have a chance, please report this as an 'issue' on Estuary's github site"
  return (never,(never,never,never))

page ctx _ wsDown About = do
  aboutEstuary ctx
  leaveEnsemble <- (LeaveEnsemble <$) <$>  getPostBuild
  return (never, (leaveEnsemble, never, never))

page ctx _ wsDown Lobby = do
  -- process received ensemble lists into widgets that display info about, and let us join, ensembles
  ensembleList <- holdDyn [] $ fmapMaybe justEnsembleList wsDown
  ensembleClicked <- liftM (switchPromptlyDyn . fmap leftmost) $ simpleList ensembleList joinButton -- Event t Text
  let navToJoinEnsemble = fmap JoinEnsemblePage ensembleClicked
  navToCreateEnsemble <- liftM (CreateEnsemblePage <$) $ el "div" $ dynButton =<< translateDyn Term.CreateNewEnsemble ctx
  -- LeaveEnsemble is issued at build time, 0.25s after build and every 3s after that an updated EnsembleList is requested
  leaveEnsemble <- (LeaveEnsemble <$) <$>  getPostBuild
  now <- liftIO $ getCurrentTime
  requestEnsembleList <- liftM (GetEnsembleList <$) $ tickLossy (3::NominalDiffTime) (addUTCTime 0.25 now)
  let serverRequests = leftmost [leaveEnsemble,requestEnsembleList]
  return (leftmost [navToJoinEnsemble, navToCreateEnsemble], (requestEnsembleList, never, never))

page ctx _ rs CreateEnsemblePage = do
  (navigateAway,responses) <- createEnsembleWidget ctx rs
  return (Lobby <$ navigateAway,(responses,never,never))

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
    translateDyn Term.ParticipantPassword ctx >>= dynText
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
  leaveEnsemble <- (LeaveEnsemble <$) <$>  getPostBuild
  let serverRequests = leftmost [leaveEnsemble,joinRequest]
  return (navEvents, (serverRequests, never, never))

page ctx renderInfo rs (EnsemblePage ensembleName) = do
  let ensResponses = fmap justEnsembleResponses rs
  (ensReq,hs) <- runEditor (ensembleView ensResponses) ctx renderInfo
  return (never,(never,ensReq,hs))

page ctx renderInfo _ Solo = do
  (ensReq,hs) <- runEditor (ensembleView never) ctx renderInfo
  leaveEnsemble <- (LeaveEnsemble <$) <$>  getPostBuild
  return (never,(leaveEnsemble,ensReq,hs))


joinButton :: MonadWidget t m => Dynamic t Text -> m (Event t Text)
joinButton x = do
  b <- clickableDivClass'' x "joinButton ui-font ui-buttons other-borders" ()
  return $ tag (current x) b


panel :: MonadWidget t m => Text -> Dynamic t Context -> Navigation -> Term.Term -> m () -> m (Event t Navigation)
panel c ctx targetPage title icon = do
  liftM (targetPage <$) $ do
    divClass c $ do
      dynButtonWithChild "splash-panel" $ do
        divClass "splash-title" $ do
          dynText =<< translateDyn title ctx
        divClass "splash-icon-container" $ do
          divClass "splash-icon" icon
