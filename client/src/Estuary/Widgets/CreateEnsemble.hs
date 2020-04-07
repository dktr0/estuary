{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.CreateEnsemble where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Data.Map

import Estuary.Types.Context
import Estuary.Types.Response
import Estuary.Types.Request
import Estuary.Widgets.Generic
import Estuary.Reflex.Utility
import qualified Estuary.Types.Term as Term

createEnsembleWidget :: MonadWidget t m => Dynamic t Context -> Event t [Response]
  -> m (Event t (), Event t Request)
createEnsembleWidget ctx rs = do

  el "div" $ dynText =<< translateDyn Term.CreateNewEnsemble ctx

  el "div" $ text "First, choose a name for your ensemble. Ensemble names must not contain spaces, tabs, or newlines."

  ename <- el "div" $ do
    translateDyn Term.EnsembleName ctx >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs

  el "div" $ text "Choose a host password for your ensemble. The host password can be used to delete the ensemble at any time. You may or may not want to share this password with the other members of a group, depending on what you are doing. You usually do not need to share this password with the other members of a group."

  hpwd <- el "div" $ do
    translateDyn Term.HostPassword ctx >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs

  el "div" $ text "Choose a participant password for your ensemble. This password is needed by anyone who will collaborate in the ensemble. It is common to share this password with everyone involved with a given ensemble. (Note: This password is never required to join the ensemble as an observer. If someone joins an ensemble without entering the participant password they will still see/hear everything that happens in the ensemble, and will even be able to change code themselves, however nothing that they do will be shared with others through the server.)"

  ppwd <- el "div" $ do
    translateDyn Term.ParticipantPassword ctx >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs

  el "div" $ text "Choose an expiry time for the ensemble (or just leave it at the default choice). The expiry time is how long the server waits to delete the ensemble after the most recent action within the ensemble (for example, editing some code, publishing a view, sending a chat message). Expiry times greater than 1 hour (the default) require that the 'community' password be entered below."

  exptime <- el "div" $ do
    translateDyn Term.EnsembleExpiry ctx >>= dynText
    let expMap = fromList [
                   (Nothing,"No expiry"),
                   (Just 3600,"1 hr"),
                   (Just 86400,"1 day"),
                   (Just 604800,"1 week"),
                   (Just 1209600,"2 weeks"),
                   (Just 2678400,"31 days")
                   ]
    _dropdown_value <$> dropdown (Just 3600) (constDyn expMap) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font"))

  el "div" $ text "The community password (below) is required only when ensembles have an expiry time of greater than 1 hour. If not creating an ensemble with a longer expiry time, you can leave this blank."

  cpwd <- el "div" $ do
    translateDyn Term.CommunityPassword ctx >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs & textInputConfig_inputType .~ "password"

  confirm <- el "div" $ dynButton =<< translateDyn Term.Confirm ctx

  divClass "" $ do -- display of errors from server in response to ensemble creation requests
    x <- holdDyn "" $ fmapMaybe justResponseError rs
    dynText x

  cancel <- el "div" $ dynButton =<< translateDyn Term.Cancel ctx

  let draftRequest = CreateEnsemble <$> cpwd <*> ename <*> hpwd <*> ppwd <*> exptime
  let createEnsemble = tagPromptlyDyn draftRequest confirm
  leaveEnsemble <- (LeaveEnsemble <$) <$> getPostBuild
  let serverRequests = leftmost [createEnsemble,leaveEnsemble]
  let ensembleCreated = () <$ fmapMaybe justResponseOK rs
  let navigateAway = leftmost [cancel,() <$ ensembleCreated]
  return (navigateAway, serverRequests)
