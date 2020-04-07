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

  ename <- divClass "" $ do
    translateDyn Term.EnsembleName ctx >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs

  hpwd <- divClass "" $ do
    translateDyn Term.HostPassword ctx >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs

  ppwd <- divClass "" $ do
    translateDyn Term.ParticipantPassword ctx >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs

  exptime <- divClass "" $ do
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

  cpwd <- el "div" $ do
    translateDyn Term.CommunityPassword ctx >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs & textInputConfig_inputType .~ "password"

  confirm <- el "div" $ dynButton =<< translateDyn Term.Confirm ctx

  cancel <- el "div" $ dynButton =<< translateDyn Term.Cancel ctx

  let draftRequest = CreateEnsemble <$> cpwd <*> ename <*> hpwd <*> ppwd <*> exptime
  let createEnsemble = tagPromptlyDyn draftRequest confirm
  leaveEnsemble <- (LeaveEnsemble <$) <$> getPostBuild
  let serverRequests = leftmost [createEnsemble,leaveEnsemble]
  let navigateAway = leftmost [cancel,() <$ createEnsemble]
  return (navigateAway, serverRequests)
