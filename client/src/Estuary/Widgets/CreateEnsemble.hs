{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.CreateEnsemble where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Data.Map
import qualified Data.Map.Strict as Map


import Estuary.Types.Context
import Estuary.Types.Response
import Estuary.Types.Request
import Estuary.Widgets.Reflex
import Estuary.Widgets.Reflex
import qualified Estuary.Types.Term as Term
import Estuary.Widgets.W
import Estuary.Types.Language


createEnsembleWidget :: MonadWidget t m => Event t [Response]
  -> W t m (Event t (), Event t Request)
createEnsembleWidget rs = el "div" $ do

  el "div" $ term Term.CreateNewEnsemble >>= dynText

  el "div" $ dynText =<< (translatableText (Map.fromList [(English,"First, choose a name for your ensemble. Ensemble names must not contain spaces, tabs, or newlines."), (Español, "Primero, elija un nombre para su ensamble. Los nombres de los ensambles no deben contener espacios, tabulaciones o  líneas vacías.")]))

  ename <- el "div" $ do
    term Term.EnsembleName >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs

  el "div" $ dynText =<< (translatableText (Map.fromList [(English,"Choose a host password for your ensemble. The host password can be used to delete the ensemble at any time. You may or may not want to share this password with the other members of a group, depending on what you are doing. You usually do not need to share this password with the other members of a group."), (Español, "Elija una contraseña de host para su ensamble. La contraseña de host se puede utilizar para eliminar el ensamble en cualquier momento. Es posible que desee o no compartir esta contraseña con lxs demás miembrxs de un grupo, según lo que esté haciendo. Por lo general, no es necesario compartir esta contraseña con lxs demás miembrxs de un grupo.")]))

  hpwd <- el "div" $ do
    term Term.HostPassword >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs

  el "div" $ dynText =<< (translatableText (Map.fromList [(English,"Choose a participant password for your ensemble. This password is needed by anyone who will collaborate in the ensemble. It is common to share this password with everyone involved with a given ensemble. (Note: This password is never required to join the ensemble as an observer. If someone joins an ensemble without entering the participant password they will still see/hear everything that happens in the ensemble, and will even be able to change code themselves, however nothing that they do will be shared with others through the server.)"), (Español, "Elijana contraseña para lxs participantes de su ensamble. Esta contraseña es necesaria para cualquier persona que colabore en el ensamble. Es común compartir esta contraseña con todas las personas involucradas en un ensamble determinado. (Nota: esta contraseña nunca es necesaria para unirse al ensamble como observador. Si alguien se une a un ensamble sin ingresar la contraseña de participante, aún verá y escuchará todo lo que sucede en el ensamble, e incluso podrá cambiar el código mismo, sin embargo, nada de lo que haga se compartirá con otrxs a través del servidor.)")]))


  ppwd <- el "div" $ do
    term Term.ParticipantPassword >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs

  el "div" $ dynText =<< (translatableText (Map.fromList [(English,"Choose an expiry time for the ensemble (or just leave it at the default choice). The expiry time is how long the server waits to delete the ensemble after the most recent action within the ensemble (for example, editing some code, publishing a view, sending a chat message). Expiry times greater than 1 hour (the default) require that the 'community' password be entered below."), (Español, "Elija un tiempo de expiración para el ensamble (o simplemente déjelo en la opción predeterminada). El tiempo de expiración es cuánto tiempo espera el servidor para eliminar el ensamble después de la acción más reciente dentro del conjunto (por ejemplo, editar un código, publicar un layout, enviar un mensaje de chat). Los tiempos de vencimiento superiores a 1 hora (el valor predeterminado) requieren que la contraseña de la 'comunidad' se ingrese a continuación.")]))


  exptime <- el "div" $ do
    term Term.EnsembleExpiry >>= dynText
    let expMap = fromList [
                   (Just 3600, "1 hr"),
                   (Just 86400,"1 day"),
                   (Just 604800,"1 week"),
                   (Just 2678400,"1 month (31 days)")
                   ]
    _dropdown_value <$> dropdown (Just 3600) (constDyn expMap) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font"))

  el "div" $ dynText =<< (translatableText (Map.fromList [(English,"The community password (below) is required only when ensembles have an expiry time of greater than 1 hour. If not creating an ensemble with a longer expiry time, you can leave this blank."), (Español, "La contraseña de la comunidad (a continuación) es necesaria solo cuando los ensambles tienen un tiempo de caducidad superior a 1 hora. Si no necesita crear un ensamble con un tiempo de expiración más largo, puede dejar este espacio en blanco.")]))

  cpwd <- el "div" $ do
    term Term.CommunityPassword >>= dynText
    let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs & textInputConfig_inputType .~ "password"

  confirm <- el "div" $ term Term.Confirm >>= dynButton

  divClass "" $ do -- display of errors from server in response to ensemble creation requests
    x <- holdDyn "" $ fmapMaybe justResponseError rs
    dynText x

  cancel <- el "div" $ term Term.Cancel >>= dynButton

  let draftRequest = CreateEnsemble <$> cpwd <*> ename <*> hpwd <*> ppwd <*> exptime
  let createEnsemble = tagPromptlyDyn draftRequest confirm
  leaveEnsemble <- (LeaveEnsemble <$) <$> getPostBuild
  let serverRequests = leftmost [createEnsemble,leaveEnsemble]
  let ensembleCreated = () <$ fmapMaybe justResponseOK rs
  let navigateAway = leftmost [cancel,() <$ ensembleCreated]
  return (navigateAway, serverRequests)
