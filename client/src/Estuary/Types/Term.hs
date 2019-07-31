{-# LANGUAGE OverloadedStrings #-}
module Estuary.Types.Term where

import Data.Text
import Estuary.Types.Language

data Term =
  EstuaryDescription |
  Tutorials |
  Solo |
  Collaborate |
  Send |
  CreateNewEnsemble |
  Language |
  Confirm |
  Cancel |
  CreateNewEnsembleNote |
  AdministratorPassword |
  EnsembleName |
  EnsemblePassword |
  TerminalChat |
  Theme |
  Load |
  Peak |
  About|
  NewTempo|
  Eval |
  JoiningEnsemble |
  EnsembleUserName |
  EnsembleLocation |
  EnsembleLogin
  deriving (Show,Eq)

translate :: Term -> Language -> Text
translate EstuaryDescription English = "Estuary (a live coding symbiont)"
translate EstuaryDescription Español = "Estuary (una simbionte live coding)"

translate Tutorials Español = "Tutoriales"
translate Tutorials English = "Tutorials"

translate About Español = "Acerca de Estuary"
translate About English = "About Estuary"

translate Solo Español = "Modo Solo"
translate Solo English = "Solo Mode"

translate Collaborate Español = "Colaborar"
translate Collaborate English = "Collaborate"

translate Send Español = "enviar"
translate Send English = "Send"

translate CreateNewEnsemble English = "Create new ensemble"
translate CreateNewEnsemble Español = "Crear nuevo ensamble"

translate Language English = "Language"
translate Language Español = "Idioma"

translate Confirm English = "Confirm"
translate Confirm Español = "Confirmar"

translate Cancel English = "Cancel"
translate Cancel Español = "Cancelar"

translate CreateNewEnsembleNote English = "Note: to create an ensemble you must enter an administrator password"
translate CreateNewEnsembleNote Español = "Nota: para crear un enamble escribe la contraseña de administrador"

translate AdministratorPassword English = "Administrator password:"
translate AdministratorPassword Español = "Contraseña del admin:"

translate EnsembleName English = "Ensemble name:"
translate EnsembleName Español = "Nombre del ensamble:"

translate EnsemblePassword English = "Ensemble password:"
translate EnsemblePassword Español = "Contraseña del ensamble:"

translate TerminalChat English = "Terminal/Chat:"
translate TerminalChat Español = "Terminal/Chat:"

translate Send English = "Send"
translate Send Español = "Enviar"

translate Theme English = "Theme"
translate Theme Español = "Tema"

translate Load English = "load"
translate Load Español = "carga"

translate NewTempo English = "Set new tempo"
translate NewTempo Español = "Establecer Nuevo tempo"

translate Eval English = "Eval"
translate Eval Español = "Eval"

translate JoiningEnsemble English = "Joining ensemble"

translate EnsembleUserName English = "Enter username (optional) within ensemble:"

translate EnsembleLocation English = "Describe your location (optional):"

translate EnsembleLogin English = "Login"

translate x _ = pack $ "?" ++ show x
