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
  About
  deriving (Show,Eq)

translate :: Term -> Language -> Text
translate EstuaryDescription Français = "Estuary (un symbiote de codage en ligne)"
translate EstuaryDescription English = "Estuary (a live coding symbiont)"
translate EstuaryDescription Español = "Estuary (una simbionte live coding)"

translate Tutorials Français = "Tutoriels"
translate Tutorials Español = "Tutoriales"
translate Tutorials English = "Tutorials"

translate About Français = "À propos de Estuary"
translate About Español = "Acerca de Estuary"
translate About English = "About Estuary"

translate Solo Français = "Mode Seul"
translate Solo Español = "Modo Solo"
translate Solo English = "Solo Mode"

translate Collaborate Français = "Collaborer"
translate Collaborate Español = "Colaborar"
translate Collaborate English = "Collaborate"

translate Send Français = "Envoyer"
translate Send Español = "enviar"
translate Send English = "Send"

translate CreateNewEnsemble Français = "Créer un nouvel ensemble"
translate CreateNewEnsemble English = "Create new ensemble"
translate CreateNewEnsemble Español = "Crear nuevo ensamble"

translate Language Français = "Langage"
translate Language English = "Language"
translate Language Español = "Idioma"

translate Confirm Français = "Confirmer"
translate Confirm English = "Confirm"
translate Confirm Español = "Confirmar"

translate Cancel Français = "Annuler"
translate Cancel English = "Cancel"
translate Cancel Español = "Cancelar"

translate CreateNewEnsembleNote Français = "Note: pour créer un ensemble, vous devez entrer un mot de passe administrateur"
translate CreateNewEnsembleNote English = "Note: to create an ensemble you must enter an administrator password"
translate CreateNewEnsembleNote Español = "Nota: para crear un enamble escribe la contraseña de administrador"

translate AdministratorPassword Français = "Mot de passe administrateur:"
translate AdministratorPassword English = "Administrator password:"
translate AdministratorPassword Español = "Contraseña del admin:"

translate EnsembleName Français = "Nom de l'ensemble:"
translate EnsembleName English = "Ensemble name:"
translate EnsembleName Español = "Nombre del ensamble:"

translate EnsemblePassword Français = "Mot de passe de l'ensemble:"
translate EnsemblePassword English = "Ensemble password:"
translate EnsemblePassword Español = "Contraseña del ensamble:"

translate TerminalChat Français = "Terminal/Chat:"
translate TerminalChat English = "Terminal/Chat:"
translate TerminalChat Español = "Terminal/Chat:"

translate Send Français = "Envoyer"
translate Send English = "Send"
translate Send Español = "enviar"

translate Theme Français = "Thème"
translate Theme English = "Theme"
translate Theme Español = "Tema"

translate Load Français = "charge"
translate Load English = "load"
translate Load Español = "carga"

translate Peak Français = "pic"
translate Peak English = "peak"
translate Peak Español = "tope"

translate x _ = pack $ "?" ++ show x
