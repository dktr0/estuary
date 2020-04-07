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
  ModeratorPassword |
  CommunityPassword |
  HostPassword |
  ParticipantPassword |
  EnsembleName |
  EnsembleExpiry |
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
  EnsembleLogin |
  Syntax
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

translate ModeratorPassword English = "Moderator password:"

translate CommunityPassword English = "Community password:"

translate HostPassword English = "Password for ensemble host:"

translate ParticipantPassword English = "Password to participate in ensemble:"

translate EnsembleName Français = "Nom de l'ensemble:"
translate EnsembleName English = "Ensemble name:"
translate EnsembleName Español = "Nombre del ensamble:"

translate EnsembleExpiry English = "Ensemble Expiry:"

translate TerminalChat Français = "Terminal/Chat:"
translate TerminalChat English = "Terminal/Chat:"
translate TerminalChat Español = "Terminal/Chat:"

translate Send Français = "Envoyer"
translate Send English = "Send"
translate Send Español = "Enviar"

translate Theme Français = "Thème"
translate Theme English = "Theme"
translate Theme Español = "Tema"

translate Load Français = "charge"
translate Load English = "load"
translate Load Español = "carga"

translate NewTempo Français = "Établir un nouveau tempo"
translate NewTempo English = "Set new tempo"
translate NewTempo Español = "Establecer Nuevo tempo"

translate Eval Français = "Eval"
translate Eval English = "Eval"
translate Eval Español = "Eval"

translate JoiningEnsemble Français = "Joindre l'ensemble"
translate JoiningEnsemble English = "Joining ensemble"
translate JoiningEnsemble Español = "Unirse al ensamble"


translate EnsembleUserName Français = "Entrer le nom d'utilisateur (optionel) comme membre de l'ensemble:"
translate EnsembleUserName English = "Enter username (optional) within ensemble:"
translate EnsembleUserName Español = "Ingresar nombre de usuario dentro del ensamble (opcional):"


translate EnsembleLocation Français = "Décrivez votre situation géographique (optionel):"
translate EnsembleLocation English = "Describe your location (optional):"
translate EnsembleLocation Español = "Coloca tu localización (opcional):"

translate EnsembleLogin Français = "S'identifier"
translate EnsembleLogin English = "Login"
translate EnsembleLogin Español = "Ingresar"

translate Peak Français = "pic"
translate Peak English = "peak"
translate Peak Español = "tope"

translate Syntax English = "Syntax"

translate x _ = pack $ "?" ++ show x
