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
  Ensemble |
  EnsembleName |
  EnsembleExpiry |
  AnonymousParticipants |
  TerminalChat |
  Theme |
  Load |
  Peak |
  About|
  NewTempo|
  EvalTerm |
  JoiningEnsemble |
  EnsembleUserName |
  EnsembleLocation |
  EnsembleLogin |
  Syntax |
  Connections |
  Latency |
  Resolution |
  Brightness |
  Reference |
  Settings |
  Activity |
  ActivityDescription |
  Status |
  StatusDescription |
  LatencyDescription |
  LoadDescription |
  FPS |
  FPSDescription |
  IPaddress |
  IPaddressDescription |
  TerminalViewCommands |
  Voices
  deriving (Show,Eq)

translate :: Term -> Language -> Text
translate TerminalViewCommands Français = "Estuary (un symbiote de codage en ligne)"
translate TerminalViewCommands English = "Estuary (a live coding symbiont)"
translate TerminalViewCommands Español = "Estuary (una simbionte live coding)"

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

translate Ensemble English = "Ensemble"
translate Ensemble Français = "Ensemble"
translate Ensemble Español = "Ensamble"

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
translate ModeratorPassword Español = "Contraseña del/de la moderador(a):"

translate CommunityPassword English = "Community password:"
translate CommunityPassword Español = "Contraseña comunitaria:"


translate HostPassword English = "Host password:"
translate HostPassword Español = "Contraseña del/de la anfintrión(a):"


translate ParticipantPassword English = "Participant password:"
translate ParticipantPassword Español = "Contraseña del/de la participante:"


translate EnsembleName Français = "Nom de l'ensemble:"
translate EnsembleName English = "Ensemble name:"
translate EnsembleName Español = "Nombre del ensamble:"

translate EnsembleExpiry English = "Ensemble Expiry:"
translate EnsembleExpiry Español = "Expiración del ensemble:"


translate AnonymousParticipants English = "Anonymous Participants"
translate AnonymousParticipants Español = "Participantes Anónimos"


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

translate EvalTerm Français = "Eval"
translate EvalTerm English = "Eval"
translate EvalTerm Español = "Eval"

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
translate Syntax Español = "Sintaxis"

translate Connections English = "connections"
translate Connections Español = "conexiones"

translate Latency English = "latency"
translate Latency Español = "latencia"

translate Resolution English = "Resolution"
translate Resolution Español = "Resolución"

translate Brightness English = "Brightness"
translate Brightness Español = "Brillo"

translate Reference English = "Reference"
translate Reference Español = "Referencia"

translate Settings English = "Settings"
translate Settings Español = "Configuración"

translate Activity English = "activity"
translate Activity Español = "actividad"

translate ActivityDescription English = "The last time participants edited some code"
translate ActivityDescription Español = "La última vez que las participantes editaron el código."

translate Status English = "status"
translate Status Español = "estatus"

translate StatusDescription English = "Displays how are the participants doing today"
translate StatusDescription Español = "Muestra cómo se sientes l@s participantes el día de hoy"

translate LatencyDescription English = "The delay before a transfer of data begins"
translate LatencyDescription Español = "El retraso de tiempo entre la transferencia de datos"

translate LoadDescription English = "The amount of work each participant computer is doing"
translate LoadDescription Español = "La cantidad de trabajo que cada computadora participante está haciendo"

translate FPS English = "FPS"
translate FPS Español = "CPS"

translate FPSDescription English = "The frames per second of the participants"
translate FPSDescription Español = "Los cuadros por segundo de cada participante"

translate IPaddress English = "IP Address"
translate IPaddress Español = "dirección IP"

translate IPaddressDescription English = "The IP addresses of the participants"
translate IPaddressDescription Español = "Las direcciones IP de l@s participantes"

translate Voices English = "voices"

translate x _ = translate x English
