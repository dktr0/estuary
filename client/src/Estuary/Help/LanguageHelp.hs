{-# LANGUAGE OverloadedStrings #-}

module Estuary.Help.LanguageHelp where

import Reflex
import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as Td
import Data.Map
import Control.Monad
import GHCJS.DOM.EventM -- just used for our test, maybe delete-able later

import Estuary.Help.NoHelpFile
import Estuary.Help.MiniTidal
import Estuary.Help.CQenze
import Estuary.Help.LaCalle
import Estuary.Help.Sucixxx
import Estuary.Help.Togo
import Estuary.Help.PunctualAudio
import Estuary.Help.Hydra
import Estuary.Help.CineCer0
import Estuary.Types.TidalParser
import Estuary.Languages.TidalParsers
import Estuary.Types.TextNotation

parserToHelp :: (MonadWidget t m) => TextNotation -> m ()
parserToHelp (TidalTextNotation CQenze) = cqenzeHelpFile
parserToHelp (TidalTextNotation Sucixxx) = sucixxxHelpFile
parserToHelp (TidalTextNotation MiniTidal) = miniTidalHelpFile
parserToHelp (TidalTextNotation LaCalle) = laCalleHelpFile
parserToHelp (TidalTextNotation Togo) = togoHelpFile
parserToHelp Punctual = punctualAudioHelpFile
parserToHelp Hydra = hydraHelpFile
parserToHelp CineCer0 = cineCer0HelpFile
parserToHelp _ = noHelpFile
