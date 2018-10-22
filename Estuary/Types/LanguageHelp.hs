-- {-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.LanguageHelp where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM -- just used for our test, maybe delete-able later
import Estuary.Types.MiniTidalReference
import Estuary.Types.LaCalleReference
import Estuary.Languages.TidalParser

import Estuary.Languages.MiniTidal
import Estuary.Languages.CQenze
import Estuary.Languages.Morelia
import Estuary.Languages.Saborts
import Estuary.Languages.Saludos
import Estuary.Languages.ColombiaEsPasion
import Estuary.Languages.Si
import Estuary.Languages.Sentidos
import Estuary.Languages.Natural
import Estuary.Languages.Medellin
import Estuary.Languages.LaCalle
import Estuary.Languages.Maria
import Estuary.Languages.Crudo
import Estuary.Languages.Puntoyya
import Estuary.Languages.Sucixxx
import Estuary.Languages.Vocesotrevez
import Estuary.Languages.Imagina
import Estuary.Languages.Alobestia

data LanguageReference =
  MiniTidalReference |
  LaCalleReference
  deriving (Show,Eq)

languageReference :: LanguageReference -> String
languageReference MiniTidalReference = (miniTidalReference Palindrome)  ++ (miniTidalReference Brak)
languageReference LaCalleReference = (laCalleReference HolaChoche) ++ (laCalleReference UnasChelas)

tidalParserToHelp :: TidalParser -> String
tidalParserToHelp MiniTidal = languageReference MiniTidalReference
tidalParserToHelp LaCalle = languageReference LaCalleReference

languageHelpWidget :: (MonadWidget t m) => TidalParser -> m () --this should be TidalParser -> Language -> m (Event t String)
languageHelpWidget t  = do
    helpText <- el "div" $ text (tidalParserToHelp t)
    -- clickEv <- wrapDomEvent (_el_element helpText) (onEventName Click) mouseOffsetXY
    return ()
