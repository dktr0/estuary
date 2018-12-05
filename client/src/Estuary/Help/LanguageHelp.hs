-- {-# LANGUAGE OverloadedStrings #-}

module Estuary.Help.LanguageHelp where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM -- just used for our test, maybe delete-able later
import Estuary.Help.MiniTidal
import Estuary.Help.LaCalle
import Estuary.Help.SvgOp
import Estuary.Help.PunctualAudio
import Estuary.Help.PunctualVideo
import Estuary.Types.TidalParser
import Estuary.Languages.TidalParsers
import Estuary.Types.TextNotation
import Data.Map
import Control.Monad

parserToHelp :: (MonadWidget t m) => TextNotation -> m ()
parserToHelp (TidalTextNotation MiniTidal) = miniTidalHelpFile
parserToHelp (TidalTextNotation LaCalle) = laCalleHelpFile
parserToHelp SvgOp = svgOp
parserToHelp PunctualAudio = punctualAudio
parserToHelp PunctualVideo = punctualVideo
parserToHelp _ = miniTidalHelpFile


-- tidalParserToHelpDyn (TidalTextNotation LaCalle) = laCalleHelpFile
-- a widget  that renders a TidalTextNotation
languageHelpWidget' :: (MonadWidget t m) => TextNotation ->  m () --this should be TidalParser -> Language -> m (Event t String)
languageHelpWidget' t = do
   parserToHelp  t -- m ()
   return ()

-- a widget  that renders a dynamic TidalTextNotation
languageHelpWidget :: (MonadWidget t m) => Dynamic t TextNotation ->  m () --this should be TidalParser -> Language -> m (Event t String)
languageHelpWidget t = do
   p <- mapDyn (parserToHelp) t --Dynamic t (m ())
   p' <- dyn p --m Event t (()) but if Event t (Event t String) then we should flattenn event to only Event t String with coincidence
        --a <-- dyn z  -- :: EVent t (Event t String)
        --a' <-- hold never a -- :: Behavior t (Event t String)switch
        --let a'' = switch a' --:: Event t String
   return ()

-- let initialWidget = f initialNotation
-- let widgetChanges = updated z
-- widgetHold initialWidgets widgetxChabges
