-- {-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.LanguageHelp where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM -- just used for our test, maybe delete-able later
import Estuary.Types.MiniTidalReference
import Estuary.Types.LaCalleReference
import Estuary.Types.TidalParser
import Estuary.Languages.TidalParsers
import Estuary.Types.TextNotation
import Data.Map
import Control.Monad

tidalParserToHelp :: (MonadWidget t m) => TextNotation -> m ()
tidalParserToHelp (TidalTextNotation MiniTidal) = miniTidalHelpFile
tidalParserToHelp (TidalTextNotation LaCalle) = laCalleHelpFile

-- tidalParserToHelpDyn (TidalTextNotation LaCalle) = laCalleHelpFile
-- a widget  that renders a TidalTextNotation
languageHelpWidget' :: (MonadWidget t m) => TextNotation ->  m () --this should be TidalParser -> Language -> m (Event t String)
languageHelpWidget' t = do
   tidalParserToHelp  t -- m ()
   return ()

-- a widget  that renders a dynamic TidalTextNotation
languageHelpWidget :: (MonadWidget t m) => Dynamic t TextNotation ->  m () --this should be TidalParser -> Language -> m (Event t String)
languageHelpWidget t = do
   p <- mapDyn (tidalParserToHelp) t --Dynamic t (m ())
   p' <- dyn p --m Event t (()) but if Event t (Event t String) then we should flattenn event to only Event t String with coincidence
        --a <-- dyn z  -- :: EVent t (Event t String)
        --a' <-- hold never a -- :: Behavior t (Event t String)switch
        --let a'' = switch a' --:: Event t String
   return ()

-- let initialWidget = f initialNotation
-- let widgetChanges = updated z
-- widgetHold initialWidgets widgetxChabges
