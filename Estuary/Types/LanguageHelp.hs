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


-- tidalParserToHelp :: TextNotation -> m ()
tidalParserToHelp (TidalTextNotation MiniTidal) = miniTidalHelpFile
tidalParserToHelp (TidalTextNotation LaCalle) = laCalleHelpFile

-- a widget  that renders a TidalTextNotation
languageHelpWidget' :: (MonadWidget t m) => TextNotation ->  m () --this should be TidalParser -> Language -> m (Event t String)
languageHelpWidget' t = do
   tidalParserToHelp  t
   return ()

-- a widget  that renders a dynamic TidalTextNotation
-- languageHelpWidget :: (MonadWidget t m) => Dynamic t TextNotation ->  m () --this should be TidalParser -> Language -> m (Event t String)
-- languageHelpWidget t = do
--    t' <- mapDyn (\x -> TidalTextNotation x) t  --Dynamic t  TextNotation
--    -- let t = constDyn $ (TidalTextNotation MiniTidal)
--    p <- mapDyn (\x -> tidalParserToHelp  x) t
--
--    return ()
