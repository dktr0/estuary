module Estuary.Languages.TextReplacement (applyTextReplacement) where

import Data.Text (Text)
import Text.Parsec

import Estuary.Types.TextNotation
import Estuary.Types.TidalParser

applyTextReplacement :: (TextNotation,Text) -> Either ParseError (TextNotation,Text)

-- 1. for base languages, we simply return the unmodified input...
applyTextReplacement (TidalTextNotation MiniTidal,x) = Right (TidalTextNotation MiniTidal,x)
applyTextReplacement (Punctual,x) = Right (Punctual,x)
applyTextReplacement (CineCer0,x) = Right (CineCer0,x)
applyTextReplacement (TimeNot,x) = Right (TimeNot,x)

-- 2. for text replacement languages, we recursively apply this function to the output
-- of their parsers. For example:
--   assume we have Experiment :: TextNotation
--   and myExperimentParser :: Text -> Either ParseError Text
--   where the returned Text is the program translated into Punctual (as Text), then...
-- applyTextReplacement (Experiment,t) = myExperimentParser t >>= prependNotation Punctual >>= applyTextReplacement

-- 3. finally, for all unmatched cases (eg. minilanguages that haven't been reimplemented as text replacement languages)
-- we just (again) return the unmodified input
applyTextReplacement x = Right x


prependNotation :: TextNotation -> Text -> Either ParseError (TextNotation,Text)
prependNotation n t = Right (n,t)
