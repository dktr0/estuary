module Estuary.Languages.TextReplacement (applyTextReplacement) where

import Data.Text (Text)
import Text.Parsec

import Estuary.Types.TextNotation
import Estuary.Types.TidalParser
import Estuary.Types.Definition

applyTextReplacement :: TextProgram -> Either ParseError TextProgram

-- 1. for base languages, we simply return the unmodified input...
applyTextReplacement (TidalTextNotation MiniTidal,x,t) = Right (TidalTextNotation MiniTidal,x,t)
applyTextReplacement (Punctual,x,t) = Right (Punctual,x,t)
applyTextReplacement (CineCer0,x,t) = Right (CineCer0,x,t)
applyTextReplacement (TimeNot,x,t) = Right (TimeNot,x,t)

-- 2. for text replacement languages, we recursively apply this function to the output
-- of their parsers. For example:
--   assume we have Experiment :: TextNotation
--   and myExperimentParser :: Text -> Either ParseError Text
--   where the returned Text is the program translated into Punctual (as Text), then...
-- applyTextReplacement (Experiment,t) = myExperimentParser t >>= prependNotation Punctual >>= applyTextReplacement
--  or what about this?
--   assume we have (OnTheFlyLanguage x) -- where data TextNotation = ... OnTheFlyLanguage Text
--   also, access to getTextReplacementParser :: Text -> (Text -> Either ParseError Text)
--   also, access to getTextReplacementBaseNotation :: Text -> TextNotation
--   then:
-- applyTextReplacement (OnTheFlyLanguage x) = getTextReplacementParser x >>= prependNotation (getTextReplacementBaseNotation x) >>= applyTextReplacement

-- 3. finally, for all unmatched cases (eg. minilanguages that haven't been reimplemented as text replacement languages)
-- we just (again) return the unmodified input
applyTextReplacement x = Right x


prependNotation :: TextNotation -> Text -> Either ParseError (TextNotation,Text)
prependNotation n t = Right (n,t)
