{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Estuary.Types.TextNotation where

import GHC.Generics
import Data.Aeson
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char
import Control.Monad.Except

import Estuary.Types.TidalParser

data TextNotation =
  UnspecifiedNotation |
  TidalTextNotation TidalParser |
  Punctual |
  CineCer0 |
  TimeNot |
  Seis8s |
  Hydra |
  JSoLang Text |
  EphemeralNotation Text
  -- Ver |
  -- Oir
  deriving (Read,Eq,Ord,Show,Generic)

instance ToJSON TextNotation where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TextNotation

textNotationDropDownLabel :: TextNotation -> String
textNotationDropDownLabel UnspecifiedNotation = ""
textNotationDropDownLabel (TidalTextNotation x) = show x
textNotationDropDownLabel x = show x

determineTextNotation :: Text -> [Text] -> Either Text (Text,TextNotation)
determineTextNotation x ns =
  case T.isPrefixOf "##" x of
    False -> return (x,UnspecifiedNotation)
    True -> do
      let x' = T.lines x
      let firstLine = head x'
      let otherLines = tail x'
      let (firstWord,y) = T.break isSpace firstLine
      let (secondWord,z) = T.break isSpace $ T.stripStart y
      let firstLine' = T.stripStart z
      case pragmaToTextNotation (T.toLower $ T.drop 2 firstWord) (T.toLower secondWord) ns of
        Nothing -> throwError "unrecognized notation pragma"
        Just n -> return (T.unlines (firstLine':otherLines), n)

pragmaToTextNotation :: Text -> Text -> [Text] -> Maybe TextNotation
pragmaToTextNotation "punctual" _ _ = Just Punctual
pragmaToTextNotation "tidal" _ _ = Just $ TidalTextNotation MiniTidal
pragmaToTextNotation "minitidal" _ _ = Just $ TidalTextNotation MiniTidal
pragmaToTextNotation "cinecer0" _ _ = Just CineCer0
pragmaToTextNotation "timenot" _ _ = Just TimeNot
pragmaToTextNotation "seis8s" _ _ = Just Seis8s
pragmaToTextNotation "hydra" _ _ = Just Hydra
pragmaToTextNotation "jsolang" x _ = Just $ JSoLang x
pragmaToTextNotation x _ ns =
  case elem x ns of
    True -> Just $ EphemeralNotation x
    False -> Nothing
