{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Estuary.Types.Language where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson

data Language =
  NoLanguage |
  English |
  Español |
  Français
  deriving (Read,Ord,Show,Eq,Generic)
 
instance ToJSON Language where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Language
instance ToJSONKey Language
instance FromJSONKey Language

languages :: [Language]
languages = [English,Español,Français]

-- Translation does not exist
-- String to be displayed when no translation yet exists
-- TODO - fill this in for other languages
translationDNE :: Language -> Text
translationDNE English = "Sorry no English translation exists (please request translations at github.com/dktr0/Estuary)"
translationDNE _ = "Sorry no translation exists (please request translations at github.com/dktr0/Estuary)"
