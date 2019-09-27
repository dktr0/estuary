{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

﻿module Estuary.Types.Language where

import Data.Text (Text)

data Language =
  English |
  Español |
  Français
  deriving (Read,Show,Eq,Ord,Generic)

instance ToJSON Language where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Language

languages :: [Language]
languages = [English,Español,Français]

-- Translation does not exist
-- String to be displayed when no translation yet exists
-- TODO - fill this in for other languages
translationDNE :: Language -> Text
translationDNE English = "Sorry no English translation exists (please request translations at github.com/dktr0/Estuary)"
translationDNE _ = "Sorry no translation exists (please request translations at github.com/dktr0/Estuary)"
