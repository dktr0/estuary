module Estuary.Types.Language where

data Language =
  English |
  Español
  Français
  deriving (Read,Show,Eq,Ord)

languages :: [Language]
languages = [English,Español,Français]

-- Translation does not exist
-- String to be displayed when no translation yet exists
-- TODO - fill this for Español
translationDNE :: Language -> String
translationDNE English = "Sorry no English translation exists (please request translations at github.com/dktr0/Estuary)"
translationDNE _ = "Sorry no translation exists (please request translations at github.com/dktr0/Estuary)"
