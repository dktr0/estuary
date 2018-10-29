module Estuary.Types.Language where

data Language =
  English |
  Español
  deriving (Read,Show,Eq,Ord)

languages :: [Language]
languages = [English,Español]
