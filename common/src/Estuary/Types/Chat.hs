{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Estuary.Types.Chat where

import Data.Time
import Data.Text (Text)
import GHC.Generics
import Data.Aeson

data Chat = Chat {
  chatTime :: UTCTime,
  chatSender :: Text,
  chatText :: Text
  } deriving (Eq,Generic)

instance Ord Chat where
  compare x y = compare (chatTime x) (chatTime y)

instance ToJSON Chat
instance FromJSON Chat

showChatMessage :: Chat -> Text
showChatMessage x = chatSender x <> ": " <> chatText x
