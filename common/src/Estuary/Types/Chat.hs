{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Estuary.Types.Chat where

import Data.Time
import Data.Text (Text)
import Text.JSON
import Text.JSON.Generic

data Chat = Chat {
  chatTime :: UTCTime,
  chatSender :: Text,
  chatText :: Text
  } deriving (Eq, Data,Typeable)

instance Ord Chat where
  compare x y = compare (chatTime x) (chatTime y)

instance JSON Chat where
  showJSON = toJSON
  readJSON = fromJSON

showChatMessage :: Chat -> Text
showChatMessage x = chatSender x <> ": " <> chatText x
