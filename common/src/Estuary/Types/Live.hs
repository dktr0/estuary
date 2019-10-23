{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.Live where

import GHC.Generics
import Data.Aeson

data Liveness = L3 | L4 deriving (Eq,Show,Generic)

instance ToJSON Liveness where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Liveness

data Live a = Live a Liveness | Edited a a deriving (Eq,Generic)

instance ToJSON a => ToJSON (Live a) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON a => FromJSON (Live a)

forRendering :: Live a -> a
forRendering (Live a _) = a
forRendering (Edited a _) = a

forEditing :: Live a -> a
forEditing (Live a _) = a
forEditing (Edited _ a) = a

instance Show a => Show (Live a) where
  show (Live a L3) = "L3:"++(show a)
  show (Live a L4) = "L4:"++(show a)
  show (Edited a b) = "L3e:"++(show a)++":"++(show b)

isEdited :: Live a -> Bool
isEdited (Edited _ _) = True
isEdited _ = False
