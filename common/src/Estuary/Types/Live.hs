{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.Live where

import Text.JSON
import Text.JSON.Generic

data Liveness = L3 | L4 deriving (Eq,Show,Data,Typeable)

instance JSON Liveness where
  showJSON = toJSON
  readJSON = fromJSON

data Live a = Live a Liveness | Edited a a deriving(Eq,Data,Typeable)

instance Data a => JSON (Live a) where
  showJSON = toJSON
  readJSON = fromJSON

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
