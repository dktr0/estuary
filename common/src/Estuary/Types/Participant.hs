{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.Participant where

-- A representation of a pseudonymous participant in an Ensemble
-- For each ensemble, this list is maintained by the server
-- and broadcast to the clients in that ensemble periodically
-- (together with a count of anonymous participants)

import Data.Text
import Data.Time
import GHC.Generics
import Data.Aeson

data Participant = Participant {
  name :: Text,
  location :: Text,
  status :: Text,
  lastEdit :: UTCTime,
  mainLoad :: Int,
  animationFPS :: Int,
  animationLoad :: Int,
  latency :: NominalDiffTime,
  browser :: Text
  } deriving (Eq,Generic)

instance ToJSON Participant where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Participant
