{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- The type EnsembleS represents an Ensemble from the standpoint of the server.
-- It wraps the type Ensemble with additional information that is only held by
-- the server (such as the password for the ensemble).

module Estuary.Types.EnsembleS where

import Data.Text (Text)
import Data.Time
import GHC.Generics
import Data.Aeson

import Estuary.Types.Ensemble

data EnsembleS = EnsembleS {
  ensemble :: Ensemble,
  password :: Text
  } deriving (Generic)

instance ToJSON EnsembleS
instance FromJSON EnsembleS

emptyEnsembleS :: UTCTime -> EnsembleS
emptyEnsembleS t = EnsembleS {
  ensemble = emptyEnsemble t,
  password = ""
  }

writePassword :: Text -> EnsembleS -> EnsembleS
writePassword s e = e { password = s }

modifyEnsemble :: (Ensemble -> Ensemble) -> EnsembleS -> EnsembleS
modifyEnsemble f e = e { ensemble = f (ensemble e) }
