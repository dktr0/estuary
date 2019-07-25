{-# LANGUAGE OverloadedStrings #-}

-- The type EnsembleS represents an Ensemble from the standpoint of the server.
-- It wraps the type Ensemble with additional information that is only held by
-- the server (such as the password for the ensemble).

module Estuary.Types.EnsembleS where

data EnsembleS = EnsembleS {
  ensemble :: Ensemble,
  password :: Text
}

writePassword :: Text -> EnsembleS -> EnsembleS
writePassword s e = e { password = s }

modifyEnsemble :: (Ensemble -> Ensemble) -> EnsembleS -> EnsembleS
modifyEnsemble f e = e { ensemble = f (ensemble e) }
