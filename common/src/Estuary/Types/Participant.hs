module Estuary.Types.Participant where

-- A representation of a pseudonymous participant in an Ensemble
-- For each ensemble, this list is maintained by the server
-- and broadcast to the clients in that ensemble periodically
-- (together with a count of anonymous participants)

import Data.Text
import Data.Time

data Participant = Participant {
  name :: Text,
  location :: Text,
  status :: Text,
  lastEdit :: UTCTime,
  mainLoad :: Double,
  animationLoad :: Double,
  latency :: NominalDiffTime
  browser :: Text
  }
