{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.ServerState where

-- This module presents the type ServerState which represents everything that an
-- Estuary server keeps track of in memory as it runs.

import qualified Network.WebSockets as WS
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import Control.Concurrent.MVar
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Text

import Estuary.Types.Tempo
import Estuary.Types.Client
import Estuary.Types.Definition
import Estuary.Types.Response
import Estuary.Types.View
import qualified Estuary.Types.EnsembleS as E
import qualified Estuary.Types.Ensemble as E

data ServerState = ServerState {
  administrativePassword :: Text,
  clients :: IntMap.IntMap Client,
  ensembles :: Map.Map Text E.EnsembleS,
  connectionCount :: Int
}

newServerState :: ServerState
newServerState = ServerState {
  administrativePassword = "",
  clients = IntMap.empty,
  ensembles = Map.empty,
  connectionCount = 0
}

addClient :: UTCTime -> ServerState -> WS.Connection -> (ClientHandle,ServerState)
addClient t s x = (i,s { clients=newMap} )
  where i = lowestAvailableKey $ clients s
        newMap = IntMap.insert i (newClient t i x) (clients s)

lowestAvailableKey :: IntMap.IntMap a -> IntMap.Key
lowestAvailableKey m = Prelude.head ([0..] \\ (IntMap.keys m))

deleteClient :: ClientHandle -> ServerState -> ServerState
deleteClient h s = s { clients = IntMap.delete h (clients s) }

-- if space already exists, createEnsemble does not make any change
createEnsemble :: Text -> Text -> UTCTime -> ServerState -> ServerState
createEnsemble name pwd t s = s { ensembles = Map.insertWith (\_ x -> x) name e (ensembles s) }
  where e = E.writePassword pwd (E.emptyEnsembleS t)

writeZone :: Text -> Int -> Definition -> ServerState -> ServerState
writeZone eName zone def s = s { ensembles = Map.adjust (E.modifyEnsemble (E.writeZone zone def)) eName (ensembles s) }

writeView :: Text -> Text -> View -> ServerState -> ServerState
writeView eName vName v s = s { ensembles = Map.adjust (E.modifyEnsemble (E.writeView vName v)) eName (ensembles s) }

writeTempo :: Text -> Tempo -> ServerState -> ServerState
writeTempo eName t s = s { ensembles = Map.adjust (E.modifyEnsemble (E.writeTempo t)) eName (ensembles s) }
