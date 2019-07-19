{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.ServerState where

-- This module presents the type ServerState which represents everything that an
-- Estuary server keeps track of in memory as it runs.

import qualified Network.WebSockets as WS
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Text

import Estuary.Types.Tempo
import Estuary.Types.Client
import Estuary.Types.Definition
import Estuary.Types.Sited
import Estuary.Types.Response
import Estuary.Types.View
import qualified Estuary.Types.Ensemble as E

data ServerState = ServerState {
  password :: Text,
  clients :: Map.Map ClientHandle Client,
  ensembles :: Map.Map Text E.Ensemble,
  connectionCount :: Int
}

newServerState :: ServerState
newServerState = ServerState {
  password = "",
  clients = Map.empty,
  ensembles = Map.empty,
  connectionCount = 0
}

addClient :: UTCTime -> ServerState -> WS.Connection -> (ClientHandle,ServerState)
addClient t s x = (i,s { clients=newMap})
  where i = Prelude.head ([0..] \\ Map.keys (clients s))
        newMap = Map.insert i (newClient t i x) (clients s)

deleteClient :: ClientHandle -> ServerState -> ServerState
deleteClient h s = s { clients = Map.delete h (clients s) }

createEnsemble :: Text -> Text -> UTCTime -> ServerState -> ServerState
createEnsemble name pwd t s = s { ensembles = Map.insertWith (\_ x -> x) name e (ensembles s) }
  where e = E.setPassword pwd (E.emptyEnsemble t)

-- if space already exists, createEnsemble does not make any change

edit :: Text -> Int -> Definition -> ServerState -> ServerState
edit w z d s = s { ensembles = Map.adjust (E.editDef z d) w (ensembles s) }

setView :: Text -> Text -> View -> ServerState -> ServerState
setView w k v s = s { ensembles = Map.adjust (E.editView k v) w (ensembles s) }

setDefaultView :: Text -> View -> ServerState -> ServerState
setDefaultView w v s = s { ensembles = Map.adjust (E.editDefaultView v) w (ensembles s)}

deleteView :: Text -> Text -> ServerState -> ServerState
deleteView e v s = s { ensembles = Map.adjust (E.deleteView v) e (ensembles s) }

tempoChangeInEnsemble :: Text -> Tempo -> ServerState -> ServerState
tempoChangeInEnsemble e t s = s { ensembles = Map.adjust (E.tempoChange t) e (ensembles s) }
