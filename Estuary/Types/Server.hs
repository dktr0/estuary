module Estuary.Types.Server where

import qualified Network.WebSockets as WS
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Data.List ((\\))
import Data.Maybe (fromMaybe)

import Estuary.Types.Client
import Estuary.Types.Definition
import Estuary.Types.Sited
import Estuary.Types.Response
import Estuary.Types.View
import qualified Estuary.Types.Ensemble as E

data Server = Server {
  password :: String,
  clients :: Map.Map ClientHandle Client,
  ensembles :: Map.Map String E.Ensemble
}

newServer :: Server
newServer = Server {
  password = "",
  clients = Map.empty,
  ensembles = Map.empty
}


updateServer :: MVar Server -> (Server -> Server) -> IO (MVar Server)
updateServer s f = do
  s' <- takeMVar s
  putMVar s (f s')
  return s

updateClient :: MVar Server -> ClientHandle -> (Client -> Client) -> IO ()
updateClient s c f = do
  s' <- takeMVar s
  let c' = (clients s') Map.! c 
  let c'' = f c'
  putMVar s $ s' { clients = Map.adjust (const c'') c (clients s') }

updateClientWithServer :: MVar Server -> ClientHandle -> (Server -> Client -> Client) -> IO ()
updateClientWithServer s c f = do
  s' <- takeMVar s
  let c' = (clients s') Map.! c 
  let c'' = f s' c'
  putMVar s $ s' { clients = Map.adjust (const c'') c (clients s') }


getPassword :: MVar Server -> IO String
getPassword s = readMVar s >>= return . password
 
addClient :: Server -> WS.Connection -> (ClientHandle,Server)
addClient s x = (i,s { clients=newMap})
  where i = head ([0..] \\ Map.keys (clients s))
        newMap = Map.insert i (newClient i x) (clients s)

deleteClient :: ClientHandle -> Server -> Server
deleteClient h s = s { clients = Map.delete h (clients s) }

createEnsemble :: String -> String -> Server -> Server
createEnsemble name pwd s = s { ensembles = Map.insertWith (\_ x -> x) name e (ensembles s) }
  where e = E.setPassword pwd E.emptyEnsemble

-- if space already exists, createEnsemble does not make any change

edit :: String -> Int -> Definition -> Server -> Server
edit w z d s = s { ensembles = Map.adjust (E.editDef z d) w (ensembles s) }

setView :: String -> String -> View -> Server -> Server
setView w k v s = s { ensembles = Map.adjust (E.editView k v) w (ensembles s) }

getEnsembleList :: MVar Server -> IO ServerResponse
getEnsembleList s = readMVar s >>= return . EnsembleList . Map.keys . ensembles

getViews :: MVar Server -> String -> IO [Sited String View]
getViews s w = readMVar s >>= return . fromMaybe [] . fmap (Map.elems . Map.mapWithKey Sited . E.views) . Map.lookup w . ensembles

getServerClientCount :: MVar Server -> IO Int
getServerClientCount s = readMVar s >>= return . Map.size . clients

getEnsemblePassword :: MVar Server -> String -> IO String
getEnsemblePassword s e = readMVar s >>= return . fromMaybe [] . fmap (E.password) . Map.lookup e . ensembles



