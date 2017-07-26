module Main where

import Data.Text (Text)
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import qualified Network.WebSockets as WS
import Control.Monad
import Control.Concurrent.MVar
import Control.Exception (try)
import Text.JSON

import Estuary.Utility
import Estuary.Types.Definition
import Estuary.Types.Sited
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.EditOrEval
import Estuary.Types.Ensemble
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.View


type ClientHandle = Int

data Client = Client {
  handle :: ClientHandle,
  connection :: WS.Connection,
  authenticated :: Bool,
  ensemble :: Maybe String
}

newClient :: ClientHandle -> WS.Connection -> Client
newClient h c = Client {
  handle = h,
  connection = c,
  authenticated = False,
  ensemble = Nothing
}


data Server = Server {
  password :: String,
  clients :: Map.Map ClientHandle Client,
  ensembles :: Map.Map String Ensemble
}

newServer :: Server
newServer = Server {
  password = "password",
  clients = Map.empty,
  ensembles = Map.fromList [("testingA",emptyEnsemble),("testingB",emptyEnsemble)]
}

getPassword :: MVar Server -> IO String
getPassword s = readMVar s >>= return . password

updateClient :: MVar Server -> ClientHandle -> (Client -> Client) -> IO ()
updateClient s c f = do
  s' <- takeMVar s
  let c' = (clients s') Map.! c 
  let c'' = f c'
  putMVar s $ s' { clients = Map.adjust (const c'') c (clients s') }
 
addClient :: Server -> WS.Connection -> (ClientHandle,Server)
addClient s x = (i,s { clients=newMap})
  where i = head ([0..] \\ Map.keys (clients s))
        newMap = Map.insert i (newClient i x) (clients s)

deleteClient :: ClientHandle -> Server -> Server
deleteClient h s = s { clients = Map.delete h (clients s) }

createEnsemble :: String -> Server -> Server
createEnsemble w s = s { ensembles = Map.insertWith (\_ x -> x) w emptyEnsemble (ensembles s) }

-- if space already exists, createEnsemble does not make any change

edit :: String -> Int -> Definition -> Server -> Server
edit w z d s = s { ensembles = Map.adjust (editDef z d) w (ensembles s) }

setView :: String -> String -> View -> Server -> Server
setView w k v s = s { ensembles = Map.adjust (editView k v) w (ensembles s) }

getEnsembleList :: MVar Server -> IO ServerResponse
getEnsembleList s = readMVar s >>= return . EnsembleList . Map.keys . ensembles

getViews :: MVar Server -> String -> IO [Sited String View]
getViews s w = readMVar s >>= return . fromMaybe [] . fmap (Map.elems . Map.mapWithKey Sited . views) . Map.lookup w . ensembles

getServerClientCount :: MVar Server -> IO Int
getServerClientCount s = readMVar s >>= return . Map.size . clients



main = do
  putStrLn "Estuary collaborative editing server (listening on port 8001)"
  server <- newMVar newServer
  WS.runServer "0.0.0.0" 8001 $ connectionHandler server

connectionHandler :: MVar Server -> WS.PendingConnection -> IO ()
connectionHandler s ws = do
  putStrLn "received new connection"
  ws' <- WS.acceptRequest ws
  ss <- takeMVar s
  let (h,ss') = addClient ss ws'
  putMVar s ss'
  WS.forkPingThread ws' 30
  getServerClientCount s >>= respondAll s . ServerClientCount
  processLoop ws' s h

processLoop :: WS.Connection -> MVar Server -> ClientHandle -> IO ()
processLoop ws s h = do
  m <- try $ WS.receiveData ws
  case m of
    Right x -> do
      let x' = decode (T.unpack x) :: Result JSString
      case x' of
        Ok x'' -> do
          processResult s h $ decode (fromJSString x'')
          processLoop ws s h
        Error x'' -> do
          putStrLn $ "Error: " ++ x''
          processLoop ws s h
    Left WS.ConnectionClosed -> close s h "unexpected loss of connection"
    Left (WS.CloseRequest _ _) -> close s h "connection closed by request from peer"
    Left (WS.ParseException e) -> do
      putStrLn ("parse exception: " ++ e)
      processLoop ws s h

close :: MVar Server -> ClientHandle -> String -> IO ()
close s h msg = do
  putStrLn $ "closing connection: " ++ msg
  updateServer s $ deleteClient h
  return ()


onlyIfAuthenticated :: MVar Server -> ClientHandle -> IO () -> IO ()
onlyIfAuthenticated s h f = do
  s' <- readMVar s
  let c = clients s' Map.! h
  if (authenticated c) then f else putStrLn "ignoring request from non-authenticated client"


processResult :: MVar Server -> ClientHandle -> Result ServerRequest -> IO ()
processResult _ c (Error x) = putStrLn ("Error: " ++ x)
processResult s c (Ok x) = processRequest s c x


processRequest :: MVar Server -> ClientHandle -> ServerRequest -> IO ()

processRequest s c (Authenticate x) = do
  pwd <- getPassword s
  if x == pwd 
    then do 
      putStrLn "received authenticate with correct password"
      updateClient s c $ \x -> x { authenticated = True } 
    else do
      putStrLn "received authenticate with wrong password"
      updateClient s c $ \x -> x { authenticated = False }

processRequest s c GetEnsembleList = onlyIfAuthenticated s c $ do
  putStrLn "GetEnsembleList"
  getEnsembleList s >>= respond s c

processRequest s c (JoinEnsemble x) = onlyIfAuthenticated s c $ do
  putStrLn $ "joining ensemble " ++ x
  updateClient s c $ \c' -> c' { ensemble = Just x }

processRequest s c LeaveEnsemble = onlyIfAuthenticated s c $ do
  putStrLn $ "leaving ensemble"
  updateClient s c $ \c' -> c' { ensemble = Nothing }

processRequest s c (CreateEnsemble x) = onlyIfAuthenticated s c $ do
  putStrLn $ "CreateEnsemble " ++ x
  updateServer s $ createEnsemble x
  getEnsembleList s >>= respondAll s

processRequest s c (EnsembleRequest x) = onlyIfAuthenticated s c $ processInEnsemble s c x

processRequest s c GetServerClientCount = do
  putStrLn "GetServerClientCount"
  getServerClientCount s >>= respond s c . ServerClientCount


processInEnsemble :: MVar Server -> ClientHandle -> Sited String (EnsembleRequest Definition) -> IO ()
processInEnsemble s c (Sited e x) = processEnsembleRequest s c e x

processEnsembleRequest :: MVar Server -> ClientHandle -> String -> EnsembleRequest Definition -> IO ()

processEnsembleRequest s c e x@(SendChat name msg) = do
  putStrLn $ "SendChat in " ++ e ++ " from " ++ name ++ ": " ++ msg
  respondEnsemble s e $ EnsembleResponse (Sited e (Chat name msg))

processEnsembleRequest s c e x@(ZoneRequest (Sited zone (Edit value))) = do
  putStrLn $ "Edit in (" ++ e ++ "," ++ (show zone) ++ "): " ++ (show value)
  updateServer s $ edit e zone value
  respondEnsembleNoOrigin s c e $ EnsembleResponse (Sited e (ZoneResponse (Sited zone (Edit value))))

processEnsembleRequest s c e x@(ZoneRequest (Sited zone (Evaluate value))) = do
  putStrLn $ "Eval in (" ++ e ++ "," ++ (show zone) ++ "): " ++ (show value)
  respondEnsembleNoOrigin s c e $ EnsembleResponse (Sited e (ZoneResponse (Sited zone (Evaluate value))))

processEnsembleRequest s c e GetViews = do
  putStrLn $ "GetViews in " ++ e
  vs <- getViews s e -- IO [Sited String View]
  forM_ vs $ \v -> respond s c (EnsembleResponse (Sited e (View v)))

processEnsembleRequest s c e x@(SetView (Sited key value)) = do
  putStrLn $ "SetView in (" ++ e ++ "," ++ key ++ "): " ++ (show value)
  updateServer s $ setView e key value
  respondEnsembleNoOrigin s c e $ EnsembleResponse (Sited e (View (Sited key value))) 

processEnsembleRequest s c e x@(TempoChange cps) = putStrLn "placeholder: TempoChange"

processEnsembleRequest _ _ _ _ = putStrLn "warning: action failed pattern matching"


send :: ServerResponse -> [Client] -> IO ()
send x = mapM_ $ \c -> WS.sendTextData (connection c) $ (T.pack . encodeStrict) x

respond :: MVar Server -> ClientHandle -> ServerResponse -> IO ()
respond s c x = withMVar s $ (send x) . (:[]) . (Map.! c)  . clients

respondAll :: MVar Server -> ServerResponse -> IO ()
respondAll s x = withMVar s $ (send x) . Map.elems . clients

respondAllNoOrigin :: MVar Server -> ClientHandle -> ServerResponse -> IO ()
respondAllNoOrigin s c x = withMVar s $ (send x) . Map.elems . Map.delete c . clients

respondEnsemble :: MVar Server -> String -> ServerResponse -> IO ()
respondEnsemble s e x = withMVar s $ (send x) . Map.elems . ensembleFilter e . clients 

respondEnsembleNoOrigin :: MVar Server -> ClientHandle -> String -> ServerResponse -> IO ()
respondEnsembleNoOrigin s c e x = withMVar s $ (send x) . Map.elems . Map.delete c . ensembleFilter e . clients

ensembleFilter :: String -> Map.Map ClientHandle Client -> Map.Map ClientHandle Client
ensembleFilter e = Map.filter $ (==(Just e)) . ensemble 

updateServer :: MVar Server -> (Server -> Server) -> IO (MVar Server)
updateServer s f = do
  s' <- takeMVar s
  putMVar s (f s')
  return s


