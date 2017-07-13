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

data Server = Server {
  password :: String,
  clients :: Map.Map ClientHandle WS.Connection,
  spaces :: Map.Map String Ensemble
}

newServer :: Server
newServer = Server {
    password = "password",
    clients = Map.empty,
    spaces = Map.fromList [("testingA",emptyEnsemble),("testingB",emptyEnsemble),("testingC",emptyEnsemble)]
  }

addClient :: Server -> WS.Connection -> (ClientHandle,Server)
addClient s x = (i,s { clients=newMap})
  where i = head ([0..] \\ Map.keys (clients s))
        newMap = Map.insert i x (clients s)

deleteClient :: ClientHandle -> Server -> Server
deleteClient h s = s { clients = Map.delete h (clients s) }

createEnsemble :: String -> Server -> Server
createEnsemble w s = s { spaces = Map.insertWith (\_ x -> x) w emptyEnsemble (spaces s) }

-- if space already exists, createEnsemble does not make any change

edit :: String -> Int -> Definition -> Server -> Server
edit w z d s = s { spaces = Map.adjust (editDef z d) w (spaces s) }

setView :: String -> String -> View -> Server -> Server
setView w k v s = s { spaces = Map.adjust (editView k v) w (spaces s) }

getEnsembleList :: MVar Server -> IO ServerResponse
getEnsembleList s = readMVar s >>= return . EnsembleList . Map.keys . spaces

getViews :: MVar Server -> String -> IO [Sited String View]
getViews s w = readMVar s >>= return . fromMaybe [] . fmap (Map.elems . Map.mapWithKey Sited . views) . Map.lookup w . spaces

getServerClientCount :: MVar Server -> IO Int
getServerClientCount s = readMVar s >>= return . Map.size . clients

data Client = Client {
  handle :: ClientHandle,
  connection :: WS.Connection,
  authenticated :: Bool,
  space :: Maybe String
  }

newClient :: ClientHandle -> WS.Connection -> Client
newClient h c = Client {
  handle = h,
  connection = c,
  authenticated = False,
  space = Nothing
  }

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
  processLoop s $ newClient h ws'

processLoop :: MVar Server -> Client -> IO ()
processLoop s c = do
  m <- try (WS.receiveData (connection c))
  case m of
    Right x -> do
      let x' = decode (T.unpack x) :: Result JSString
      case x' of
        Ok x'' -> do
          c' <- processResult s c $ decode (fromJSString x'')
          processLoop s c'
        Error x'' -> do
          putStrLn $ "Error: " ++ x''
          processLoop s c
    Left WS.ConnectionClosed -> close s c "unexpected loss of connection"
    Left (WS.CloseRequest _ _) -> close s c "connection closed by request from peer"
    Left (WS.ParseException e) -> do
      putStrLn ("parse exception: " ++ e)
      processLoop s c

close :: MVar Server -> Client -> String -> IO ()
close s c msg = do
  putStrLn $ "closing connection: " ++ msg
  updateServer s $ deleteClient (handle c)
  return ()


onlyIfAuthenticated :: Client -> IO Client -> IO Client
onlyIfAuthenticated c f = if (authenticated c) then f else do
  putStrLn "ignoring request from non-authenticated client"
  return c


processResult :: MVar Server -> Client -> Result ServerRequest
  -> IO Client
processResult _ c (Error x) = do
  putStrLn ("Error: " ++ x)
  return c
processResult s c (Ok x) = do
  processRequest s c x


processRequest :: MVar Server -> Client -> ServerRequest -> IO Client

processRequest s c (Authenticate x) = do
  pwd <- readMVar s >>= return . password
  if x == pwd
    then do
      putStrLn "received authenticate with correct password"
      return $ c { authenticated = True }
    else do
      putStrLn "received authenticate with wrong password"
      return $ c { authenticated = False }

processRequest s c GetEnsembleList = onlyIfAuthenticated c $ do
  putStrLn "GetEnsembleList"
  getEnsembleList s >>= respond (connection c)
  return c

processRequest s c (JoinEnsemble x) = onlyIfAuthenticated c $ do
  putStrLn $ "joining space " ++ x
  return $ c { space = Just x }

processRequest s c LeaveEnsemble = onlyIfAuthenticated c $ do
  putStrLn $ maybe "warning: no space to leave" ("leaving space " ++) $ space c
  return $ c { space = Nothing }

processRequest s c (CreateEnsemble x) = onlyIfAuthenticated c $ do
  putStrLn $ "CreateEnsemble " ++ x
  updateServer s $ createEnsemble x
  getEnsembleList s >>= broadcast s
  return c

processRequest s c (EnsembleRequest x) = onlyIfAuthenticated c $ processInEnsemble s c x

processRequest s c GetServerClientCount = onlyIfAuthenticated c $ do
  putStrLn "GetServerClientCount"
  getServerClientCount s >>= respond (connection c) . ServerClientCount
  return c

processRequest s c _ = do
  putStrLn "warning: request failed pattern matching"
  return c


processInEnsemble :: MVar Server -> Client -> Sited String (EnsembleRequest Definition) -> IO Client
processInEnsemble s c (Sited x y) = processEnsembleRequest s c x y

processEnsembleRequest :: MVar Server -> Client -> String -> EnsembleRequest Definition -> IO Client

processEnsembleRequest s c w x@(SendChat name msg) = do
  putStrLn $ "SendChat in " ++ w ++ " from " ++ name ++ ": " ++ msg
  broadcast s $ EnsembleResponse (Sited w (Chat name msg))
  return c

processEnsembleRequest s c w x@(ZoneRequest (Sited zone (Edit value))) = do
  putStrLn $ "Edit in (" ++ w ++ "," ++ (show zone) ++ "): " ++ (show value)
  updateServer s $ edit w zone value
  broadcastNoOrigin s c $ EnsembleResponse (Sited w (ZoneResponse (Sited zone (Edit value))))
  return c

processEnsembleRequest s c w x@(ZoneRequest (Sited zone (Evaluate value))) = do
  putStrLn $ "Eval in (" ++ w ++ "," ++ (show zone) ++ "): " ++ (show value)
  broadcastNoOrigin s c $ EnsembleResponse (Sited w (ZoneResponse (Sited zone (Evaluate value))))
  return c

processEnsembleRequest s c w GetViews = do
  putStrLn $ "GetViews in " ++ w
  vs <- getViews s w -- IO [Sited String View]
  forM_ vs $ \v -> respond (connection c) (EnsembleResponse (Sited w (View v)))
  return c

processEnsembleRequest s c w x@(SetView (Sited key value)) = do
  putStrLn $ "SetView in (" ++ w ++ "," ++ key ++ "): " ++ (show value)
  updateServer s $ setView w key value
  return c

processEnsembleRequest s c w x@(TempoChange cps) = do
  putStrLn "placeholder: TempoChange"
  return c

processEnsembleRequest s c w _ = do
  putStrLn "warning: action failed pattern matching"
  return c

respond :: WS.Connection -> ServerResponse -> IO ()
respond ws x = do
  let x' = T.pack (encodeStrict x)
  WS.sendTextData ws x'

broadcast :: MVar Server -> ServerResponse -> IO ()
broadcast s x = do
  s' <- takeMVar s
  let x' = T.pack (encodeStrict x)
  let cs' = Map.elems $ clients s'
  forM_ cs' $ \ws -> WS.sendTextData ws x'
  putMVar s s'

broadcastNoOrigin :: MVar Server -> Client -> ServerResponse -> IO ()
broadcastNoOrigin s c x = do
  s' <- takeMVar s
  let x' = T.pack (encodeStrict x)
  let cs' = Map.elems $ Map.delete (handle c) $ clients s'
  forM_ cs' $ \ws -> WS.sendTextData ws x'
  putMVar s s'

updateServer :: MVar Server -> (Server -> Server) -> IO (MVar Server)
updateServer s f = do
  s' <- takeMVar s
  putMVar s (f s')
  return s
