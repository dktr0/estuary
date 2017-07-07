module Main where

import Data.Text (Text)
import Data.List ((\\))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import qualified Network.WebSockets as WS
import Control.Monad
import Control.Concurrent.MVar
import Control.Exception (try)
import Text.JSON

import Estuary.Protocol.JSON

data Space = Space {
  zones :: Map.Map Int ZoneValue
  }

editZone :: Int -> ZoneValue -> Space -> Space
editZone z v s = s { zones = Map.insert z v (zones s) }

type ClientHandle = Int

data ServerState = ServerState {
  password :: String,
  clients :: Map.Map ClientHandle WS.Connection,
  spaces :: Map.Map String Space
}

newServerState :: ServerState
newServerState = ServerState {
    password = "password",
    clients = Map.empty,
    spaces = Map.empty
  }



addClient :: ServerState -> WS.Connection -> (ClientHandle,ServerState)
addClient s x = (i,s { clients=newMap})
  where i = head ([0..] \\ Map.keys (clients s))
        newMap = Map.insert i x (clients s)

createSpace :: String -> ServerState -> ServerState
createSpace w s = s { spaces = Map.insertWith (\_ x -> x) w (Space Map.empty) (spaces s) }

-- if space already exists, createSpace does not make any change

edit :: String -> Zone -> ZoneValue -> ServerState -> ServerState
edit w z v s = s { spaces = Map.adjust (editZone z v) w (spaces s) }


data ClientState = ClientState {
  handle :: ClientHandle,
  connection :: WS.Connection,
  authenticated :: Bool,
  space :: Maybe String
  }

newClientState :: ClientHandle -> WS.Connection -> ClientState
newClientState h c = ClientState {
  handle = h,
  connection = c,
  authenticated = False,
  space = Nothing
  }

main = do
  putStrLn "Estuary collaborative editing server (listening on port 8001)"
  server <- newMVar newServerState
  WS.runServer "0.0.0.0" 8001 $ connectionHandler server

connectionHandler :: MVar ServerState -> WS.PendingConnection -> IO ()
connectionHandler s ws = do
  putStrLn "received new connection"
  ws' <- WS.acceptRequest ws
  ss <- takeMVar s
  let (h,ss') = addClient ss ws'
  putMVar s ss'
  let c = newClientState h ws'
  WS.forkPingThread ws' 30
  processLoop s c

processLoop :: MVar ServerState -> ClientState -> IO ()
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
    Left WS.ConnectionClosed -> putStrLn "unexpected loss of connection"
    Left (WS.CloseRequest _ _) -> putStrLn "connection closed by request from peer"
    Left (WS.ParseException e) -> putStrLn ("parse exception: " ++ e)


onlyIfAuthenticated :: ClientState -> IO ClientState -> IO ClientState
onlyIfAuthenticated c f = if (authenticated c) then f else do
  putStrLn "ignoring request from non-authenticated client"
  return c


processResult :: MVar ServerState -> ClientState -> Result (Request ZoneValue)
  -> IO ClientState
processResult _ c (Error x) = do
  putStrLn ("Error: " ++ x)
  return c
processResult s c (Ok x) = do
  putStrLn "processResult"
  processRequest s c x


processRequest :: MVar ServerState -> ClientState -> Request ZoneValue -> IO ClientState

processRequest s c (Authenticate x) = do
  pwd <- readMVar s >>= return . password
  if x == pwd
    then do
      putStrLn "received authenticate with correct password"
      return $ c { authenticated = True }
    else do
      putStrLn "received authenticate with wrong password"
      return $ c { authenticated = False }

processRequest s c RequestSpaceList = onlyIfAuthenticated c $ do
  putStrLn "RequestSpaceList"
  spaceNames <- readMVar s >>= return . Map.keys . spaces
  respond (connection c) $ SpaceList spaceNames
  return c

processRequest s c (JoinSpace x) = onlyIfAuthenticated c $ do
  putStrLn $ "joining space " ++ x
  return $ c { space = Just x }

processRequest s c LeaveSpace = onlyIfAuthenticated c $ do
  putStrLn $ maybe "warning: no space to leave" ("leaving space " ++) $ space c
  return $ c { space = Nothing }

processRequest s c (CreateSpace x) = onlyIfAuthenticated c $ do
  putStrLn $ "CreateSpace " ++ x
  updateServer s $ createSpace x
  return c

processRequest s c (SpaceRequest x) = onlyIfAuthenticated c $ processInSpace s c x


processInSpace :: MVar ServerState -> ClientState -> InSpace (Action ZoneValue) -> IO ClientState
processInSpace s c (InSpace x y) = do
  putStrLn "warning: processInSpace is disregarding space names as a placeholder"
  processAction s c x y


processAction :: MVar ServerState -> ClientState -> String -> Action ZoneValue -> IO ClientState

processAction s c w x@(Chat name msg) = do
  putStrLn $ "Chat in " ++ w ++ " from " ++ name ++ ": " ++ msg
  broadcast s $ SpaceResponse (InSpace w x)
  return c

processAction s c w x@(Edit zone value) = do
  putStrLn $ "Edit in (" ++ w ++ "," ++ (show zone) ++ "): " ++ (show value)
  updateServer s $ edit w zone value
  broadcastNoOrigin s c $ SpaceResponse (InSpace w x)
  return c

processAction s c w x@(Eval zone value) = do
  putStrLn $ "Eval in (" ++ w ++ "," ++ (show zone) ++ "): " ++ (show value)
  broadcastNoOrigin s c $ SpaceResponse (InSpace w x)
  return c

processAction s c w x@(Tempo at beat cps) = do
  putStrLn "placeholder: Tempo"
  return c

processAction s c w x@(TempoChange cps) = do
  putStrLn "placeholder: TempoChange"
  return c


respond :: WS.Connection -> ServerResponse -> IO ()
respond ws x = do
  let x' = T.pack (encodeStrict x)
  WS.sendTextData ws x'

broadcast :: MVar ServerState -> ServerResponse -> IO ()
broadcast s x = do
  s' <- takeMVar s
  let x' = T.pack (encodeStrict x)
  let cs' = Map.elems $ clients s'
  forM_ cs' $ \ws -> WS.sendTextData ws x'
  putMVar s s'

broadcastNoOrigin :: MVar ServerState -> ClientState -> ServerResponse -> IO ()
broadcastNoOrigin s c x = do
  s' <- takeMVar s
  let x' = T.pack (encodeStrict x)
  let cs' = Map.elems $ Map.delete (handle c) $ clients s'
  forM_ cs' $ \ws -> WS.sendTextData ws x'
  putMVar s s'

updateServer :: MVar ServerState -> (ServerState -> ServerState) -> IO (MVar ServerState)
updateServer s f = do
  s' <- takeMVar s
  putMVar s (f s')
  return s
