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

data Space = Space (Map.Map Int ZoneValue)

type ClientHandle = Int

data ServerState = ServerState {
  password :: MVar String,
  spaces :: MVar (Map.Map String Space),
  clients :: MVar (Map.Map ClientHandle WS.Connection)
}

newServerState :: IO ServerState
newServerState = do
  p <- newMVar "password"
  s <- newMVar Map.empty
  c <- newMVar Map.empty
  return $ ServerState {
    password = p,
    spaces = s,
    clients = c
  }

getPassword :: ServerState -> IO String
getPassword s = readMVar (password s)

setPassword :: String -> ServerState -> IO String
setPassword x s = swapMVar (password s) x

addClient :: ServerState -> WS.Connection -> IO ClientHandle
addClient s x = do
  oldClientMap <- takeMVar (clients s)
  let i = head ([0..] \\ Map.keys oldClientMap) -- find lowest int not currently used as a client id
  let newClientMap = Map.insert i x oldClientMap
  putMVar (clients s) newClientMap
  return i


data ClientState = ClientState {
  handle :: ClientHandle,
  authenticated :: Bool,
  space :: Maybe String
  }

newClientState :: ClientHandle -> ClientState
newClientState h = ClientState {
  handle = h,
  authenticated = False,
  space = Nothing
  }

main = do
  putStrLn "Estuary collaborative editing server (listening on port 8001)"
  server <- newServerState
  setPassword "password" server
  WS.runServer "0.0.0.0" 8001 $ connectionHandler server

connectionHandler :: ServerState -> WS.PendingConnection -> IO ()
connectionHandler s ws = do
  putStrLn "received new connection"
  ws' <- WS.acceptRequest ws
  h <- addClient s ws'
  let c = newClientState h
  WS.forkPingThread ws' 30
  processLoop s c ws'

processLoop :: ServerState -> ClientState -> WS.Connection -> IO ()
processLoop s c ws = do
  m <- try (WS.receiveData ws)
  case m of
    Right x -> do
      let x' = decode (T.unpack x) :: Result JSString
      case x' of
        Ok x'' -> do
          c' <- processResult s c $ decode (fromJSString x'')
          processLoop s c' ws
        Error x'' -> do
          putStrLn $ "Error: " ++ x''
          processLoop s c ws
    Left WS.ConnectionClosed -> putStrLn "unexpected loss of connection"
    Left (WS.CloseRequest _ _) -> putStrLn "connection closed by request from peer"
    Left (WS.ParseException e) -> putStrLn ("parse exception: " ++ e)


onlyIfAuthenticated :: ClientState -> IO ClientState -> IO ClientState
onlyIfAuthenticated c f = if (authenticated c) then f else do
  putStrLn "ignoring request from non-authenticated client"
  return c


processResult :: ServerState -> ClientState -> Result (Request ZoneValue)
  -> IO ClientState
processResult _ c (Error x) = do
  putStrLn ("Error: " ++ x)
  return c
processResult s c (Ok x) = do
  putStrLn "processResult"
  processRequest s c x


processRequest :: ServerState -> ClientState -> Request ZoneValue -> IO ClientState

processRequest s c (Authenticate x) = do
  pwd <- getPassword s
  if x == pwd
    then do
      putStrLn "received authenticate with correct password"
      return $ c { authenticated = True }
    else do
      putStrLn "received authenticate with wrong password"
      return $ c { authenticated = False }

processRequest s c RequestSpaceList = onlyIfAuthenticated c $ do
  putStrLn "placeholder: would respond with space list"
  return c

processRequest s c (JoinSpace x) = onlyIfAuthenticated c $ do
  putStrLn $ "joining space " ++ x
  return $ c { space = Just x }

processRequest s c LeaveSpace = onlyIfAuthenticated c $ do
  putStrLn $ maybe "warning: no space to leave" ("leaving space " ++) $ space c
  return $ c { space = Nothing }

processRequest s c (CreateSpace x) = onlyIfAuthenticated c $ do
  putStrLn $ "placeholder: would create new space " ++ x
  return c

processRequest s c (SpaceRequest x) = onlyIfAuthenticated c $ processInSpace s c x


processInSpace :: ServerState -> ClientState -> InSpace (Action ZoneValue) -> IO ClientState
processInSpace s c (InSpace x y) = do
  putStrLn "warning: processInSpace is disregarding space names as a placeholder"
  processAction s c x y


processAction :: ServerState -> ClientState -> String -> Action ZoneValue -> IO ClientState

processAction s c w x@(Chat name msg) = do
  putStrLn $ "Chat in " ++ w ++ " from " ++ name ++ ": " ++ msg
  broadcast s $ SpaceResponse (InSpace w x)
  return c

processAction s c w x@(Edit zone value) = do
  putStrLn $ "Edit in (" ++ w ++ "," ++ (show zone) ++ "): " ++ (show value)
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


broadcast :: JSON a => ServerState -> a -> IO ()
broadcast s x = do
  let x' = T.pack (encodeStrict x)
  cs <- takeMVar (clients s)
  let cs' = Map.elems cs
  forM_ cs' $ \ws -> WS.sendTextData ws x'
  putMVar (clients s) cs

broadcastNoOrigin :: JSON a => ServerState -> ClientState -> a -> IO ()
broadcastNoOrigin s c x = do
  let x' = T.pack (encodeStrict x)
  let h = handle c
  cs <- takeMVar (clients s)
  let cs' = Map.elems $ Map.delete h cs
  forM_ cs' $ \ws -> WS.sendTextData ws x'
  putMVar (clients s) cs
