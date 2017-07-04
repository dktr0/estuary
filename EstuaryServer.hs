module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS
import Estuary.Protocol.JSON
import Control.Concurrent.MVar
import Control.Exception (try)

data Space = Space (Map Int ZoneValue)

data ServerState = ServerState {
  password :: MVar String
  spaces :: MVar (Map String Space)
   }

newServerState :: IO ServerState
newServerState = do
  p <- newMVar "password"
  return $ ServerState {
    password = p
  }

getPassword :: ServerState -> IO String
getPassword s = readMVar (password s)

setPassword :: String -> ServerState -> IO ()
setPassword x s = swapMVar (password s) x

data ClientState = ClientState {
  authenticated :: Bool
  space :: Maybe String
  }

newClientState :: ClientState
newClientState = ClientState {
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
  ws' <- WS.acceptRequest c
  WS.forkPingThread ws' 30
  processLoop s newClientState ws'

processLoop :: ServerState -> ClientState -> WS.Connection -> IO ()
processLoop s c ws = do
  m <- try (WS.receiveData ws)
  case m of
    Right x -> do
      c' <- processRequest s c (decode (T.unpack x))
      loop s c' ws
    Left WS.ConnectionClosed -> putStrLn "unexpected loss of connection"
    Left (WS.CloseRequest _ _) -> putStrLn "connection closed by request from peer"
    Left (WS.ParseException e) -> putStrLn ("parse exception: " ++ e)


onlyIfAuthenticated :: ClientState -> IO ClientState -> IO ClientState
onlyIfAuthenticated c f = if (authenticated c) then f else $ return c


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


processInSpace :: ServerState -> ClientState -> InSpace ZoneValue -> IO ClientState
processInSpace s c (InSpace x y) = do
  putStrLn "warning: processInSpace is disregarding space names as a placeholder"
  processAction s c y


processAction :: ServerState -> ClientState -> Action ZoneValue -> IO ClientState

processAction s c (Chat name msg) = do
  putStrLn "placeholder: Chat"
  return c

processAction s c (Edit zone value) = do
  putStrLn "placeholder: Edit"
  return c

processAction s c (Eval zone value) = do
  putStrLn "placeholder: Eval"
  return c

processAction s c (Tempo at beat cps) = do
  putStrLn "placeholder: Tempo"
  return c

processAction s c (TempoChange cps) = do
  putStrLn "placeholder: TempoChange"
  return c

-- some old code that we needed to send responses to specific clients:
-- WS.sendTextData conn (T.pack (encodeStrict z))
