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

import Estuary.Utility
import Estuary.Types.Definition
import Estuary.Types.Sited
import Estuary.Types.Action
import Estuary.Types.EditOrEval
import Estuary.Types.Space
import Estuary.Types.Request
import Estuary.Types.Response

type ClientHandle = Int

data Server = Server {
  password :: String,
  clients :: Map.Map ClientHandle WS.Connection,
  spaces :: Map.Map String Space
}

newServer :: Server
newServer = Server {
    password = "password",
    clients = Map.empty,
    spaces = Map.fromList [("testingA",emptySpace),("testingB",emptySpace),("testingC",emptySpace)]
  }

addClient :: Server -> WS.Connection -> (ClientHandle,Server)
addClient s x = (i,s { clients=newMap})
  where i = head ([0..] \\ Map.keys (clients s))
        newMap = Map.insert i x (clients s)

deleteClient :: ClientHandle -> Server -> Server
deleteClient h s = s { clients = Map.delete h (clients s) }

createSpace :: String -> Server -> Server
createSpace w s = s { spaces = Map.insertWith (\_ x -> x) w emptySpace (spaces s) }

-- if space already exists, createSpace does not make any change

edit :: String -> Int -> Definition -> Server -> Server
edit w z d s = s { spaces = Map.adjust (editDef z d) w (spaces s) }

getSpaceList :: MVar Server -> IO ServerResponse
getSpaceList s = readMVar s >>= return . SpaceList . Map.keys . spaces


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

processRequest s c RequestSpaceList = onlyIfAuthenticated c $ do
  putStrLn "RequestSpaceList"
  getSpaceList s >>= respond (connection c)
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
  getSpaceList s >>= broadcast s
  return c

processRequest s c (SpaceRequest x) = onlyIfAuthenticated c $ processInSpace s c x


processInSpace :: MVar Server -> Client -> Sited String (Action Definition) -> IO Client
processInSpace s c (Sited x y) = processAction s c x y

processAction :: MVar Server -> Client -> String -> Action Definition -> IO Client

processAction s c w x@(Chat name msg) = do
  putStrLn $ "Chat in " ++ w ++ " from " ++ name ++ ": " ++ msg
  broadcast s $ SpaceResponse (Sited w x)
  return c

processAction s c w x@(ZoneAction (Sited zone (Edit value))) = do
  putStrLn $ "Edit in (" ++ w ++ "," ++ (show zone) ++ "): " ++ (show value)
  updateServer s $ edit w zone value
  broadcastNoOrigin s c $ SpaceResponse (Sited w x)
  return c

processAction s c w x@(ZoneAction (Sited zone (Eval value))) = do
  putStrLn $ "Eval in (" ++ w ++ "," ++ (show zone) ++ "): " ++ (show value)
  broadcastNoOrigin s c $ SpaceResponse (Sited w x)
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
