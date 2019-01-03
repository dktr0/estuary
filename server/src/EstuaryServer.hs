{-# LANGUAGE OverloadedStrings #-}
module EstuaryServer where

import Data.Text (Text)
import Data.List ((\\))
import Data.Maybe (fromMaybe,isJust,fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.State
import Control.Monad.Except

import qualified Database.SQLite.Simple as SQLite
import Text.JSON
import qualified Network.WebSockets as WS
import qualified Network.Wai as WS
import qualified Network.Wai.Handler.WebSockets as WS
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings, ssIndices, ssMaxAge)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip
import WaiAppStatic.Types (unsafeToPiece,MaxAge(..))
import Data.Time
import Data.Time.Clock.POSIX
import Data.Map

import Estuary.Utility
import Estuary.Types.Definition
import Estuary.Types.Sited
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import qualified Estuary.Types.Ensemble as E
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.View
import Estuary.Types.Client
import Estuary.Types.Server
import Estuary.Types.Database
import Estuary.Types.Tempo
import Estuary.Types.Transaction

runServerWithDatabase :: String -> Int -> SQLite.Connection -> IO ()
runServerWithDatabase pswd port db = do
  postLogToDatabase db $ "Estuary collaborative editing server, listening on port " ++ (show port)
  postLogToDatabase db $ "administrative password: " ++ pswd
  es <- readEnsembles db
  postLogToDatabase db $ (show (size es)) ++ " ensembles restored from database"
  s <- newMVar $ newServer { password = pswd, ensembles = es }
  let settings = (defaultWebAppSettings "Estuary.jsexe") {
    ssIndices = [unsafeToPiece "index.html"],
    ssMaxAge = MaxAgeSeconds 30 -- 30 seconds max cache time
    }
  run port $ gzipMiddleware $ WS.websocketsOr WS.defaultConnectionOptions (webSocketsApp db s) (staticApp settings)

gzipMiddleware :: WS.Middleware
gzipMiddleware = gzip $ def {
  gzipFiles = GzipPreCompressed GzipIgnore
}

webSocketsApp :: SQLite.Connection -> MVar Server -> WS.ServerApp -- = PendingConnection -> IO ()
webSocketsApp db sMVar ws = do
  ws' <- try $ WS.acceptRequest ws
  case ws' of
    Right ws'' -> do
      postLogToDatabase db $ "received new connection"
      ss <- takeMVar sMVar
      let (cHandle,ss') = addClient ss ws''
      let ss'' = ss' { connectionCount = connectionCount ss' + 1 }
      ss''' <- runTransaction broadcastServerClientCount db cHandle ss'
      putMVar sMVar ss'''
      (WS.forkPingThread ws'' 30) `catch` \(SomeException e) -> postLogToDatabase db $ "exception in forking ping thread: " ++ (show e)
      processLoop db ws'' sMVar cHandle
    Left (SomeException e) -> do
      postLogToDatabase db $ "exception during WS.acceptRequest: " ++ (show e)



processLoop :: SQLite.Connection -> WS.Connection -> MVar Server -> ClientHandle -> IO ()
processLoop db ws sMVar cHandle = do
  m <- try $ WS.receiveData ws

  case m of
    Right m' -> do
      s <- takeMVar sMVar
      s' <- if (isJust $ Data.Map.lookup cHandle (clients s))
        then runTransaction (processMessage m') db cHandle s
        else do
          postLogToDatabase db $ "*** warning - failed sanity check: WS.receiveData succeeded for client already deleted from server"
          return s
      putMVar sMVar s'
      processLoop db ws sMVar cHandle
    Left WS.ConnectionClosed -> do
      s <- takeMVar sMVar
      s' <- runTransaction (close "unexpected loss of connection") db cHandle s
      putMVar sMVar s'
    Left (WS.CloseRequest _ _) -> do
      s <- takeMVar sMVar
      s' <- runTransaction (close "connection closed by request from peer") db cHandle s
      putMVar sMVar s'
    Left (WS.ParseException e) -> do
      postLogToDatabase db $ "parse exception: " ++ e
      processLoop db ws sMVar cHandle
    Left _ -> postLogToDatabase db $ "***unknown exception in processLoop - terminating this client's connection***"
    -- Left (WS.UnicodeException e) -> do
    --  postLog db $ "Unicode exception: " ++ e
    --  processLoop db ws s h


processMessage :: Text -> Transaction ()
processMessage msg = do
  let msg' = decode (T.unpack msg) :: Result JSString
  case msg' of
    Ok x -> processResult $ decode (fromJSString x)
    Error x -> throwError x

processResult :: Result Request -> Transaction ()
processResult (Ok x) = processRequest x
processResult (Error x) = throwError $ "Error (processResult): " ++ x


processRequest :: Request -> Transaction ()

processRequest GetServerClientCount = do
  postLog "GetServerClientCount"
  n <- gets (size . clients)
  respond $ ServerClientCount n

processRequest (Ping t) = respond $ Pong t

processRequest GetEnsembleList = do
  postLog $ "GetEnsembleList"
  es <- gets (keys . ensembles)
  respond $ EnsembleList es

processRequest (Authenticate x) = do
  pwd <- gets password
  if x == pwd
    then do
      postLog $ "Authenticate with correct password"
      modifyClient $ \x -> x { authenticated = True }
    else do
      postLog $ "Authenticate with incorrect password"
      modifyClient $ \x -> x { authenticated = False }

processRequest (CreateEnsemble name pwd) = do
  whenNotAuthenticated $ throwError "ignoring CreateEnsemble from non-authenticated client"
  postLog $ "CreateEnsemble " ++ name
  t <- liftIO $ getCurrentTime
  modify' $ createEnsemble name pwd t
  es <- gets (keys . ensembles)
  respondAll $ EnsembleList es
  saveNewEnsembleToDatabase name

processRequest (JoinEnsemble x) = do
  postLog $ "joining ensemble " ++ x
  s <- get
  e <- justOrError (Data.Map.lookup x $ ensembles s) "attempt to join non-existent ensemble"
  now <- liftIO getCurrentTime
  respond $ EnsembleResponse $ NewTempo (E.tempo e) now
  mapM_ respond $ fmap EnsembleResponse $ Map.mapWithKey ZoneResponse $ E.defs e
  respond $ EnsembleResponse $ DefaultView $ E.defaultView e
  mapM_ respond $ fmap EnsembleResponse $ Map.mapWithKey View $ E.views e
  modifyClient $ \c -> c { ensemble = Just x }
  modifyClient $ \c -> c { authenticatedInEnsemble = E.password e == "" }

processRequest LeaveEnsemble = do
  postLog $ "leaving ensemble"
  modifyClient $ \c -> c { ensemble = Nothing, authenticatedInEnsemble = False }

processRequest (EnsembleRequest x) = processEnsembleRequest x

processEnsembleRequest :: EnsembleRequest -> Transaction ()

processEnsembleRequest (AuthenticateInEnsemble p2) = do
  e <- getEnsemble
  let p1 = E.password e
  when (p1 /= p2 && p2 /= "") $ do
    modifyClient $ \c -> c { authenticatedInEnsemble = False }
    throwError $ "failed AuthenticateInEnsemble"
  -- otherwise...
  postLog $ "successful AuthenticateInEnsemble"
  modifyClient $ \c -> c { authenticatedInEnsemble = True }

processEnsembleRequest (SendChat name msg) = do
  whenNotAuthenticatedInEnsemble $ throwError "ignoring SendChat from client not authenticated in ensemble"
  eName <- getEnsembleName
  postLog $ "SendChat in " ++ eName ++ " from " ++ name ++ ": " ++ msg
  respondEnsemble $ EnsembleResponse (Chat name msg)

processEnsembleRequest (ZoneRequest zone value) = do
  whenNotAuthenticatedInEnsemble $ throwError "ignoring ZoneRequest from client not authenticated in ensemble"
  eName <- getEnsembleName
  postLog $ "Edit in (" ++ eName ++ "," ++ (show zone) ++ "): " ++ (show value)
  modify' $ edit eName zone value
  respondEnsembleNoOrigin $ EnsembleResponse $ ZoneResponse zone value
  saveEnsembleToDatabase

processEnsembleRequest ListViews = do
  eName <- getEnsembleName
  postLog $ "ListViews in " ++ eName
  vs <- (Map.keys . E.views) <$> getEnsemble
  respond $ EnsembleResponse $ ViewList vs

processEnsembleRequest (GetView v) = do
  eName <- getEnsembleName
  postLog $ "GetView " ++ v ++ " in ensemble " ++ eName
  e <- getEnsemble
  v' <- justOrError (Data.Map.lookup v $ E.views e) $ "attempt to get unknown view in " ++ eName
  respond $ EnsembleResponse $ View v v'

processEnsembleRequest (PublishView key value) = do
  whenNotAuthenticatedInEnsemble $ throwError "ignoring PublishView from client not authenticated in ensemble"
  eName <- getEnsembleName
  postLog $ "PublishView in (" ++ eName ++ "," ++ key ++ "): " ++ (show value)
  modify' $ setView eName key value
  saveEnsembleToDatabase

processEnsembleRequest (PublishDefaultView v) = do
  whenNotAuthenticatedInEnsemble $ throwError "ignoring PublishDefaultView from client not authenticated in ensemble"
  eName <- getEnsembleName
  postLog $ "PublishDefaultView in " ++ eName
  modify' $ setDefaultView eName v
  saveEnsembleToDatabase

processEnsembleRequest (DeleteView x) = do
  whenNotAuthenticatedInEnsemble $ throwError "ignoring DeleteView from client not authenticated in ensemble"
  eName <- getEnsembleName
  postLog $ "DeleteView " ++ x ++ " in ensemble " ++ eName
  modify' $ deleteView eName x
  saveEnsembleToDatabase

processEnsembleRequest (SetTempo t) = do
  whenNotAuthenticatedInEnsemble $ throwError "ignoring SetTempo from client not authenticated in ensemble"
  eName <- getEnsembleName
  postLog $ "TempoChange in " ++ eName
  now <- liftIO $ getCurrentTime
  let t' = Tempo {
    cps = cps t,
    at = addUTCTime (-0.075) now, -- TODO: should use Cristian's algorithm, but for now assume set 75 msec ago
    beat = beat t
  }
  modify' $ tempoChangeInEnsemble eName t'
  respondEnsembleNoOrigin $ EnsembleResponse $ NewTempo t' now
  saveEnsembleToDatabase

processEnsembleRequest GetEnsembleClientCount = do
  eName <- getEnsembleName
  x <- gets (Map.size . Map.filter (\c -> ensemble c == Just eName) . clients)
  respond $ EnsembleResponse $ EnsembleClientCount x
