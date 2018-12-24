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
import Estuary.Types.Transaction

runServerWithDatabase :: String -> Int -> SQLite.Connection -> IO ()
runServerWithDatabase pswd port db = do
  putStrLn $ "Estuary collaborative editing server, listening on port " ++ (show port)
  putStrLn $ "password: " ++ pswd
  es <- readEnsembles db
  postLog db $ (show (size es)) ++ " ensembles restored from database"
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
webSocketsApp db s ws = do
  ws' <- try $ WS.acceptRequest ws
  case ws' of
    Right ws'' -> do
      ss <- takeMVar s
      let (h,ss') = addClient ss ws''
      let cc = connectionCount ss' + 1
      let ss'' = ss' { connectionCount=cc }
      putMVar s ss''
      postLog db $ "received new connection (" ++ (show cc) ++ " connections since launch)"
      (WS.forkPingThread ws'' 30) `catch`
        \(SomeException e) -> postLog db $ "exception in forking ping thread: " ++ (show e)
      (getServerClientCount s >>= respondAll s . ServerClientCount) `catch`
        \(SomeException e) -> postLog db $ "exception during transmission of serverClientCount: " ++ (show e)
      processLoop db ws'' s h
    Left (SomeException e) -> do
      postLog db $ "exception during WS.acceptRequest: " ++ (show e)


processLoop :: SQLite.Connection -> WS.Connection -> MVar Server -> ClientHandle -> IO ()
processLoop db ws sMvar cHandle = do
  m <- try $ WS.receiveData ws
  case m of
    Right m' -> do
      s <- takeMVar sMVar
      s' <- runTransaction (processMessage m') db cHandle s
      putMVar sMVar s'
      processLoop db ws s h
    Left WS.ConnectionClosed -> do
      s <- takeMVar sMVar
      s' <- runTransaction (close "unexpected loss of connection") db cHandle s
      putMVar sMVar s'
    Left (WS.CloseRequest _ _) -> do
      s <- takeMVar sMVar
      s' <- runTransaction (close "connection closed by request from peer") db cHandle s
      putMVar sMVar s'
    Left (WS.ParseException e) -> do
      postLog db $ "parse exception: " ++ e
      processLoop db ws s h
    -- Left (WS.UnicodeException e) -> do
    --  postLog db $ "Unicode exception: " ++ e
    --  processLoop db ws s h


processMessage :: Text -> Transaction ()
processMessage msg = do
  let msg' = decode (T.unpack msg) :: Result JSString
  case msg' of 
    Ok x -> processResult $ decode (fromJSString x)
    Error x'' -> throwError x
  
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
  if x == pwd $
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
  es <- gets getEnsembleList
  respondAll $ EnsembleList es
  saveNewEnsembleToDatabase name

processRequest (JoinEnsemble x) = do
  postLog $ "joining ensemble " ++ x
  s <- get
  e <- fromJustOrError (lookup x $ ensembles s) "attempt to join non-existent ensemble"
  respond $ EnsembleResponse (Sited x (NewTempo (E.tempo e)))
  mapM_ respond $ fmap (EnsembleResponse . Sited x . ZoneResponse) $ Map.mapWithKey Sited $ E.defs e
  respond $ EnsembleResponse (Sited x (DefaultView (E.defaultView e)))
  mapM_ respond $ fmap (EnsembleResponse . Sited x . View) $ Map.mapWithKey Sited $ E.views e 
  modifyClient $ \x -> x { ensemble = Just x }
  modifyClient $ \x -> x { authenticatedInEnsemble = E.password e == "" }

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

processEnsembleRequest (ZoneRequest zone value)) = do
  whenNotAuthenticatedInEnsemble $ throwError "ignoring ZoneRequest from client not authenticated in ensemble"
  eName <- getEnsembleName
  postLog $ "Edit in (" ++ eName ++ "," ++ (show zone) ++ "): " ++ (show value)
  modify' s $ edit eName zone value
  respondEnsembleNoOrigin $ EnsembleResponse $ ZoneResponse zone value
  saveEnsembleToDatabase e

processEnsembleRequest ListViews = do
  eName <- getEnsembleName
  postLog $ "ListViews in " ++ eName
  vs <- (Map.keys . views) <$> getEnsemble
  respond $ EnsembleResponse $ ViewList vs

processEnsembleRequest (GetView v) = do
  eName <- getEnsembleName
  postLog $ "GetView " ++ v ++ " in ensemble " ++ eName
  e <- getEnsemble
  v' <- fromJustOrError (lookup v $ views e) $ "attempt to get unknown view in " ++ eName
  respond $ EnsembleResponse $ View v v'

processEnsembleRequest e (PublishView (Sited key value)) = do
  whenNotAuthenticatedInEnsemble $ throwError "ignoring PublishView from client not authenticated in ensemble"
  postLog $ "PublishView in (" ++ e ++ "," ++ key ++ "): " ++ (show value)
  modify' $ setView e key value -- ??? does modify' exist for MonadError instances?
  saveEnsembleToDatabase e

processEnsembleRequest e (PublishDefaultView v) = do
  whenNotAuthenticatedInEnsemble $ throwError "ignoring PublishDefaultView from client not authenticated in ensemble"
  postLog $ "PublishDefaultView in " ++ e
  modify' $ setDefaultView e v
  saveEnsembleToDatabase e

processEnsembleRequest e (DeleteView x) = do
  whenNotAuthenticatedInEnsemble $ throwError "ignoring DeleteView from client not authenticated in ensemble"
  postLog $ "DeleteView " ++ x ++ " in ensemble " ++ e
  modify' $ deleteView e x
  saveEnsembleToDatabase s e db

processEnsembleRequest db s c e x@(SetTempo t) = do
  whenNotAuthenticatedInEnsemble $ throwError "ignoring SetTempo from client not authenticated in ensemble"
  modify' $ tempoChangeInEnsemble e t
  newTempo <- getTempoInEnsemble s e -- *** this one too
  if isJust newTempo then do
    let newTempo' = fromJust newTempo
    respondAll s $ EnsembleResponse $ Sited e $ NewTempo newTempo'
    postLog db $ "TempoChange in " ++ e
    saveEnsembleToDatabase s e db
  else postLog db $ "attempt to TempoChange in non-existent ensemble " ++ e

