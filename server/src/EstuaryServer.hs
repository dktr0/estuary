{-# LANGUAGE OverloadedStrings #-}
module EstuaryServer where

import Data.List ((\\))
import Data.Maybe (fromMaybe,isJust,fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.State
import Control.Monad.Except

import qualified Database.SQLite.Simple as SQLite
import Data.Aeson
import Data.ByteString.Lazy
import qualified Network.WebSockets as WS
import qualified Network.Wai as WS hiding (requestHeaders)
import qualified Network.Wai.Handler.WebSockets as WS
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings, ssIndices, ssMaxAge)
import Network.HTTP.Types.Status (status200, status301)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Internal
import Network.Wai.Handler.WarpTLS
import Network.TLS (Version(..))
import Network.TLS.Extra.Cipher
import Network.Wai.Middleware.Gzip
import WaiAppStatic.Types (unsafeToPiece,MaxAge(..))
import Data.Time
import Data.Time.Clock.POSIX
import Data.Map
import Data.Maybe
import TextShow

import Estuary.Utility
import Estuary.Types.Name
import Estuary.Types.Definition
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import qualified Estuary.Types.Ensemble as E hiding (ensembleName)
import qualified Estuary.Types.EnsembleS as E
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.View
import Estuary.Types.Client
import Estuary.Types.Participant
import Estuary.Types.ServerState
import Estuary.Types.Database
import Estuary.Types.Tempo
import Estuary.Types.Transaction
import Estuary.Types.Chat

runServerWithDatabase :: Password -> Password -> Int -> Bool -> SQLite.Connection -> IO ()
runServerWithDatabase mpwd cpwd port httpRedirect db = do
  nCap <- getNumCapabilities
  postLogNoHandle db $ "Estuary collaborative editing server"
  postLogNoHandle db $ "max simultaneous Haskell threads: " <> showt nCap
  postLogNoHandle db $ "moderator password: " <> mpwd
  postLogNoHandle db $ "community password: " <> cpwd
  es <- readEnsembles db
  postLogNoHandle db $ showt (size es) <> " ensembles restored from database"
  s <- newServerState mpwd cpwd es
  forkIO $ maintenanceThread db s
  let settings = (defaultWebAppSettings "Estuary.jsexe") {
    ssIndices = [unsafeToPiece "index.html"],
    ssMaxAge = MaxAgeSeconds 30 -- 30 seconds max cache time
    }
  postLogNoHandle db $ "listening on port " <> showt port <> " (HTTPS only)"
  when httpRedirect $ void $ forkIO $ do
    postLogNoHandle db $ "(also listening on port 80 and redirecting plain HTTP requests to HTTPS, ie. on port 443)"
    run 80 ourRedirect
  runTLS ourTLSSettings (ourSettings port) $ gzipMiddleware $ WS.websocketsOr WS.defaultConnectionOptions (webSocketsApp db s) (staticApp settings)

maintenanceThread :: SQLite.Connection -> ServerState -> IO ()
maintenanceThread db ss = do
  nClients <- atomically $ fmap IntMap.size $ readTVar $ clients ss
  es <- atomically $ readTVar $ ensembles ss
  sequence $ Map.mapWithKey (deleteEnsembleIfExpired db ss) es
  nEnsembles <- writeAllEnsembles db ss
  postLogNoHandle db $ "maintenance: " <> showt nClients <> " clients, " <> showt nEnsembles <> " ensembles"
  threadDelay $ 900000000 -- maintenance runs every 15 minutes
  maintenanceThread db ss

deleteEnsembleIfExpired :: SQLite.Connection -> ServerState -> Text -> TVar E.EnsembleS -> IO ()
deleteEnsembleIfExpired db ss eName etv = do
  e <- atomically $ readTVar etv
  case E.expiry e of
    Nothing -> return ()
    Just x -> do
      t0 <- atomically $ readTVar $ E.lastActionTime e
      t1 <- getCurrentTime
      let elapsed = diffUTCTime t1 t0
      when (elapsed >= x) $ do
        Estuary.Types.ServerState.deleteEnsemble ss eName
        Estuary.Types.Database.deleteEnsemble db eName
        postLogNoHandle db $ "maintenance: deleting expired ensembles " <> eName

ourRedirect :: WS.Application
ourRedirect req respond = do
  let location = "https://" <> (maybe "localhost" id $ WS.requestHeaderHost req) <> "/" <> WS.rawPathInfo req
  respond $ WS.responseLBS status301 [("Content-Type","text/plain"),("Location",location)] "Redirect"

ourTLSSettings :: TLSSettings
ourTLSSettings = defaultTlsSettings {
  certFile = "../cert.pem",
  keyFile = "../privkey.pem",
  tlsAllowedVersions = [TLS12],
  tlsCiphers = [
    cipher_ECDHE_ECDSA_AES256GCM_SHA384,
    cipher_ECDHE_RSA_AES256GCM_SHA384,
    cipher_ECDHE_ECDSA_AES128GCM_SHA256,
    cipher_ECDHE_RSA_AES128GCM_SHA256
    ],
  onInsecure = DenyInsecure "You must use HTTPS to connect to Estuary. Try using the same URL in your browser but with https instead of http at the beginning."
  }

ourSettings :: Port -> Settings
ourSettings port = defaultSettings {
  settingsPort = port,
  settingsFdCacheDuration = 30,
  settingsFileInfoCacheDuration = 30
  }

gzipMiddleware :: WS.Middleware
gzipMiddleware = gzip $ def {
  gzipFiles = GzipPreCompressed GzipIgnore
}

webSocketsApp :: SQLite.Connection -> ServerState -> WS.PendingConnection -> IO ()
webSocketsApp db ss pc = do
  ws' <- try $ WS.acceptRequest pc
  case ws' of
    Right ws'' -> do
      let rh = WS.requestHeaders $ WS.pendingRequest pc
      let rhMap = Map.fromList rh
      let ip = Map.findWithDefault "unknown IP address" "Host" rhMap
      (cHandle,ctvar) <- addClient ss ws''
      postLog db cHandle $ "new connection from " <> showt ip
      postLog db cHandle $ "request headers: " <> T.pack (show rhMap)
      (WS.forkPingThread ws'' 10) `catch` \(SomeException e) -> postLog db cHandle $ "exception forking ping thread: " <> (T.pack $ show e)
      processLoop db ss cHandle ctvar ws''
    Left (SomeException e) -> do
      postLogNoHandle db $ "exception during WS.acceptRequest: " <> (T.pack $ show e)


processLoop :: SQLite.Connection -> ServerState -> ClientHandle -> TVar Client -> WS.Connection -> IO ()
processLoop db ss cHandle ctvar ws = do
  m <- try $ WS.receiveData ws
  case m of
    Right m' -> do
      processMessage db ss ws cHandle ctvar m'
      processLoop db ss cHandle ctvar ws
    Left (WS.ParseException e) -> do
      postLog db cHandle $ "parse exception: " <> (T.pack e)
      processLoop db ss cHandle ctvar ws
    Left WS.ConnectionClosed -> close db ss cHandle ctvar "unexpected loss of connection"
    Left (WS.CloseRequest _ _) -> close db ss cHandle ctvar "by request from peer"
    Left _ -> close db ss cHandle ctvar "* unknown exception in receiveData"


close :: SQLite.Connection -> ServerState -> ClientHandle -> TVar Client -> Text -> IO ()
close db ss cHandle ctvar msg = do
  postLog db cHandle $ "closing connection: " <> msg
  x <- runTransaction ss $ do
    c <- liftSTM $ readTVar ctvar
    deleteClient cHandle
    return c
  case x of
    Left e -> postLog db cHandle $ "*error closing connection* " <> e
    Right c -> do
      postLog db cHandle $ "(client deleted)"
      case memberOfEnsemble c of
        Nothing -> return ()
        Just etvar -> do
          e <- readTVarIO etvar
          let eName = E.ensembleName e
          case handleInEnsemble c of
            "" -> do
              n <- readTVarIO $ E.anonymousConnections e
              sendEnsembleNoOrigin db cHandle e $ EnsembleResponse $ AnonymousParticipants n
              postLog db cHandle $ "(anonymous) leaves ensemble " <> eName <> " (close completed)"
            h -> do
              sendEnsembleNoOrigin db cHandle e $ EnsembleResponse $ ParticipantLeaves h
              postLog db cHandle $ h <> " leaves ensemble " <> eName <> " (close completed)"


processMessage :: SQLite.Connection -> ServerState -> WS.Connection -> ClientHandle -> TVar Client -> ByteString -> IO ()
processMessage db ss ws cHandle ctvar msg = case eitherDecode msg of
  Right x -> processRequest db ss ws cHandle ctvar x
  Left x -> postLog db cHandle $ "* eitherDecode error in processMessage: " <> (T.pack x)

processRequest :: SQLite.Connection -> ServerState -> WS.Connection -> ClientHandle -> TVar Client -> Request -> IO ()

processRequest db ss ws cHandle ctvar (BrowserInfo t) = atomically $ modifyTVar ctvar $ \c -> c { browserInfo = t }

processRequest db ss ws cHandle ctvar (ClientInfo load animationFPS animationLoad latency pingTime) = do
  (n,c) <- atomically $ do
    modifyTVar ctvar $ \c -> c {
      clientMainLoad = load,
      clientAnimationFPS = animationFPS,
      clientAnimationLoad = animationLoad,
      clientLatency = latency
      }
    n <- readTVar $ clientCount ss
    c <- readTVar ctvar
    return (n,c)
  send db cHandle cHandle ws $ ServerInfo n pingTime
  case memberOfEnsemble c of
    Just etvar -> do
      e <- readTVarIO etvar
      when (handleInEnsemble c /= "") $ sendEnsemble db cHandle e $ EnsembleResponse $ ParticipantUpdate $ clientToParticipant c
    Nothing -> return ()

processRequest db ss ws cHandle ctvar GetEnsembleList = do
  eNames <- atomically $ Map.keys <$> readTVar (ensembles ss)
  send db cHandle cHandle ws $ EnsembleList eNames

processRequest db ss ws cHandle ctvar (CreateEnsemble cpwd name opwd jpwd expTime) = do
  now <- getCurrentTime
  x <- runTransaction ss $ createEnsemble cHandle cpwd name opwd jpwd expTime now
  case x of
    Left e -> do
      let m = "unable to CreateEnsemble: " <> e
      sendThisClient db cHandle ws (ResponseError m)
      postLog db cHandle $ "*CreateEnsemble* " <> m
    Right _ -> do
      let m = "created ensemble " <> name <> " host password=" <> opwd <> " participant password=" <> jpwd
      sendThisClient db cHandle ws (ResponseOK m)
      postLog db cHandle m

processRequest db ss ws cHandle ctvar (DeleteThisEnsemble opwd) = do
  x <- runTransaction ss $ deleteThisEnsemble ctvar opwd
  case x of
    Left e -> do
      let m = "unable to delete this ensemble: " <> e
      sendThisClient db cHandle ws (ResponseError m)
      postLog db cHandle $ "*DeleteThisEnsemble* " <> m
    Right eName -> do
      let m = "deleted this ensemble " <> eName
      sendThisClient db cHandle ws (ResponseOK m)
      postLog db cHandle m

processRequest db ss ws cHandle ctvar (DeleteEnsemble eName mpwd) = do
  x <- runTransaction ss $ Estuary.Types.Transaction.deleteEnsemble cHandle eName mpwd
  case x of
    Left e -> do
      let m = "unable to delete ensemble " <> eName <> ": " <> e
      sendThisClient db cHandle ws (ResponseError m)
      postLog db cHandle $ "*DeleteEnsemble* " <> m
    Right _ -> do
      let m = "deleted ensemble " <> eName
      sendThisClient db cHandle ws (ResponseOK m)
      postLog db cHandle m

processRequest db ss ws cHandle ctvar (JoinEnsemble eName uName loc pwd) = do
  x <- runTransactionIO ss $ joinEnsemble db cHandle ctvar eName uName loc pwd
  let uName' = if uName == "" then "(anonymous)" else uName
  case x of
    Left e -> do
      let m = "unable to join ensemble " <> eName <> " as " <> uName' <> ": " <> e
      sendThisClient db cHandle ws (ResponseError m)
      postLog db cHandle m
    Right _ -> postLog db cHandle $ "joined ensemble " <> eName <> " as " <> uName'

processRequest db ss ws cHandle ctvar LeaveEnsemble = do
  runTransactionIOLogged db ss cHandle "LeaveEnsemble" $ leaveEnsemble db ctvar

processRequest db ss ws cHandle ctvar (EnsembleRequest x) = processEnsembleRequest db ss ws cHandle ctvar x

processEnsembleRequest :: SQLite.Connection -> ServerState -> WS.Connection -> ClientHandle -> TVar Client -> EnsembleRequest -> IO ()

processEnsembleRequest db ss ws cHandle ctvar (WriteZone zone value) = do
  now <- getCurrentTime
  x <- runTransaction ss $ do
    writeZone now ctvar zone value
    updateLastEdit ctvar now
    getClientEnsemble ctvar
  case x of
    Left err -> do
      let m = "*WriteZone* " <> err
      sendThisClient db cHandle ws (ResponseError m)
      postLog db cHandle m
    Right e -> sendEnsembleNoOrigin db cHandle e $ EnsembleResponse $ ZoneRcvd zone value

processEnsembleRequest db ss ws cHandle ctvar (WriteChat msg) = do
  now <- getCurrentTime
  x <- runTransaction ss $ do
    whenNotAuthenticatedInEnsemble ctvar $ throwError "not authenticated"
    uName <- liftSTM $ handleInEnsemble <$> readTVar ctvar
    when (uName == "") $ throwError "ignoring anonymous chat"
    updateLastEdit ctvar now
    e <- getClientEnsemble ctvar
    return (e,uName)
  case x of
    Left err -> do
      let m = "*WriteChat* " <> err
      sendThisClient db cHandle ws (ResponseError m)
      postLog db cHandle m
    Right (e,uName) -> do
      postLog db cHandle $ uName <> " in " <> E.ensembleName e <> " chats: " <> msg
      sendEnsemble db cHandle e $ EnsembleResponse $ ChatRcvd $ Chat now uName msg

processEnsembleRequest db ss ws cHandle ctvar (WriteStatus msg) = do
  now <- getCurrentTime
  x <- runTransaction ss $ do
    whenNotAuthenticatedInEnsemble ctvar $ throwError "not authenticated"
    uName <- liftSTM $ handleInEnsemble <$> readTVar ctvar
    when (uName == "") $ throwError "ignoring anonymous status update"
    liftSTM $ modifyTVar ctvar $ \c -> c { statusInEnsemble = msg }
    p <- updateLastEdit ctvar now
    e <- getClientEnsemble ctvar
    return (p,e,uName)
  case x of
    Left err -> do
      let m = "*WriteStatus* " <> err
      sendThisClient db cHandle ws (ResponseError m)
      postLog db cHandle m
    Right (p,e,uName) -> do
      postLog db cHandle $ "WriteStatus from " <> uName <> " in " <> E.ensembleName e <> ": " <> msg
      sendEnsembleNoOrigin db cHandle e $ EnsembleResponse $ ParticipantUpdate p

processEnsembleRequest db ss ws cHandle ctvar (WriteView preset view) = do
  now <- getCurrentTime
  x <- runTransaction ss $ do
    when (not $ nameIsLegal preset) $ throwError "view name cannot contain spaces/newlines/control characters"
    writeView now ctvar preset view
    updateLastEdit ctvar now
    getClientEnsemble ctvar
  case x of
    Left err -> do
      let m = "*PublishView* " <> err
      sendThisClient db cHandle ws (ResponseError m)
      postLog db cHandle m
    Right e -> do
      sendThisClient db cHandle ws (ResponseOK $ "published view " <> preset)
      sendEnsembleNoOrigin db cHandle e $ EnsembleResponse $ ViewRcvd preset view
      postLog db cHandle $ "WriteView in (" <> E.ensembleName e <> "," <> preset <> ")"

processEnsembleRequest db ss ws cHandle ctvar (WriteTempo t) = do
  now <- getCurrentTime
  x <- runTransaction ss $ do
    writeTempo now ctvar t
    updateLastEdit ctvar now
    getClientEnsemble ctvar
  case x of
    Left err -> do
      let m = "*WriteTempo* " <> err
      sendThisClient db cHandle ws (ResponseError m)
      postLog db cHandle m
    Right e -> do
      sendThisClient db cHandle ws (ResponseOK $ "setting tempo succeeded")
      sendEnsembleNoOrigin db cHandle e $ EnsembleResponse $ TempoRcvd t
      postLog db cHandle $ "WriteTempo in " <> E.ensembleName e
