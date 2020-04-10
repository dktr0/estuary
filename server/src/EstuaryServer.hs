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
import TextShow

import Estuary.Utility
import Estuary.Types.Name
import Estuary.Types.Definition
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import qualified Estuary.Types.Ensemble as E
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
      cHandle <- addClient ss ws''
      postLog db cHandle $ "new connection from " <> showt ip
      (WS.forkPingThread ws'' 10) `catch` \(SomeException e) -> postLog db cHandle $ "exception forking ping thread: " <> (T.pack $ show e)
      processLoop db ss cHandle ws''
    Left (SomeException e) -> do
      postLogNoHandle db $ "exception during WS.acceptRequest: " <> (T.pack $ show e)


processLoop :: SQLite.Connection -> ServerState -> ClientHandle -> WS.Connection -> IO ()
processLoop db ss cHandle ws = do
  m <- try $ WS.receiveData ws
  case m of
    Right m' -> do
      processMessage db ss ws cHandle m'
      processLoop db ss cHandle ws
    Left (WS.ParseException e) -> do
      postLog db cHandle $ "parse exception: " <> (T.pack e)
      processLoop db ss cHandle ws
    Left WS.ConnectionClosed -> close db ss cHandle "unexpected loss of connection"
    Left (WS.CloseRequest _ _) -> close db ss cHandle "by request from peer"
    Left _ -> close db ss cHandle "* unknown exception in receiveData"


close :: SQLite.Connection -> ServerState -> ClientHandle -> Text -> IO ()
close db ss cHandle msg = do
  x <- runTransaction ss $ do
    c <- getClient cHandle
    n <- countAnonymousParticipants cHandle
    deleteClient cHandle
    return (c,n)
  case x of
    Left e -> postLog db cHandle $ "*close* " <> e
    Right (c,n) -> do
      postLog db cHandle $ "closing connection: " <> msg
      when (isJust $ memberOfEnsemble c) $ do
        let eName = fromJust $ memberOfEnsemble c
        let uName = handleInEnsemble c
        let uName' = if uName == "" then "(anonymous)" else uName
        postLog db cHandle $ uName' <> " leaves ensemble " <> eName
        let r = if uName == "" then AnonymousParticipants n else ParticipantLeaves uName
        sendEnsembleNoOrigin db ss cHandle eName (EnsembleResponse r)


processMessage :: SQLite.Connection -> ServerState -> WS.Connection -> ClientHandle -> ByteString -> IO ()
processMessage db ss ws cHandle msg = case eitherDecode msg of
  Right x -> processRequest db ss ws cHandle x
  Left x -> postLog db cHandle $ "* eitherDecode error in processMessage: " <> (T.pack x)

processRequest :: SQLite.Connection -> ServerState -> WS.Connection -> ClientHandle -> Request -> IO ()

processRequest db ss ws cHandle (BrowserInfo t) = do
  x <- runTransaction ss $ modifyClient cHandle $ \c -> c { browserInfo = t }
  case x of
    Left e -> postLog db cHandle $ "*BrowserInfo* " <> e
    Right () -> return ()

processRequest db ss ws cHandle (ClientInfo pingTime load animationLoad latency) = do
  x <- runTransaction ss $ do
    modifyClient cHandle $ \c -> c {
      clientMainLoad = load,
      clientAnimationLoad = animationLoad,
      clientLatency = latency
      }
    n <- getServerClientCount
    self <- getClient cHandle
    return (n,self)
  case x of
    Left e -> postLog db cHandle $ "*ClientInfo* " <> e
    Right (n,self) -> do
      send db cHandle cHandle ws $ ServerInfo n pingTime
      case memberOfEnsemble self of
        Just eName -> sendEnsemble db ss cHandle eName $ EnsembleResponse $ ParticipantUpdate (handleInEnsemble self) (clientToParticipant self)
        Nothing -> return ()

processRequest db ss ws cHandle GetEnsembleList = do
  x <- runTransaction ss $ getEnsembleNames
  case x of
    Left e -> do
      let m = "unable to GetEnsembleList: " <> e
      sendThisClient db cHandle ws (ResponseError m)
      postLog db cHandle $ "*GetEnsembleList* " <> m
    Right eNames -> send db cHandle cHandle ws $ EnsembleList eNames

processRequest db ss ws cHandle (CreateEnsemble cpwd name opwd jpwd expTime) = do
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

processRequest db ss ws cHandle (DeleteThisEnsemble opwd) = do
  x <- runTransaction ss $ deleteThisEnsemble cHandle opwd
  case x of
    Left e -> do
      let m = "unable to delete this ensemble: " <> e
      sendThisClient db cHandle ws (ResponseError m)
      postLog db cHandle $ "*DeleteThisEnsemble* " <> m
    Right eName -> do
      let m = "deleted this ensemble " <> eName
      sendThisClient db cHandle ws (ResponseOK m)
      postLog db cHandle m

processRequest db ss ws cHandle (DeleteEnsemble eName mpwd) = do
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

processRequest db ss ws cHandle (JoinEnsemble eName uName loc pwd) = do
  x <- runTransactionIO ss $ joinEnsemble db cHandle eName uName loc pwd
  let uName' = if uName == "" then "(anonymous)" else uName
  case x of
    Left e -> do
      let m = "unable to join ensemble " <> eName <> " as " <> uName' <> ": " <> e
      sendThisClient db cHandle ws (ResponseError m)
      postLog db cHandle m
    Right _ -> postLog db cHandle $ "joined ensemble " <> eName <> " as " <> uName'

processRequest db ss ws cHandle LeaveEnsemble = do
  runTransactionIOLogged db ss cHandle "LeaveEnsemble" $ leaveEnsemble db cHandle

processRequest db ss ws cHandle (EnsembleRequest x) = processEnsembleRequest db ss ws cHandle x

processEnsembleRequest :: SQLite.Connection -> ServerState -> WS.Connection -> ClientHandle -> EnsembleRequest -> IO ()

processEnsembleRequest db ss ws cHandle (WriteZone zone value) = do
  now <- getCurrentTime
  x <- runTransaction ss $ do
    writeZone now cHandle zone value
    p <- updateLastEdit cHandle now
    eName <- getEnsembleName cHandle
    return (p,eName)
  case x of
    Left e -> postLog db cHandle $ "*WriteZone* " <> e
    Right (p,eName) -> do
      sendEnsembleNoOrigin db ss cHandle eName $ EnsembleResponse $ ZoneRcvd zone value
      sendEnsemble db ss cHandle eName $ EnsembleResponse $ ParticipantUpdate (name p) p

processEnsembleRequest db ss ws cHandle (WriteChat msg) = do
  now <- getCurrentTime
  x <- runTransaction ss $ do
    whenNotAuthenticatedInEnsemble cHandle $ throwError "not authenticated"
    eName <- getEnsembleName cHandle
    uName <- handleInEnsemble <$> getClient cHandle
    when (uName == "") $ throwError "ignoring from anonymous"
    p <- updateLastEdit cHandle now
    return (p,eName,uName)
  case x of
    Left e -> postLog db cHandle $ "*WriteChat* " <> e
    Right (p,eName,uName) -> do
      postLog db cHandle $ uName <> " in " <> eName <> " chats: " <> msg
      sendEnsemble db ss cHandle eName $ EnsembleResponse $ ChatRcvd $ Chat now uName msg
      sendEnsemble db ss cHandle eName $ EnsembleResponse $ ParticipantUpdate (name p) p

processEnsembleRequest db ss ws cHandle (WriteStatus msg) = do
  now <- getCurrentTime
  x <- runTransaction ss $ do
    whenNotAuthenticatedInEnsemble cHandle $ throwError "not authenticated"
    eName <- getEnsembleName cHandle
    uName <- handleInEnsemble <$> getClient cHandle
    when (uName == "") $ throwError "ignoring from anonymous"
    modifyClient cHandle $ \c -> c { statusInEnsemble = msg }
    p <- updateLastEdit cHandle now
    return (p,eName,uName)
  case x of
    Left e -> postLog db cHandle $ "*WriteStatus* " <> e
    Right (p,eName,uName) -> do
      postLog db cHandle $ "WriteStatus from " <> uName <> " in " <> eName <> ": " <> msg
      sendEnsemble db ss cHandle eName $ EnsembleResponse $ ParticipantUpdate (name p) p

processEnsembleRequest db ss ws cHandle (WriteView preset view) = do
  now <- getCurrentTime
  x <- runTransaction ss $ do
    writeView now cHandle preset view
    eName <- getEnsembleName cHandle
    p <- updateLastEdit cHandle now
    return (p,eName)
  case x of
    Left e -> postLog db cHandle $ "*WriteView* " <> e
    Right (p,eName) -> do
      postLog db cHandle $ "WriteView in (" <> eName <> "," <> preset <> ")"
      sendEnsembleNoOrigin db ss cHandle eName $ EnsembleResponse $ ViewRcvd preset view
      sendEnsemble db ss cHandle eName $ EnsembleResponse $ ParticipantUpdate (name p) p

processEnsembleRequest db ss ws cHandle (WriteTempo t) = do
  now <- getCurrentTime
  x <- runTransaction ss $ do
    writeTempo now cHandle t
    eName <- getEnsembleName cHandle
    p <- updateLastEdit cHandle now
    return (p,eName)
  case x of
    Left e -> postLog db cHandle $ "*WriteTempo* " <> e
    Right (p,eName) -> do
      postLog db cHandle $ "WriteTempo in " <> eName
      sendEnsembleNoOrigin db ss cHandle eName $ EnsembleResponse $ TempoRcvd t
      sendEnsemble db ss cHandle eName $ EnsembleResponse $ ParticipantUpdate (name p) p
