{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
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
import Control.Concurrent.Async (race)
import Control.Concurrent.STM hiding (atomically,readTVarIO)
import qualified Control.Concurrent.STM as STM
import Control.Exception
import Control.Monad.State
import Control.Monad.Except

import qualified Database.SQLite.Simple as SQLite
import Data.Aeson
import Data.ByteString.Lazy
import Network.Socket (SockAddr)
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
import qualified Estuary.Types.Ensemble as E hiding (ensembleName)
import qualified Estuary.Types.EnsembleS as E
import Estuary.Types.Request as Request
import Estuary.Types.Response as Response
import Estuary.Types.View
import Estuary.Types.Client
import Estuary.Types.Participant
import Estuary.Types.ServerState
import Estuary.Types.Database
import Estuary.Types.Tempo
import Estuary.Types.Transaction
import Estuary.Types.Chat
import Estuary.AtomicallyTimed
import Estuary.Types.LogEntry
import Estuary.Types.TranslatableText
import qualified System.Posix.Resource as Resource

increaseOpenFilesLimit :: IO ()
increaseOpenFilesLimit = do
  initialLimits <- Resource.getResourceLimit Resource.ResourceOpenFiles
  let initialSoft = Resource.softLimit initialLimits
  let initialHard = Resource.hardLimit initialLimits
  postLogNoHandle $ "initial soft limit of open files: " <> T.pack (showResourceLimit initialSoft)
  postLogNoHandle $ "initial hard limit of open files: " <> T.pack (showResourceLimit initialHard)
  case initialSoft of
    Resource.ResourceLimit n -> do
      when (n < 16384) $ do
        postLogNoHandle " attempting to increase soft limit to 16384..."
        Resource.setResourceLimit Resource.ResourceOpenFiles (initialLimits { Resource.softLimit = Resource.ResourceLimit 16384} )
        newLimits <- Resource.getResourceLimit Resource.ResourceOpenFiles
        let newSoft = Resource.softLimit newLimits
        postLogNoHandle $ "new soft limit of open files: " <> T.pack (showResourceLimit $ Resource.softLimit  newLimits)
    _ -> return ()

showResourceLimit :: Resource.ResourceLimit -> String
showResourceLimit Resource.ResourceLimitInfinity = "infinite"
showResourceLimit Resource.ResourceLimitUnknown = "unknown"
showResourceLimit (Resource.ResourceLimit n) = show n

runServerWithDatabase :: Password -> Password -> Int -> Bool -> SQLite.Connection -> IO ()
runServerWithDatabase mpwd cpwd port httpRedirect db = do
  nCap <- getNumCapabilities
  postLogNoHandle $ "Estuary collaborative editing server"
  increaseOpenFilesLimit
  postLogNoHandle $ "max simultaneous Haskell threads: " <> showt nCap
  postLogNoHandle $ "moderator password: " <> mpwd
  postLogNoHandle $ "community password: " <> cpwd
  es <- readEnsembles db
  postLogNoHandle $ showt (size es) <> " ensembles restored from database"
  s <- newServerState mpwd cpwd es
  forkIO $ maintenanceThread db s
  let settings = (defaultWebAppSettings "Estuary.jsexe") {
    ssIndices = [unsafeToPiece "index.html"],
    ssMaxAge = MaxAgeSeconds 30 -- 30 seconds max cache time
    }
  postLogNoHandle $ "listening on port " <> showt port <> " (HTTPS only)"
  when httpRedirect $ void $ forkIO $ do
    postLogNoHandle $ "(also listening on port 80 and redirecting plain HTTP requests to HTTPS, ie. on port 443)"
    run 80 ourRedirect
  runTLS ourTLSSettings (ourSettings port) $ gzipMiddleware $ websocketsOrWithSockAddr WS.defaultConnectionOptions (webSocketsApp db s) (staticApp settings)

websocketsOrWithSockAddr :: WS.ConnectionOptions -> (SockAddr -> WS.ServerApp) -> WS.Application -> WS.Application
websocketsOrWithSockAddr opts app backup req sendResponse =
  case WS.websocketsApp opts (app $ WS.remoteHost req) req of
    Nothing -> backup req sendResponse
    Just res -> sendResponse res

maintenanceThread :: SQLite.Connection -> ServerState -> IO ()
maintenanceThread db ss = do
  nClients <- $atomically $ fmap IntMap.size $ readTVar $ clients ss
  es <- $atomically $ readTVar $ ensembles ss
  sequence $ Map.mapWithKey (deleteEnsembleIfExpired db ss) es
  nEnsembles <- writeAllEnsembles db ss
  postLogNoHandle $ "maintenance: " <> showt nClients <> " clients, " <> showt nEnsembles <> " ensembles"
  threadDelay $ 300000000 -- maintenance runs every 5 minutes
  maintenanceThread db ss

deleteEnsembleIfExpired :: SQLite.Connection -> ServerState -> Text -> TVar E.EnsembleS -> IO ()
deleteEnsembleIfExpired db ss eName etv = do
  e <- $atomically $ readTVar etv
  case E.expiry e of
    Nothing -> return ()
    Just x -> do
      t0 <- $atomically $ readTVar $ E.lastActionTime e
      t1 <- getCurrentTime
      let elapsed = diffUTCTime t1 t0
      when (elapsed >= x) $ do
        Estuary.Types.ServerState.deleteEnsemble ss eName
        Estuary.Types.Database.deleteEnsemble db eName
        postLogNoHandle $ "maintenance: deleting expired ensembles " <> eName

ourRedirect :: WS.Application
ourRedirect req respond = do
  let location = "https://" <> (maybe "localhost" id $ WS.requestHeaderHost req) <> "/" <> WS.rawPathInfo req
  respond $ WS.responseLBS status301 [("Content-Type","text/plain"),("Location",location)] "Redirect"

ourTLSSettings :: TLSSettings
ourTLSSettings = (tlsSettings "../cert.pem" "../privkey.pem") {
-- defaultTlsSettings {
--  certFile = "../cert.pem",
--  keyFile = "../privkey.pem",
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

webSocketsApp :: SQLite.Connection -> ServerState -> SockAddr -> WS.PendingConnection -> IO ()
webSocketsApp db ss sockAddr pc = do
  ws' <- try $ WS.acceptRequest pc
  case ws' of
    Right ws'' -> do
      (cHandle,ctvar,sChan) <- addClient sockAddr ss ws''
      postLog cHandle $ "new connection from " <> T.pack (show sockAddr)
      WS.withPingThread ws'' 10 (pure ()) $ do
        race (sendThread ss cHandle sChan ws'') (receiveThread db ss cHandle ctvar ws'')
      postLog cHandle $ "thread finished."
    Left (SomeException e) -> do
      postLogNoHandle $ "exception during WS.acceptRequest: " <> (T.pack $ show e)

sendThread :: ServerState -> ClientHandle -> TChan (ClientHandle,Response) -> WS.Connection -> IO ()
sendThread ss cHandle sChan ws = do
  (originHandle,r) <- STM.atomically $ readTChan sChan
  x <- try $ send originHandle cHandle ws r
  case x of
    Right _ -> sendThread ss cHandle sChan ws
    Left (SomeException e) -> do
      let ce = fromException (SomeException e)
      case ce of
        Just (WS.CloseRequest _ _) -> removeAnotherClient ss originHandle cHandle $ "CloseRequest during send"
        Just WS.ConnectionClosed -> removeAnotherClient ss originHandle cHandle $ "ConnectionClosed during send"
        otherwise -> removeAnotherClient ss originHandle cHandle $ "unusual exception during send: " <> (T.pack $ show e)

send :: ClientHandle -> ClientHandle -> WS.Connection -> Response -> IO ()
send originHandle destHandle c x = do
  warningThread <- forkIO $ do
    threadDelay 10000
    postLog originHandle $ "*** websocket send to (" <> showt destHandle <> ") is taking a long time..."
  t0 <- getCurrentTime
  WS.sendTextData c $ encode x
  t1 <- getCurrentTime
  killThread warningThread
  let diff = diffUTCTime t1 t0
  when (diff > 0.010) $ postLog originHandle $ "*** websocket send to (" <> showt destHandle <> ") took " <> showt (realToFrac diff :: Double) <> " seconds ***"

receiveThread :: SQLite.Connection -> ServerState -> ClientHandle -> TVar Client -> WS.Connection -> IO ()
receiveThread db ss cHandle ctvar ws = do
  m <- try $ WS.receiveData ws
  case m of
    Right m' -> do
      processMessage db ss ws cHandle ctvar m'
      receiveThread db ss cHandle ctvar ws
    Left (WS.ParseException e) -> do
      postLog cHandle $ "parse exception: " <> (T.pack e)
      receiveThread db ss cHandle ctvar ws
    Left WS.ConnectionClosed -> close db ss cHandle ctvar "unexpected loss of connection"
    Left (WS.CloseRequest _ _) -> close db ss cHandle ctvar "by request from peer"
    Left _ -> close db ss cHandle ctvar "* unknown exception in receiveData"


close :: SQLite.Connection -> ServerState -> ClientHandle -> TVar Client -> Text -> IO ()
close db ss cHandle ctvar msg = do
  postLog cHandle $ "closing connection: " <> msg
  x <- runTransaction ss $ do
    c <- liftSTM $ readTVar ctvar
    deleteClient cHandle
    return c
  case x of
    Left e -> postLog cHandle $ "*error closing connection* " <> e
    Right c -> do
      notifyWhenClientDepartsEnsemble ss cHandle c
      postLog cHandle $ "close connection completed."


processMessage :: SQLite.Connection -> ServerState -> WS.Connection -> ClientHandle -> TVar Client -> ByteString -> IO ()
processMessage db ss ws cHandle ctvar msg = case eitherDecode msg of
  Right x -> processRequest db ss ws cHandle ctvar x
  Left x -> postLog cHandle $ "* eitherDecode error in processMessage: " <> (T.pack x)

processRequest :: SQLite.Connection -> ServerState -> WS.Connection -> ClientHandle -> TVar Client -> Request -> IO ()

processRequest db ss ws cHandle ctvar (BrowserInfo t) = $atomically $ modifyTVar ctvar $ \c -> c { browserInfo = t }

processRequest db ss ws cHandle ctvar (ClientInfo load animationFPS animationLoad latency pingTime) = do
  (n,c) <- $atomically $ do
    modifyTVar ctvar $ \c -> c {
      clientMainLoad = load,
      clientAnimationFPS = animationFPS,
      clientAnimationLoad = animationLoad,
      clientLatency = latency
      }
    n <- readTVar $ clientCount ss
    c <- readTVar ctvar
    return (n,c)
  sendThisClient ctvar $ ServerInfo n pingTime
  case memberOfEnsemble c of
    Just etvar -> do
      e <- $readTVarIO etvar
      when (handleInEnsemble c /= "") $ sendEnsemble cHandle e $ ParticipantUpdate $ clientToParticipant c
    Nothing -> return ()

processRequest db ss ws cHandle ctvar GetEnsembleList = do
  eNames <- $atomically $ Map.keys <$> readTVar (ensembles ss)
  sendThisClient ctvar $ EnsembleList eNames

processRequest db ss ws cHandle ctvar (CreateEnsemble cpwd name opwd jpwd expTime) = do
  now <- getCurrentTime
  x <- runTransaction ss $ createEnsemble cHandle cpwd name opwd jpwd expTime now
  case x of
    Left e -> do
      let m = "unable to CreateEnsemble: " <> e
      sendThisClient ctvar (Response.Error m)
      postLog cHandle $ "*CreateEnsemble* " <> m
    Right _ -> do
      let m = "created ensemble " <> name <> " host password=" <> opwd <> " participant password=" <> jpwd
      sendThisClient ctvar (Response.OK m)
      postLog cHandle m

processRequest db ss ws cHandle ctvar (DeleteThisEnsemble opwd) = do
  x <- runTransaction ss $ deleteThisEnsemble ctvar opwd
  case x of
    Left e -> do
      let m = "unable to delete this ensemble: " <> e
      sendThisClient ctvar (Response.Error m)
      postLog cHandle $ "*DeleteThisEnsemble* " <> m
    Right eName -> do
      Estuary.Types.Database.deleteEnsemble db eName
      let m = "deleted this ensemble " <> eName
      sendThisClient ctvar (Response.OK m)
      postLog cHandle m

processRequest db ss ws cHandle ctvar (DeleteEnsemble eName mpwd) = do
  x <- runTransaction ss $ Estuary.Types.Transaction.deleteEnsemble cHandle eName mpwd
  case x of
    Left e -> do
      let m = "unable to delete ensemble " <> eName <> ": " <> e
      sendThisClient ctvar (Response.Error m)
      postLog cHandle $ "*DeleteEnsemble* " <> m
    Right _ -> do
      Estuary.Types.Database.deleteEnsemble db eName
      let m = "deleted ensemble " <> eName
      sendThisClient ctvar (Response.OK m)
      postLog cHandle m

processRequest db ss ws cHandle ctvar (JoinEnsemble eName uName loc pwd) = do
  x <- runTransactionIO ss $ joinEnsemble db cHandle ctvar eName uName loc pwd False
  let uName' = if uName == "" then "(anonymous)" else uName
  case x of
    Left e -> do
      let m = "unable to join ensemble " <> eName <> " as " <> uName' <> ": " <> e
      sendThisClient ctvar (Response.Error m)
      postLog cHandle m
    Right _ -> postLog cHandle $ "joined ensemble " <> eName <> " as " <> uName'

processRequest db ss ws cHandle ctvar (RejoinEnsemble eName uName loc pwd) = do
  x <- runTransactionIO ss $ joinEnsemble db cHandle ctvar eName uName loc pwd True
  let uName' = if uName == "" then "(anonymous)" else uName
  case x of
    Left e -> do
      let m = "unable to rejoin ensemble " <> eName <> " as " <> uName' <> ": " <> e
      sendThisClient ctvar (Response.Error m)
      postLog cHandle m
    Right _ -> postLog cHandle $ "rejoined ensemble " <> eName <> " as " <> uName'

processRequest db ss ws cHandle ctvar LeaveEnsemble = do
  runTransactionIOLogged db ss cHandle "LeaveEnsemble" $ leaveEnsemble db ctvar

processRequest db ss ws cHandle ctvar (Request.WriteZone zone value changesRender) = do
  now <- getCurrentTime
  x <- runTransaction ss $ do
    writeZone now ctvar zone value
    updateLastEdit ctvar now
    getClientEnsemble ctvar
  case x of
    Left err -> do
      let m = "*WriteZone* " <> err
      sendThisClient ctvar (Response.Error m)
      postLog cHandle m
    Right e -> sendEnsembleNoOrigin cHandle e $ Response.WriteZone zone value changesRender

processRequest db ss ws cHandle ctvar (Request.SendChat msg) = do
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
      sendThisClient ctvar (Response.Error m)
      postLog cHandle m
    Right (e,uName) -> do
      postLog cHandle $ uName <> " in " <> E.ensembleName e <> " chats: " <> msg
      let l = LogEntry {
        logEntryTime = now,
        logEntrySender = uName,
        logEntryText = noLanguage msg
        }
      sendEnsemble cHandle e $ Response.EnsembleLog l

processRequest db ss ws cHandle ctvar (Request.WriteStatus msg) = do
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
      sendThisClient ctvar (Response.Error m)
      postLog cHandle m
    Right (p,e,uName) -> do
      postLog cHandle $ "WriteStatus from " <> uName <> " in " <> E.ensembleName e <> ": " <> msg
      sendEnsembleNoOrigin cHandle e $ ParticipantUpdate p

processRequest db ss ws cHandle ctvar (Request.WriteView preset view) = do
  now <- getCurrentTime
  x <- runTransaction ss $ do
    when (not $ nameIsLegal preset) $ throwError "view name cannot contain spaces/newlines/control characters"
    writeView now ctvar preset view
    updateLastEdit ctvar now
    getClientEnsemble ctvar
  case x of
    Left err -> do
      let m = "*PublishView* " <> err
      sendThisClient ctvar (Response.Error m)
      postLog cHandle m
    Right e -> do
      sendThisClient ctvar (Response.OK $ "published view " <> preset)
      sendEnsembleNoOrigin cHandle e $ Response.WriteView preset view
      postLog cHandle $ "WriteView in (" <> E.ensembleName e <> "," <> preset <> ")"

processRequest db ss ws cHandle ctvar (Request.WriteTempo t) = do
  now <- getCurrentTime
  x <- runTransaction ss $ do
    writeTempo now ctvar t
    updateLastEdit ctvar now
    getClientEnsemble ctvar
  case x of
    Left err -> do
      let m = "*WriteTempo* " <> err
      sendThisClient ctvar (Response.Error m)
      postLog cHandle m
    Right e -> do
      sendThisClient ctvar (Response.OK $ "setting tempo succeeded")
      sendEnsembleNoOrigin cHandle e $ Response.WriteTempo t
      postLog cHandle $ "WriteTempo in " <> E.ensembleName e

processRequest db ss ws cHandle ctvar (Request.WriteResourceOps ops) = do
  now <- getCurrentTime
  x <- runTransaction ss $ do
    writeResourceOps now ctvar ops
    updateLastEdit ctvar now
    getClientEnsemble ctvar
  case x of
    Left err -> do
      let m = "*WriteResourceOps* " <> err
      sendThisClient ctvar (Response.Error m)
      postLog cHandle m
    Right e -> do
      sendEnsembleNoOrigin cHandle e $ Response.WriteResourceOps ops
      postLog cHandle $ "WriteResourceOps in " <> E.ensembleName e

processRequest db ss ws cHandle ctvar Request.ResetZones = do
  now <- getCurrentTime
  x <- runTransaction ss $ do
    resetZones now ctvar
    updateLastEdit ctvar now
    getClientEnsemble ctvar
  case x of
    Left err -> do
      let m = "*ResetZonesRequest* " <> err
      sendThisClient ctvar (Response.Error m)
      postLog cHandle m
    Right e -> do
      sendThisClient ctvar (Response.OK $ "resetzones succeeded")
      sendEnsembleNoOrigin cHandle e $ Response.ResetZones
      postLog cHandle $ "ResetZones in " <> E.ensembleName e

processRequest db ss ws cHandle ctvar Request.ResetViews = do
  now <- getCurrentTime
  x <- runTransaction ss $ do
    resetViews now ctvar
    updateLastEdit ctvar now
    getClientEnsemble ctvar
  case x of
    Left err -> do
      let m = "*ResetViewsRequest* " <> err
      sendThisClient ctvar (Response.Error m)
      postLog cHandle m
    Right e -> do
      sendThisClient ctvar (Response.OK $ "resetviews succeeded")
      sendEnsembleNoOrigin cHandle e $ Response.ResetViews
      postLog cHandle $ "ResetViews in " <> E.ensembleName e

processRequest db ss ws cHandle ctvar (Request.Reset t) = do
  now <- getCurrentTime
  x <- runTransaction ss $ do
    resetZones now ctvar
    writeTempo now ctvar t
    updateLastEdit ctvar now
    getClientEnsemble ctvar
  case x of
    Left err -> do
      let m = "*ResetRequest* " <> err
      sendThisClient ctvar (Response.Error m)
      postLog cHandle m
    Right e -> do
      sendThisClient ctvar (Response.OK $ "reset succeeded")
      sendEnsembleNoOrigin cHandle e $ Response.Reset t
      postLog cHandle $ "Reset in " <> E.ensembleName e
