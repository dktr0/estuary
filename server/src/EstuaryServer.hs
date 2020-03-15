{-# LANGUAGE OverloadedStrings #-}
module EstuaryServer where

import Data.List ((\\))
import Data.Maybe (fromMaybe,isJust,fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
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
import qualified Network.Wai as WS
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

runServerWithDatabase :: Text -> Int -> Bool -> SQLite.Connection -> IO ()
runServerWithDatabase pwd port httpRedirect db = do
  nCap <- getNumCapabilities
  postLogNoHandle db $ "Estuary collaborative editing server"
  postLogNoHandle db $ "max simultaneous Haskell threads = " <> showt nCap
  postLogNoHandle db $ "administrative password = " <> pwd
  es <- readEnsembles db
  postLogNoHandle db $ showt (size es) <> " ensembles restored from database"
  s <- newServerState pwd es
  let settings = (defaultWebAppSettings "Estuary.jsexe") {
    ssIndices = [unsafeToPiece "index.html"],
    ssMaxAge = MaxAgeSeconds 30 -- 30 seconds max cache time
    }
  postLogNoHandle db $ "listening on port " <> showt port <> " (HTTPS only)"
  when httpRedirect $ void $ forkIO $ do
    postLogNoHandle db $ "(also listening on port 80 and redirecting plain HTTP requests to HTTPS, ie. on port 443)"
    run 80 ourRedirect
  runTLS ourTLSSettings (ourSettings port) $ gzipMiddleware $ WS.websocketsOr WS.defaultConnectionOptions (webSocketsApp db s) (staticApp settings)

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
  let rh = WS.pendingRequest pc
  Prelude.putStrLn $ show rh
  ws' <- try $ WS.acceptRequest pc
  case ws' of
    Right ws'' -> do
      cHandle <- addClient ss ws''
      postLog db cHandle "new connection"
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
close db ss cHandle msg = runTransactionIOLogged db ss cHandle "close" $ do
  c <- getClient cHandle
  n <- countAnonymousParticipants cHandle
  cs <- getEnsembleClientsNoOrigin cHandle
  deleteClient cHandle
  return $ do
    postLog db cHandle $ "closing connection: " <> msg
    when (isJust $ memberOfEnsemble c) $ do
      let eName = fromJust $ memberOfEnsemble c
      let uName = handleInEnsemble c
      let uName' = if uName == "" then "(anonymous)" else uName
      postLog db cHandle $ uName' <> " leaves ensemble " <> eName
      let r = if uName == "" then AnonymousParticipants n else ParticipantLeaves uName
      sendClients db cHandle cs (EnsembleResponse r)


processMessage :: SQLite.Connection -> ServerState -> WS.Connection -> ClientHandle -> ByteString -> IO ()
processMessage db ss ws cHandle msg = case eitherDecode msg of
  Right x -> processRequest db ss ws cHandle x
  Left x -> postLog db cHandle $ "* eitherDecode error in processMessage: " <> (T.pack x)

processRequest :: SQLite.Connection -> ServerState -> WS.Connection -> ClientHandle -> Request -> IO ()

processRequest db ss ws cHandle (BrowserInfo t) =
  runTransactionLogged db ss cHandle "BrowserInfo" $ modifyClient cHandle $ \c -> c { browserInfo = t }

processRequest db ss ws cHandle (ClientInfo pingTime load animationLoad latency) = do
  runTransactionIOLogged db ss cHandle "ClientInfo" $ do
    modifyClient cHandle $ \c -> c {
      clientMainLoad = load,
      clientAnimationLoad = animationLoad,
      clientLatency = latency
      }
    n <- getServerClientCount
    self <- getClient cHandle
    ecs <- getEnsembleClients cHandle
    return $ do
      send db cHandle cHandle ws $ ServerInfo n pingTime
      sendClients db cHandle ecs $ EnsembleResponse $ ParticipantUpdate (handleInEnsemble self) (clientToParticipant self)

processRequest db ss ws cHandle GetEnsembleList = do
  runTransactionIOLogged db ss cHandle "GetEnsembleList" $ do
    eNames <- getEnsembleNames
    return $ send db cHandle cHandle ws $ EnsembleList eNames

processRequest db ss ws cHandle (Authenticate x) = do
  runTransactionIOLogged db ss cHandle "Authenticate" $ do
    y <- authenticate cHandle x
    return $ case y of
      True -> postLog db cHandle $ "Authenticate with correct password"
      False -> postLog db cHandle $ "Authenticate with incorrect password"

processRequest db ss ws cHandle (CreateEnsemble name pwd) = do
  now <- getCurrentTime
  runTransactionIOLogged db ss cHandle "CreateEnsemble" $ do
    e <- createEnsemble cHandle name pwd now
    return $ do
      postLog db cHandle $ "CreateEnsemble " <> name <> " password=" <> pwd
      writeNewEnsembleS db name e -- NOTE: this database write will be unnecessary when we move to a model where database operations (except logging) are in a different "low priority" thread

processRequest db ss ws cHandle (JoinEnsemble eName uName loc pwd) = do
  x <- runTransactionIO ss $ joinEnsemble db cHandle eName uName loc pwd
  let uName' = if uName == "" then "(anonymous)" else uName
  case x of
    Left e -> do
      send db cHandle cHandle ws $ ResponseError e
      postLog db cHandle $ "JoinEnsemble " <> eName <> " as " <> uName' <> " FAILED because: " <> e
    Right _ -> postLog db cHandle $ "joined ensemble " <> eName <> " as " <> uName'

processRequest db ss ws cHandle LeaveEnsemble = do
  runTransactionIOLogged db ss cHandle "LeaveEnsemble" $ leaveEnsemble db cHandle

processRequest db ss ws cHandle (EnsembleRequest x) = processEnsembleRequest db ss ws cHandle x

processEnsembleRequest :: SQLite.Connection -> ServerState -> WS.Connection -> ClientHandle -> EnsembleRequest -> IO ()

processEnsembleRequest db ss ws cHandle (WriteZone zone value) = do
  now <- getCurrentTime
  runTransactionIOLogged db ss cHandle "WriteZone" $ do
    writeZone cHandle zone value
    p <- updateLastEdit cHandle now
    cs <- getEnsembleClientsNoOrigin cHandle
    cs' <- getEnsembleClients cHandle
    eName <- getEnsembleName cHandle
    return $ do
      sendClients db cHandle cs $ EnsembleResponse $ ZoneRcvd zone value
      sendClients db cHandle cs' $ EnsembleResponse $ ParticipantUpdate (name p) p
      saveEnsembleToDatabase db ss eName

processEnsembleRequest db ss ws cHandle (WriteChat msg) = do
  now <- getCurrentTime
  runTransactionIOLogged db ss cHandle "WriteChat" $ do
    whenNotAuthenticatedInEnsemble cHandle $ throwError "ignoring WriteChat (not authenticated)"
    eName <- getEnsembleName cHandle
    uName <- handleInEnsemble <$> getClient cHandle
    when (uName == "") $ throwError "ignoring WriteChat from anonymous"
    p <- updateLastEdit cHandle now
    cs <- getEnsembleClients cHandle
    eName <- getEnsembleName cHandle
    return $ do
      postLog db cHandle $ uName <> " in " <> eName <> " chats: " <> msg
      sendClients db cHandle cs $ EnsembleResponse $ ChatRcvd $ Chat now uName msg
      sendClients db cHandle cs $ EnsembleResponse $ ParticipantUpdate (name p) p

processEnsembleRequest db ss ws cHandle (WriteStatus msg) = do
  now <- getCurrentTime
  runTransactionIOLogged db ss cHandle "WriteStatus" $ do
    whenNotAuthenticatedInEnsemble cHandle $ throwError "ignoring WriteStatus (not authenticated)"
    eName <- getEnsembleName cHandle
    uName <- handleInEnsemble <$> getClient cHandle
    when (uName == "") $ throwError "ignoring WriteStatus from anonymous"
    modifyClient cHandle $ \c -> c { statusInEnsemble = msg }
    p <- updateLastEdit cHandle now
    cs <- getEnsembleClients cHandle
    return $ do
      postLog db cHandle $ "WriteStatus from " <> uName <> " in " <> eName <> ": " <> msg
      sendClients db cHandle cs $ EnsembleResponse $ ParticipantUpdate (name p) p

processEnsembleRequest db ss ws cHandle (WriteView preset view) = do
  now <- getCurrentTime
  runTransactionIOLogged db ss cHandle "WriteStatus" $ do
    writeView cHandle preset view
    eName <- getEnsembleName cHandle
    p <- updateLastEdit cHandle now
    cs <- getEnsembleClientsNoOrigin cHandle
    cs' <- getEnsembleClients cHandle
    return $ do
      postLog db cHandle $ "WriteView in (" <> eName <> "," <> preset <> ")"
      sendClients db cHandle cs $ EnsembleResponse $ ViewRcvd preset view
      sendClients db cHandle cs' $ EnsembleResponse $ ParticipantUpdate (name p) p
      saveEnsembleToDatabase db ss eName

processEnsembleRequest db ss ws cHandle (WriteTempo t) = do
  now <- getCurrentTime
  runTransactionIOLogged db ss cHandle "WriteTempo" $ do
    writeTempo cHandle t
    eName <- getEnsembleName cHandle
    p <- updateLastEdit cHandle now
    cs <- getEnsembleClientsNoOrigin cHandle
    cs' <- getEnsembleClients cHandle
    return $ do
      postLog db cHandle $ "WriteTempo in " <> eName
      sendClients db cHandle cs $ EnsembleResponse $ TempoRcvd t
      sendClients db cHandle cs' $ EnsembleResponse $ ParticipantUpdate (name p) p
      saveEnsembleToDatabase db ss eName
