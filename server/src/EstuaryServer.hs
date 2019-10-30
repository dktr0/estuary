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
import Control.Concurrent.MVar
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

runServerWithDatabase :: Text -> Int -> SQLite.Connection -> IO ()
runServerWithDatabase pswd port db = do
  nCap <- getNumCapabilities
  postLogToDatabase db $ "Estuary collaborative editing server, listening on port " <> showt port
  postLogToDatabase db $ "max simultaneous Haskell threads = " <> showt nCap
  postLogToDatabase db $ "administrative password = " <> pswd
  es <- readEnsembles db
  postLogToDatabase db $ (T.pack $ show (size es)) <> " ensembles restored from database"
  s <- newMVar $ newServerState { administrativePassword = pswd, ensembles = es }
  let settings = (defaultWebAppSettings "Estuary.jsexe") {
    ssIndices = [unsafeToPiece "index.html"],
    ssMaxAge = MaxAgeSeconds 30 -- 30 seconds max cache time
    }
  runTLS ourTLSSettings (ourSettings port) $ gzipMiddleware $ WS.websocketsOr WS.defaultConnectionOptions (webSocketsApp db s) (staticApp settings)

ourTLSSettings :: TLSSettings
ourTLSSettings = defaultTlsSettings {
  certFile = "cert.pem",
  keyFile = "privkey.pem",
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

webSocketsApp :: SQLite.Connection -> MVar ServerState -> WS.ServerApp -- = PendingConnection -> IO ()
webSocketsApp db sMVar ws = do
  ws' <- try $ WS.acceptRequest ws
  case ws' of
    Right ws'' -> do
      postLogToDatabase db $ "received new connection"
      ss <- takeMVar sMVar
      t <- getCurrentTime
      let (cHandle,ss') = addClient t ss ws''
      let ss'' = ss' { connectionCount = connectionCount ss' + 1 }
      -- ss''' <- runTransaction broadcastServerClientCount db cHandle ss'
      putMVar sMVar ss''
      (WS.forkPingThread ws'' 30) `catch` \(SomeException e) -> postLogToDatabase db $ "exception in forking ping thread: " <> (T.pack $ show e)
      processLoop db ws'' sMVar cHandle
    Left (SomeException e) -> do
      postLogToDatabase db $ "exception during WS.acceptRequest: " <> (T.pack $ show e)



processLoop :: SQLite.Connection -> WS.Connection -> MVar ServerState -> ClientHandle -> IO ()
processLoop db ws sMVar cHandle = do
  m <- try $ WS.receiveData ws
--  t0 <- getCurrentTime
  case m of
    Right m' -> do
      s <- takeMVar sMVar
      s' <- if (isJust $ IntMap.lookup cHandle (clients s))
        then runTransaction (processMessage m') db cHandle s
        else do
          postLogToDatabase db $ "*** warning - failed sanity check: WS.receiveData succeeded for client already deleted from server"
          return s
      putMVar sMVar s'
--      t1 <- getCurrentTime
--      T.putStrLn $ showt ((realToFrac $ diffUTCTime t1 t0) :: Double)
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
      postLogToDatabase db $ "parse exception: " <> (T.pack e)
      processLoop db ws sMVar cHandle
    Left _ -> postLogToDatabase db $ "***unknown exception in processLoop - terminating this client's connection***"
    -- Left (WS.UnicodeException e) -> do
    --  postLog db $ "Unicode exception: " ++ e
    --  processLoop db ws s h


processMessage :: ByteString -> Transaction ()
processMessage msg =
  case eitherDecode msg of
    Right x -> processRequest x
    Left x -> throwError (T.pack x)

processRequest :: Request -> Transaction ()

processRequest (BrowserInfo t) = do
  postLog "BrowserInfo"
  modifyClient $ \c -> c { browserInfo = t }

processRequest (ClientInfo pingTime load animationLoad latency)  = do
  postLog "ClientInfo" -- note: should disable or throttle logging of this for high user count situations
  modifyClient $ \c -> c {
    clientMainLoad = load,
    clientAnimationLoad = animationLoad,
    clientLatency = latency
    }
  n <- getServerClientCount
  respond $ ServerInfo n pingTime
  updateParticipant

processRequest GetEnsembleList = do
  postLog $ "GetEnsembleList"
  es <- gets (keys . ensembles)
  respond $ EnsembleList es

processRequest (Authenticate x) = do
  pwd <- gets administrativePassword
  if x == pwd
    then do
      postLog $ "Authenticate with correct password"
      modifyClient $ \x -> x { authenticated = True }
    else do
      postLog $ "Authenticate with incorrect password"
      modifyClient $ \x -> x { authenticated = False }

processRequest (CreateEnsemble name pwd) = do
  whenNotAuthenticated $ throwError "ignoring CreateEnsemble from non-authenticated client"
  postLog $ "CreateEnsemble " <> name
  t <- liftIO $ getCurrentTime
  modify' $ createEnsemble name pwd t
  es <- gets (keys . ensembles)
  respondAll $ EnsembleList es
  saveNewEnsembleToDatabase name

processRequest (JoinEnsemble eName uName loc pwd) = do  -- JoinEnsemble Text Text Text Text
  postLog $ "joining ensemble " <> eName
  s <- get
  e <- justOrError (Data.Map.lookup eName $ ensembles s) "attempt to join non-existent ensemble"
  when (uName /= "") $ do
    -- client is requesting a specific user name in the ensemble, succeeds only if not already taken
    handleTaken <- handleTakenInEnsemble uName eName
    when handleTaken $ do
      let m = "user handle already used by someone else in this ensemble"
      respond $ ResponseError m
      throwError m
  when (E.password e /= "" && pwd /= "" && E.password e /= pwd) $ do
    -- the ensemble requires password for authentication, they provided one, but it doesn't match
    let m = "incorrect ensemble password"
    respond $ ResponseError m
    throwError m
  -- if we get this far, the client's join attempt will succeed
  -- we need to record whether they are authorized in the ensemble
  -- which will be true if either:
  -- (a) no password is required, or
  -- (b) they entered the required password
  let authed = (E.password e == "" || E.password e == pwd)
  -- update the server's record for this client to register successful ensemble join
  modifyClient $ \c -> c {
    memberOfEnsemble = Just eName,
    handleInEnsemble = uName,
    locationInEnsemble = loc,
    statusInEnsemble = "",
    authenticatedInEnsemble = authed
    }
  -- send responses to this client indicating successful join, and ensemble tempo, defs and views
  respond $ JoinedEnsemble eName uName
  respond $ EnsembleResponse $ TempoRcvd (E.tempo $ E.ensemble e)
  mapM_ respond $ fmap EnsembleResponse $ IntMap.mapWithKey ZoneRcvd $ E.zones $ E.ensemble e
  mapM_ respond $ fmap EnsembleResponse $ Map.mapWithKey ViewRcvd $ E.views $ E.ensemble e
  -- TODO: send new participant information about existing participants (they'll get *some* info on updates, anyway)
  -- send information about new participant to all clients in this ensemble
  let anonymous = uName == ""
  when (not anonymous) $ do
    p <- clientToParticipant <$> getClient
    respondEnsemble $ EnsembleResponse $ ParticipantJoins uName p
  when anonymous $ do
    n <- countAnonymousParticipants
    respondEnsemble $ EnsembleResponse $ AnonymousParticipants n

-- *** note: other members still need to be notified of departure due to disconnection also!!! ***
processRequest LeaveEnsemble = do
  postLog $ "leaving ensemble"
  -- notify all other members of the ensemble of this client's departure
  uName <- handleInEnsemble <$> getClient
  let anonymous = uName == ""
  when (not anonymous) $ respondEnsembleNoOrigin $ EnsembleResponse $ ParticipantLeaves uName
  when anonymous $ do
    n <- countAnonymousParticipants
    respondEnsembleNoOrigin $ EnsembleResponse $ AnonymousParticipants (n-1)
  -- modify servers record of this client so that they are ensemble-less
  modifyClient $ \c -> c {
    memberOfEnsemble = Nothing,
    handleInEnsemble = "",
    locationInEnsemble = "",
    statusInEnsemble = "",
    authenticatedInEnsemble = False
    }

processRequest (EnsembleRequest x) = processEnsembleRequest x


processEnsembleRequest :: EnsembleRequest -> Transaction ()

processEnsembleRequest (WriteZone zone value) = do
  whenNotAuthenticatedInEnsemble $ throwError "ignoring ZoneRequest from client not authenticated in ensemble"
  eName <- getEnsembleName
  -- postLog $ "Edit in (" <> eName <> "," <> showt zone <> ")"
  modify' $ writeZone eName zone value
  respondEnsembleNoOrigin $ EnsembleResponse $ ZoneRcvd zone value
  -- saveEnsembleToDatabase -- note: 100x improvement in server transaction time when not logging message above or saving ensemble
  updateLastEdit
  return ()

processEnsembleRequest (WriteChat msg) = do
  whenNotAuthenticatedInEnsemble $ throwError "ignoring WriteChat from client not authenticated in ensemble"
  sender <- handleInEnsemble <$> getClient
  when (sender == "") $ throwError "ignoring WriteChat from anonymous (yet authenticated) client in ensemble"
  eName <- getEnsembleName
  postLog $ "WriteChat in " <> eName <> " from " <> sender <> ": " <> msg
  now <- liftIO $ getCurrentTime
  respondEnsemble $ EnsembleResponse (ChatRcvd (Chat now sender msg))
  updateLastEdit

processEnsembleRequest (WriteStatus msg) = do
  whenNotAuthenticatedInEnsemble $ throwError "ignoring WriteStatus from client not authenticated in ensemble"
  name <- handleInEnsemble <$> getClient
  when (name == "") $ throwError "ignoring WriteStatus from anonymous (yet authenticated) client in ensemble"
  eName <- getEnsembleName
  postLog $ "WriteStatus in " <> eName <> " from " <> name <> ": " <> msg
  modifyClient $ \c -> c { statusInEnsemble = msg }
  p <- clientToParticipant <$> getClient
  respondEnsembleNoOrigin $ EnsembleResponse $ ParticipantUpdate name p
  updateLastEdit

processEnsembleRequest (WriteView preset view) = do
  whenNotAuthenticatedInEnsemble $ throwError "ignoring WriteView from client not authenticated in ensemble"
  eName <- getEnsembleName
  postLog $ "WriteView in (" <> eName <> "," <> preset <> "): " <> (T.pack $ show view) -- TODO: View should work with TextShow to avoid string
  modify' $ writeView eName preset view
  saveEnsembleToDatabase
  respondEnsembleNoOrigin $ EnsembleResponse $ ViewRcvd preset view
  updateLastEdit

processEnsembleRequest (WriteTempo t) = do
  whenNotAuthenticatedInEnsemble $ throwError "ignoring SetTempo from client not authenticated in ensemble"
  eName <- getEnsembleName
  postLog $ "WriteTempo in " <> eName
  modify' $ writeTempo eName t
  respondEnsembleNoOrigin $ EnsembleResponse $ TempoRcvd t
  saveEnsembleToDatabase
  updateLastEdit


updateParticipant :: Transaction ()
updateParticipant = do
  -- note: probably should fail silently (non-failure) if not in an ensemble or anonymous
  p <- clientToParticipant <$> getClient
  respondEnsembleNoOrigin $ EnsembleResponse $ ParticipantUpdate (name p) p

updateLastEdit :: Transaction ()
updateLastEdit = do
  -- note: probably should fail silently (non-failure) if not in an ensemble or anonymous
  t <- liftIO $ getCurrentTime
  modifyClient $ \c -> c { lastEditInEnsemble = t }
  p <- clientToParticipant <$> getClient
  respondEnsembleNoOrigin $ EnsembleResponse $ ParticipantUpdate (name p) p
