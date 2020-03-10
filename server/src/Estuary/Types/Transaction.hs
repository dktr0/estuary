{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.Transaction where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Database.SQLite.Simple as SQLite
import qualified Network.WebSockets as WS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Aeson
import Data.Maybe
import Control.Concurrent.STM
import TextShow
import Data.Time

import Estuary.Types.ServerState
import qualified Estuary.Types.Ensemble as E
import qualified Estuary.Types.EnsembleS as E
import Estuary.Types.Client
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.Database
import Estuary.Types.Definition
import Estuary.Types.View
import Estuary.Types.Tempo
import Estuary.Types.Participant

type Transaction = ReaderT ServerState (ExceptT Text STM)

liftSTM :: STM a -> Transaction a
liftSTM = lift . lift

runTransaction :: ServerState -> Transaction a -> IO (Either Text a)
runTransaction ss t = do
  x <- try $ atomically $ runExceptT (runReaderT t ss)
  case x of
    Left e -> return $ Left $ "* runTransaction caught exception: " <> T.pack (show (e :: SomeException))
    Right a -> return a

runTransactionIO :: ServerState -> Transaction (IO a) -> IO (Either Text a)
runTransactionIO ss t = do
  x <- runTransaction ss t
  case x of
    Left e -> return $ Left e
    Right a -> do
      a' <- try a
      case a' of
        Left e -> return $ Left $ "* runTransactionIO caught exception: " <> T.pack (show (e :: SomeException))
        Right a'' -> return $ Right a''

runTransactionLogged :: SQLite.Connection -> ServerState -> ClientHandle -> Text -> Transaction a -> IO ()
runTransactionLogged db ss cHandle msgPrefix t = runTransaction ss t >>= postLeftsToLog db cHandle msgPrefix

runTransactionIOLogged :: SQLite.Connection -> ServerState -> ClientHandle -> Text -> Transaction (IO a) -> IO ()
runTransactionIOLogged db ss cHandle msgPrefix t = runTransaction ss t >>= postLeftsToLog db cHandle msgPrefix

justOrError :: Maybe a -> Text -> Transaction a
justOrError m e = maybe (throwError e) (return) m

getClient :: ClientHandle -> Transaction Client
getClient cHandle = do
  s <- ask
  cMap <- liftSTM $ readTVar (clients s)
  ctvar <- justOrError (IntMap.lookup cHandle cMap) $ "client (" <> showt cHandle <> ") not found in Server"
  liftSTM $ readTVar ctvar

getEnsembleName :: ClientHandle -> Transaction Text
getEnsembleName cHandle = do
  eName <- memberOfEnsemble <$> getClient cHandle
  justOrError eName "getEnsembleName for client not member of ensemble"

modifyClient :: ClientHandle -> (Client -> Client) -> Transaction ()
modifyClient cHandle f = do
  ss <- ask
  cMap <- liftSTM $ readTVar (clients ss)
  ctvar <- justOrError (IntMap.lookup cHandle cMap) $ "client (" <> showt cHandle <> ") not found in Server"
  liftSTM $ do
    c <- readTVar ctvar
    writeTVar ctvar $ f c

getEnsembleS :: Text -> Transaction E.EnsembleS
getEnsembleS eName = do
  s <- ask
  eMap <- liftSTM $ readTVar (ensembles s)
  etvar <- justOrError (Map.lookup eName eMap) $ "ensemble " <> eName <> " not found on Estuary server"
  liftSTM $ readTVar etvar

-- returns how many anonymous participants in the ensemble the given ClientHandle is a part of
-- if the given ClientHandle is not part of an ensemble, returns -1
countAnonymousParticipants :: ClientHandle -> Transaction Int
countAnonymousParticipants cHandle = do
  ss <- ask
  c <- getClient cHandle
  case memberOfEnsemble c of
    Nothing -> return (-1)
    Just eName -> liftSTM $ do
      cMap <- readTVar (clients ss)
      cs <- mapM readTVar cMap
      return $ IntMap.size $ IntMap.filter (\x -> (handleInEnsemble x == "") && (memberOfEnsemble x == Just eName)) cs

getEnsembleNames :: Transaction [Text]
getEnsembleNames = do
  s <- ask
  eMap <- liftSTM $ readTVar (ensembles s)
  return $ Map.keys eMap

modifyEnsembleS :: ClientHandle -> (E.EnsembleS -> E.EnsembleS) -> Transaction ()
modifyEnsembleS cHandle f = do
  c <- getClient cHandle
  when (isNothing $ memberOfEnsemble c) $ throwError "modifyEnsembleS for client not in ensemble"
  when (isJust $ memberOfEnsemble c) $ do
    let eName = fromJust $ memberOfEnsemble c
    s <- ask
    eMap <- liftSTM $ readTVar (ensembles s)
    etvar <- justOrError (Map.lookup eName eMap) $ "ensemble " <> eName <> " not found in Server"
    liftSTM $ do
      e <- readTVar etvar
      writeTVar etvar $ f e

authenticate :: ClientHandle -> Text -> Transaction Bool
authenticate cHandle x = do
  pwd <- administrativePassword <$> ask
  modifyClient cHandle $ \c -> c { authenticated = x == pwd }
  return $ x == pwd

writeZone :: ClientHandle -> Int -> Definition -> Transaction ()
writeZone cHandle zone def = do
  whenNotAuthenticatedInEnsemble cHandle $ throwError "ignoring ZoneRequest from client not authenticated in ensemble"
  modifyEnsembleS cHandle $ E.modifyEnsemble (E.writeZone zone def)

writeView :: ClientHandle -> Text -> View -> Transaction ()
writeView cHandle vName v = modifyEnsembleS cHandle $ E.modifyEnsemble (E.writeView vName v)

writeTempo :: ClientHandle -> Tempo -> Transaction ()
writeTempo cHandle t = modifyEnsembleS cHandle $ E.modifyEnsemble (E.writeTempo t)

handleTakenInEnsemble :: Text -> Text -> Transaction Bool
handleTakenInEnsemble uName eName = do
  when (uName == "") $ throwError "handleTakenInEnsemble called with empty uName"
  when (eName == "") $ throwError "handleTakenInEnsemble called with empty eName"
  s <- ask
  liftSTM $ do
    x <- readTVar $ clients s
    x' <- mapM readTVar x
    let x'' = IntMap.filter (\c -> memberOfEnsemble c == Just eName && handleInEnsemble c == uName) x'
    return $ IntMap.size x'' > 0


getServerClientCount :: Transaction Int
getServerClientCount = do
  s <- ask
  cMap <- liftSTM $ readTVar (clients s)
  return $ IntMap.size cMap

createEnsemble :: ClientHandle -> Text -> Text -> UTCTime -> Transaction E.EnsembleS
createEnsemble cHandle name pwd now = do
  whenNotAuthenticated cHandle $ throwError "ignoring CreateEnsemble from non-authenticated client"
  s <- ask
  oldMap <- liftSTM $ readTVar (ensembles s)
  when (isJust $ Map.lookup name oldMap) $ throwError "ignoring CreateEnsemble for duplicate ensemble name"
  let newEns = E.writePassword pwd $ E.emptyEnsembleS now
  etvar <- liftSTM $ newTVar $ newEns
  let newMap = Map.insert name etvar oldMap
  liftSTM $ writeTVar (ensembles s) newMap
  return newEns

joinEnsemble :: SQLite.Connection -> ClientHandle -> Text -> Text -> Text -> Text -> Transaction (IO ())
joinEnsemble db cHandle eName uName loc pwd = do
  self <- getClient cHandle
  e <- getEnsembleS eName
  cs <- clientsInEnsemble eName
  n <- countAnonymousParticipants cHandle
  -- when client is requesting a specific user name in the ensemble, succeeds only if not already taken...
  when (uName /= "") $ do
    handleTaken <- handleTakenInEnsemble uName eName
    when handleTaken $ throwError $ "handle " <> uName <> " already in use in ensemble " <> eName
  -- when the ensemble requires password for authentication, they provided one, but it doesn't match...
  when (E.password e /= "" && pwd /= "" && E.password e /= pwd) $ throwError "incorrect ensemble password"
  -- if we get this far, the client's join attempt will succeed
  let authed = (E.password e == "" || E.password e == pwd)
  modifyClient cHandle $ \c -> c {
    memberOfEnsemble = Just eName,
    handleInEnsemble = uName,
    locationInEnsemble = loc,
    statusInEnsemble = "",
    authenticatedInEnsemble = authed
  }
  return $ do
    -- send responses to this client indicating successful join, and ensemble tempo, defs and views
    let respond = sendClient db cHandle self
    respond $ JoinedEnsemble eName uName
    respond $ EnsembleResponse $ TempoRcvd (E.tempo $ E.ensemble e)
    mapM_ respond $ fmap EnsembleResponse $ IntMap.mapWithKey ZoneRcvd $ E.zones $ E.ensemble e
    mapM_ respond $ fmap EnsembleResponse $ Map.mapWithKey ViewRcvd $ E.views $ E.ensemble e
    -- TODO: send new participant information about existing participants (they'll get *some* info on updates, anyway)
    -- send information about new participant to all clients in this ensemble
    let respondEnsemble = sendClients db cHandle cs
    let anonymous = uName == ""
    when (not anonymous) $ respondEnsemble $ EnsembleResponse $ ParticipantJoins uName (clientToParticipant self)
    when anonymous $ respondEnsemble $ EnsembleResponse $ AnonymousParticipants n


leaveEnsemble :: SQLite.Connection -> ClientHandle -> Transaction (IO ())
leaveEnsemble db cHandle = do
  c <- getClient cHandle
  modifyClient cHandle $ \x -> x {
    memberOfEnsemble = Nothing,
    handleInEnsemble = "",
    locationInEnsemble = "",
    statusInEnsemble = "",
    authenticatedInEnsemble = False
    }
  n <- countAnonymousParticipants cHandle
  cs <- getEnsembleClientsNoOrigin cHandle
  return $ do
    when (isJust $ memberOfEnsemble c) $ do -- only send a response if the client was actually in an ensemble...
      -- notify all other members of the ensemble of this client's departure
      let eName = fromJust $ memberOfEnsemble c
      let uName = handleInEnsemble c
      let anonymous = uName == ""
      when (not anonymous) $ do
        postLog db cHandle $ uName <> " leaving ensemble " <> eName
        sendClients db cHandle cs $ EnsembleResponse $ ParticipantLeaves uName
      when anonymous $ do
        postLog db cHandle $ "(anonymous) leaving ensemble " <> eName
        sendClients db cHandle cs $ EnsembleResponse $ AnonymousParticipants n


isAuthenticated :: ClientHandle -> Transaction Bool
isAuthenticated cHandle = getClient cHandle >>= return . authenticated

isAuthenticatedInEnsemble :: ClientHandle -> Transaction Bool
isAuthenticatedInEnsemble cHandle = getClient cHandle >>= return . authenticatedInEnsemble

whenAuthenticated :: ClientHandle -> Transaction a -> Transaction ()
whenAuthenticated cHandle t = do
  x <- isAuthenticated cHandle
  when x $ void t
  return ()

whenNotAuthenticated :: ClientHandle -> Transaction a -> Transaction ()
whenNotAuthenticated cHandle t = do
  x <- isAuthenticated cHandle
  when (not x) $ void t
  return ()

whenAuthenticatedInEnsemble :: ClientHandle -> Transaction a -> Transaction ()
whenAuthenticatedInEnsemble cHandle t = do
  x <- isAuthenticatedInEnsemble cHandle
  when x $ void t
  return ()

whenNotAuthenticatedInEnsemble :: ClientHandle -> Transaction a -> Transaction ()
whenNotAuthenticatedInEnsemble cHandle t = do
  x <- isAuthenticatedInEnsemble cHandle
  when (not x) $ void t
  return ()

send :: SQLite.Connection -> ClientHandle -> ClientHandle -> WS.Connection -> Response -> IO ()
send db originHandle destHandle c x = do
  y <- try $ WS.sendTextData c $ encode x
  case y of
    Right x -> return ()
    Left (SomeException e) -> do
      let ce = fromException (SomeException e)
      case ce of
        Just (WS.CloseRequest _ _) -> postLog db originHandle $ "CloseRequest exception sending to (" <> showt destHandle <> ")"
        Just WS.ConnectionClosed -> postLog db originHandle $ "ConnectionClosed exception sending to (" <> showt destHandle <> ")"
        otherwise -> postLog db originHandle $ "unusual exception sending to (" <> showt destHandle <> ") : " <> (T.pack $ show e)

sendClient :: SQLite.Connection -> ClientHandle -> Client -> Response -> IO ()
sendClient db originHandle c x = send db originHandle (Estuary.Types.Client.handle c) (connection c) x

sendClients :: SQLite.Connection -> ClientHandle -> [Client] -> Response -> IO ()
sendClients db originHandle cs r = mapM_ (\x -> sendClient db originHandle x r) cs

getEnsembleClients :: ClientHandle -> Transaction [Client]
getEnsembleClients cHandle = do
  self <- getClient cHandle
  case memberOfEnsemble self of
    Just eName -> clientsInEnsemble eName
    Nothing -> return []

getEnsembleClientsNoOrigin :: ClientHandle -> Transaction [Client]
getEnsembleClientsNoOrigin cHandle = do
  self <- getClient cHandle
  case memberOfEnsemble self of
    Just eName -> clientsInEnsembleNoOrigin cHandle eName
    Nothing -> return []

clientsInEnsemble :: Text -> Transaction [Client]
clientsInEnsemble eName = do
  s <- ask
  liftSTM $ do
    cMap <- readTVar (clients s)
    cMap' <- mapM readTVar cMap
    return $ IntMap.elems $ ensembleFilter (Just eName) cMap'

clientsInEnsembleNoOrigin :: ClientHandle -> Text -> Transaction [Client]
clientsInEnsembleNoOrigin cHandle eName = do
  s <- ask
  liftSTM $ do
    cMap <- readTVar (clients s)
    cMap' <- mapM readTVar cMap
    return $ IntMap.elems $ IntMap.delete cHandle $ ensembleFilter (Just eName) cMap'

ensembleFilter :: Maybe Text -> IntMap.IntMap Client -> IntMap.IntMap Client
ensembleFilter (Just e) = IntMap.filter $ (==(Just e)) . memberOfEnsemble
ensembleFilter Nothing = const IntMap.empty

saveEnsembleToDatabase :: SQLite.Connection -> ServerState -> Text -> IO ()
saveEnsembleToDatabase db ss eName = do
  etvar <- atomically $ Map.lookup eName <$> readTVar (ensembles ss)
  case etvar of
    Nothing -> postLogNoHandle db $ "*** strange error: saveEnsembleToDatabase for " <> eName <> " (which is not found in ensemble map)"
    Just x -> do
      x' <- atomically $ readTVar x
      writeEnsembleS db eName x'

updateLastEdit :: ClientHandle -> UTCTime -> Transaction Participant
updateLastEdit cHandle now = do
  modifyClient cHandle $ \c -> c { lastEditInEnsemble = now }
  clientToParticipant <$> getClient cHandle
