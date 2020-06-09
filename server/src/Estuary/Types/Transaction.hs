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
import Estuary.Types.Name

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
runTransactionIOLogged db ss cHandle msgPrefix t = runTransactionIO ss t >>= postLeftsToLog db cHandle msgPrefix

justOrError :: Maybe a -> Text -> Transaction a
justOrError m e = maybe (throwError e) (return) m


deleteClient :: ClientHandle -> Transaction Client
deleteClient cHandle = do
  ss <- ask
  oldMap <- liftSTM $ readTVar (clients ss)
  let ctvar = IntMap.lookup cHandle oldMap
  case ctvar of
    Just ctvar' -> liftSTM $ do
      -- remove client from any ensemble they are a part of
      theClient <- readTVar ctvar'
      case (memberOfEnsemble theClient) of
        Just etvar -> readTVar etvar >>= E.deleteConnection (Estuary.Types.Client.handle theClient)
        Nothing -> return ()
      -- remove client from global list of clients
      let newMap = IntMap.delete cHandle oldMap
      writeTVar (clients ss) newMap
      writeTVar (clientCount ss) (IntMap.size newMap)
      return theClient
    Nothing -> throwError $ "client " <> showt cHandle <> "not found"

removeAnotherClient :: ServerState -> SQLite.Connection -> ClientHandle -> ClientHandle -> Text -> IO ()
removeAnotherClient ss db originHandle targetHandle msg = do
  postLog db originHandle $ "removing (" <> showt targetHandle <> ") because: " <> msg
  x <- runTransaction ss $ deleteClient targetHandle
  case x of
    Left err -> postLog db originHandle $ "*error during removal* " <> err
    Right c -> do
      notifyWhenClientDepartsEnsemble ss db originHandle c
      postLog db originHandle $ "removal of (" <> showt targetHandle <> ") complete."


getEnsembleName :: TVar Client -> Transaction Text
getEnsembleName ctvar = do
  e <- liftSTM $ memberOfEnsemble <$> readTVar ctvar
  etvar <- justOrError e "getEnsembleName for client not member of ensemble"
  liftSTM $ E.ensembleName <$> readTVar etvar

getClientEnsemble :: TVar Client -> Transaction E.EnsembleS
getClientEnsemble ctvar = do
  e <- liftSTM $ memberOfEnsemble <$> readTVar ctvar
  etvar <- justOrError e "getClientEnsemble for client not member of ensemble"
  liftSTM $ readTVar etvar

getEnsemble :: Text -> Transaction E.EnsembleS
getEnsemble eName = do
  etvar <- getEnsembleTVar eName
  liftSTM $ readTVar etvar

getEnsembleTVar :: Text -> Transaction (TVar E.EnsembleS)
getEnsembleTVar eName = do
  s <- ask
  eMap <- liftSTM $ readTVar (ensembles s)
  justOrError (Map.lookup eName eMap) $ "ensemble " <> eName <> " not found on Estuary server"


getNamedParticipants :: ServerState -> E.EnsembleS -> IO [Participant]
getNamedParticipants ss e = do
  a <- readTVarIO $ clients ss -- IntMap (TVar Client)
  b <- readTVarIO $ E.namedConnections e -- IntMap Text
  c <- traverse readTVarIO $ IntMap.intersection a b
  return $ IntMap.elems $ fmap clientToParticipant c


writeZone :: UTCTime -> TVar Client -> Int -> Definition -> Transaction ()
writeZone now ctvar zone def = do
  c <- liftSTM $ readTVar ctvar
  etvar <- justOrError (memberOfEnsemble c) "client not part of ensemble"
  when (not $ authenticatedInEnsemble c) $ throwError "client not authenticated in ensemble"
  liftSTM $ do
    e <- readTVar etvar
    E.writeZone e now zone def


writeView :: UTCTime -> TVar Client -> Text -> View -> Transaction ()
writeView now ctvar vName v = do
  c <- liftSTM $ readTVar ctvar
  etvar <- justOrError (memberOfEnsemble c) "client not part of ensemble"
  when (not $ authenticatedInEnsemble c) $ throwError "client not authenticated in ensemble"
  liftSTM $ do
    e <- readTVar etvar
    E.writeView e now vName v


writeTempo :: UTCTime -> TVar Client -> Tempo -> Transaction ()
writeTempo now ctvar t = do
  c <- liftSTM $ readTVar ctvar
  etvar <- justOrError (memberOfEnsemble c) "client not part of ensemble"
  when (not $ authenticatedInEnsemble c) $ throwError "client not authenticated in ensemble"
  liftSTM $ do
    e <- readTVar etvar
    E.writeTempo e now t


handleTakenInEnsemble :: Text -> Text -> Transaction Bool
handleTakenInEnsemble uName eName = do
  when (uName == "") $ throwError "handleTakenInEnsemble called with empty uName"
  when (eName == "") $ throwError "handleTakenInEnsemble called with empty eName"
  e <- getEnsemble eName
  liftSTM $ E.nameTaken e uName


createEnsemble :: ClientHandle -> Password -> Name -> Password -> Password -> Maybe NominalDiffTime -> UTCTime -> Transaction ()
createEnsemble cHandle cpwd name hpwd ppwd expTime now = do
  when (not $ nameIsLegal name) $ throwError "ensemble name cannot contain spaces/tabs/newlines/control characters"
  when (not $ nameIsLegal hpwd) $ throwError "host password cannot contain spaces/tabs/newlines/control characters"
  when (not $ nameIsLegal ppwd) $ throwError "participant password cannot contain spaces/tabs/newlines/control characters"
  s <- ask
  oldMap <- liftSTM $ readTVar (ensembles s)
  when (isJust $ Map.lookup name oldMap) $ throwError "ensemble with same name already exists"
  let needsCommunityPwd = case expTime of Nothing -> True; Just expTime' -> expTime' > 3600
  when (needsCommunityPwd && cpwd == "") $ throwError "community password required but not provided"
  when (needsCommunityPwd && cpwd /= communityPassword s) $ throwError "incorrect community password"
  liftSTM $ do
    e <- E.newEnsembleS name now hpwd ppwd expTime
    etvar <- newTVar e
    let newMap = Map.insert name etvar oldMap
    writeTVar (ensembles s) newMap


deleteThisEnsemble :: TVar Client -> Text -> Transaction Text
deleteThisEnsemble ctvar opwd = do
  ename <- getEnsembleName ctvar
  e <- getEnsemble ename
  let op = E.ownerPassword e
  when ((opwd == "") && (op /= "")) $
    throwError "owner password not provided"
  when ((opwd /= op) && (op /= "")) $
    throwError "incorrect owner password"
  s <- ask
  liftSTM $ do
    oldMap <- readTVar (ensembles s)
    writeTVar (ensembles s) $ Map.delete ename oldMap
    removeAllClientsFromEnsemble s e
  return ename

removeAllClientsFromEnsemble :: ServerState -> E.EnsembleS -> STM ()
removeAllClientsFromEnsemble ss e = do
  allClients <- readTVar (clients ss)
  ensembleConnections <- readTVar (E.connections e)
  let ensembleClients = IntMap.intersection allClients ensembleConnections
  mapM_ (flip modifyTVar $ \x -> x { memberOfEnsemble = Nothing }) ensembleClients

deleteEnsemble :: ClientHandle -> Text -> Text -> Transaction ()
deleteEnsemble cHandle ename mpwd = do
  when (mpwd == "") $
    throwError "moderator password not provided"
  s <- ask
  when (mpwd /= moderatorPassword s) $
    throwError "incorrect moderator password"
  oldMap <- liftSTM $ readTVar (ensembles s)
  case Map.lookup ename oldMap of
    Nothing -> throwError "no ensemble by that name"
    Just etvar -> liftSTM $ do
      writeTVar (ensembles s) $ Map.delete ename oldMap
      e <- readTVar etvar
      removeAllClientsFromEnsemble s e


joinEnsemble :: SQLite.Connection -> ClientHandle -> TVar Client -> Name -> Name -> Text -> Password -> Bool -> Transaction (IO ())
joinEnsemble db cHandle ctvar eName uName loc pwd isReauth = do
  etvar <- getEnsembleTVar eName
  e <- liftSTM $ readTVar etvar
  -- when client is requesting a specific user name in the ensemble, succeeds only if not already taken...
  when (uName /= "") $ do
    when (not $ nameIsLegal uName) $ throwError "handles must not contain spaces/tabs/newlines/control characters"
    handleTaken <- handleTakenInEnsemble uName eName
    when (handleTaken && not isReauth) $ throwError $ "handle " <> uName <> " already in use in ensemble " <> eName
  -- when the ensemble requires password for authentication, they provided one, but it doesn't match...
  let epwd = E.joinPassword e
  when (epwd /= "" && pwd /= "" && epwd /= pwd) $ throwError "incorrect ensemble password"
  -- if we get this far, the client's join attempt will succeed
  let authed = (epwd == "" || epwd == pwd)
  liftSTM $ do
    ws <- connection <$> readTVar ctvar
    when (uName /= "") $ E.addNamedConnection cHandle uName ws e
    when (uName == "") $ E.addAnonymousConnection cHandle ws e
    modifyTVar ctvar $ \c -> c {
      memberOfEnsemble = Just etvar,
      handleInEnsemble = uName,
      locationInEnsemble = loc,
      statusInEnsemble = "",
      authenticatedInEnsemble = authed
    }
  t <- liftSTM $ E.readTempo e
  zs <- liftSTM $ E.readZones e
  vs <- liftSTM $ E.readViews e
  self <- liftSTM $ readTVar ctvar
  e' <- liftSTM $ readTVar etvar -- deliberate reread
  ss <- ask
  return $ do

    -- send responses to this client indicating successful join, and ensemble tempo, defs and views
    let respond = sendThisClient ss db cHandle (connection self)
    when (not isReauth) $ respond $ JoinedEnsemble eName uName loc pwd
    when (isReauth) $ respond $ ResponseOK "rejoined ensemble"
    respond $ EnsembleResponse $ TempoRcvd t
    mapM_ respond $ fmap EnsembleResponse $ IntMap.mapWithKey ZoneRcvd zs
    mapM_ respond $ fmap EnsembleResponse $ Map.mapWithKey ViewRcvd vs

    -- send new participant information about existing participants
    ps <- getNamedParticipants ss e'
    mapM_ respond $ fmap (EnsembleResponse . ParticipantUpdate) ps
    anonN <- readTVarIO $ E.anonymousConnections e'
    respond  $ EnsembleResponse $ AnonymousParticipants anonN

    -- send information about new participant to all of the other clients in this ensemble
    let respondEnsemble = sendEnsembleNoOrigin ss db cHandle e
    case uName of
      "" -> do
        respondEnsemble $ EnsembleResponse $ AnonymousParticipants anonN
      _ -> respondEnsemble $ EnsembleResponse $ ParticipantJoins (clientToParticipant self)


leaveEnsemble :: SQLite.Connection -> TVar Client -> Transaction (IO ())
leaveEnsemble db ctvar = do
  clientBefore  <- liftSTM $ readTVar ctvar
  let cHandle = Estuary.Types.Client.handle clientBefore
  case memberOfEnsemble clientBefore of
    Nothing -> return $ return ()
    Just etvar -> do
      e <- liftSTM $ readTVar etvar
      liftSTM $ do
        E.deleteConnection cHandle e
        modifyTVar ctvar $ \x -> x {
          memberOfEnsemble = Nothing,
          handleInEnsemble = "",
          locationInEnsemble = "",
          statusInEnsemble = "",
          authenticatedInEnsemble = False
          }
      ss <- ask
      return $ notifyWhenClientDepartsEnsemble ss db cHandle clientBefore


notifyWhenClientDepartsEnsemble :: ServerState -> SQLite.Connection -> ClientHandle -> Client -> IO ()
notifyWhenClientDepartsEnsemble ss db originHandle c = do
  case memberOfEnsemble c of
    Nothing -> return ()
    Just etvar -> do
      let uName = handleInEnsemble c
      let anonymous = uName == ""
      e <- readTVarIO etvar
      let eName = E.ensembleName e
      case uName of
        "" -> do
          n <- readTVarIO $ E.anonymousConnections e
          postLog db originHandle $ "(anonymous) leaving ensemble " <> eName
          sendEnsembleNoOrigin ss db originHandle e $ EnsembleResponse $ AnonymousParticipants n
        otherwise -> do
          postLog db originHandle $ uName <> " leaving ensemble " <> eName
          sendEnsembleNoOrigin ss db originHandle e $ EnsembleResponse $ ParticipantLeaves uName


updateLastEdit :: TVar Client -> UTCTime -> Transaction Participant
updateLastEdit ctvar now = liftSTM $ do
  c <- readTVar ctvar
  let c' = c { lastEditInEnsemble = now }
  writeTVar ctvar c'
  return $ clientToParticipant c'


isAuthenticatedInEnsemble :: TVar Client -> Transaction Bool
isAuthenticatedInEnsemble ctvar = do
  x <- liftSTM $ memberOfEnsemble <$> readTVar ctvar
  case x of
    Just _ -> return True
    Nothing -> return False

whenAuthenticatedInEnsemble :: TVar Client -> Transaction a -> Transaction ()
whenAuthenticatedInEnsemble ctvar t = do
  x <- isAuthenticatedInEnsemble ctvar
  when x $ void t
  return ()

whenNotAuthenticatedInEnsemble :: TVar Client -> Transaction a -> Transaction ()
whenNotAuthenticatedInEnsemble ctvar t = do
  x <- isAuthenticatedInEnsemble ctvar
  when (not x) $ void t
  return ()


send :: ServerState -> SQLite.Connection -> ClientHandle -> ClientHandle -> WS.Connection -> Response -> IO ()
send ss db originHandle destHandle c x = do
  y <- try $ WS.sendTextData c $ encode x
  case y of
    Right x -> return ()
    Left (SomeException e) -> do
      let ce = fromException (SomeException e)
      case ce of
        Just (WS.CloseRequest _ _) ->
          removeAnotherClient ss db originHandle destHandle $ "CloseRequest exception during send"
        Just WS.ConnectionClosed ->
          removeAnotherClient ss db originHandle destHandle $ "ConnectionClosed exception during send"
        otherwise ->
          postLog db originHandle $ "unusual exception sending to (" <> showt destHandle <> ") : " <> (T.pack $ show e)

sendThisClient :: ServerState -> SQLite.Connection -> ClientHandle -> WS.Connection -> Response -> IO ()
sendThisClient ss db originHandle c x = send ss db originHandle originHandle c x

sendClients :: ServerState -> SQLite.Connection -> ClientHandle -> IntMap.IntMap WS.Connection -> Response -> IO ()
sendClients ss db originHandle cs r = do
  IntMap.traverseWithKey (\k v -> send ss db originHandle k v r) cs
  return ()

sendEnsemble :: ServerState -> SQLite.Connection -> ClientHandle -> E.EnsembleS -> Response -> IO ()
sendEnsemble ss db cHandle e r = do
  cs <- E.readConnections e
  sendClients ss db cHandle cs r

sendEnsembleNoOrigin :: ServerState -> SQLite.Connection -> ClientHandle -> E.EnsembleS -> Response -> IO ()
sendEnsembleNoOrigin ss db cHandle e r = do
  cs <- E.readConnectionsNoOrigin e cHandle
  sendClients ss db cHandle cs r
