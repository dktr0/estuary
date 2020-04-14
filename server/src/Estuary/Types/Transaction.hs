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
      let newMap = IntMap.delete cHandle oldMap
      writeTVar (clients ss) newMap
      writeTVar (clientCount ss) (IntMap.size newMap)
      readTVar ctvar'
    Nothing -> throwError $ "client " <> showt cHandle <> "not found"


getEnsembleName :: TVar Client -> Transaction Text
getEnsembleName ctvar = do
  eName <- liftSTM $ memberOfEnsemble <$> readTVar ctvar
  justOrError eName "getEnsembleName for client not member of ensemble"


getEnsemble :: Text -> Transaction E.EnsembleS
getEnsemble eName = do
  s <- ask
  eMap <- liftSTM $ readTVar (ensembles s)
  etvar <- justOrError (Map.lookup eName eMap) $ "ensemble " <> eName <> " not found on Estuary server"
  liftSTM $ readTVar etvar


-- returns how many anonymous participants in the ensemble the given ClientHandle is a part of
-- if the given ClientHandle is not part of an ensemble, returns -1
countAnonymousParticipantsIO :: ServerState -> TVar Client -> IO Int
countAnonymousParticipantsIO ss ctvar = do
  c <- atomically $ readTVar ctvar
  case memberOfEnsemble c of
    Nothing -> return (-1)
    Just eName -> do
      cMap <- atomically $ readTVar $ clients ss
      cs <- mapM (atomically . readTVar) cMap
      return $ IntMap.size $ IntMap.filter (\x -> (handleInEnsemble x == "") && (memberOfEnsemble x == Just eName)) cs


writeZone :: UTCTime -> TVar Client -> Int -> Definition -> Transaction ()
writeZone now ctvar zone def = do
  x <- liftSTM $ memberOfEnsemble <$> readTVar ctvar
  case x of
    Just eName -> do
      e <- getEnsemble eName
      liftSTM $ E.writeZone e now zone def
    Nothing -> throwError "client not authenticated in ensemble"


writeView :: UTCTime -> TVar Client -> Text -> View -> Transaction ()
writeView now ctvar vName v = do
  x <- liftSTM $ memberOfEnsemble <$> readTVar ctvar
  case x of
    Just eName -> do
      e <- getEnsemble eName
      liftSTM $ E.writeView e now vName v
    Nothing -> throwError "client not authenticated in ensemble"


writeTempo :: UTCTime -> TVar Client -> Tempo -> Transaction ()
writeTempo now ctvar t = do
  x <- liftSTM $ memberOfEnsemble <$> readTVar ctvar
  case x of
    Just eName -> do
      e <- getEnsemble eName
      liftSTM $ E.writeTempo e now t
    Nothing -> throwError "client not authenticated in ensemble"


handleTakenInEnsemble :: Text -> Text -> Transaction Bool
handleTakenInEnsemble uName eName = do
  when (uName == "") $ throwError "handleTakenInEnsemble called with empty uName"
  when (eName == "") $ throwError "handleTakenInEnsemble called with empty eName"
  s <- ask
  liftSTM $ do
    x <- readTVar $ clients s
    x' <- mapM readTVar x -- *** TODO: this is bad
    let x'' = IntMap.filter (\c -> memberOfEnsemble c == Just eName && handleInEnsemble c == uName) x'
    return $ IntMap.size x'' > 0


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
    e <- E.newEnsembleS now hpwd ppwd expTime
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
  return ename


deleteEnsemble :: ClientHandle -> Text -> Text -> Transaction ()
deleteEnsemble cHandle ename mpwd = do
  when (mpwd == "") $
    throwError "moderator password not provided"
  s <- ask
  when (mpwd /= moderatorPassword s) $
    throwError "incorrect moderator password"
  oldMap <- liftSTM $ readTVar (ensembles s)
  when (isNothing $ Map.lookup ename oldMap) $
    throwError "no ensemble by that name"
  liftSTM $ writeTVar (ensembles s) $ Map.delete ename oldMap


joinEnsemble :: SQLite.Connection -> ClientHandle -> TVar Client -> Name -> Name -> Text -> Password -> Transaction (IO ())
joinEnsemble db cHandle ctvar eName uName loc pwd = do
  e <- getEnsemble eName
  -- when client is requesting a specific user name in the ensemble, succeeds only if not already taken...
  when (uName /= "") $ do
    when (not $ nameIsLegal uName) $ throwError "handles must not contain spaces/tabs/newlines/control characters"
    handleTaken <- handleTakenInEnsemble uName eName
    when handleTaken $ throwError $ "handle " <> uName <> " already in use in ensemble " <> eName
  -- when the ensemble requires password for authentication, they provided one, but it doesn't match...
  let epwd = E.joinPassword e
  when (epwd /= "" && pwd /= "" && epwd /= pwd) $ throwError "incorrect ensemble password"
  -- if we get this far, the client's join attempt will succeed
  let authed = (epwd == "" || epwd == pwd)
  liftSTM $ modifyTVar ctvar $ \c -> c {
    memberOfEnsemble = Just eName,
    handleInEnsemble = uName,
    locationInEnsemble = loc,
    statusInEnsemble = "",
    authenticatedInEnsemble = authed
  }
  t <- liftSTM $ E.readTempo e
  zs <- liftSTM $ E.readZones e
  vs <- liftSTM $ E.readViews e
  self <- liftSTM $ readTVar ctvar
  ss <- ask
  return $ do
    -- send responses to this client indicating successful join, and ensemble tempo, defs and views
    let respond = sendClient db cHandle self
    respond $ JoinedEnsemble eName uName
    respond $ EnsembleResponse $ TempoRcvd t
    mapM_ respond $ fmap EnsembleResponse $ IntMap.mapWithKey ZoneRcvd zs
    mapM_ respond $ fmap EnsembleResponse $ Map.mapWithKey ViewRcvd $ vs
    -- TODO: send new participant information about existing participants (they'll get *some* info on updates, anyway)
    -- send information about new participant to all clients in this ensemble
    let respondEnsemble = sendEnsemble db ss cHandle eName
    case uName of
      "" -> respondEnsemble $ EnsembleResponse $ ParticipantJoins uName (clientToParticipant self)
      otherwise -> do
        n <- countAnonymousParticipantsIO ss ctvar
        respondEnsemble $ EnsembleResponse $ AnonymousParticipants n


leaveEnsemble :: SQLite.Connection -> TVar Client -> Transaction (IO ())
leaveEnsemble db ctvar = do
  clientBefore  <- liftSTM $ readTVar ctvar
  liftSTM $ modifyTVar ctvar $ \x -> x {
    memberOfEnsemble = Nothing,
    handleInEnsemble = "",
    locationInEnsemble = "",
    statusInEnsemble = "",
    authenticatedInEnsemble = False
    }
  ss <- ask
  return $ do
    case memberOfEnsemble clientBefore of
      Nothing -> return ()
      Just eName -> do -- notify all other members of the ensemble of this client's departure
        let uName = handleInEnsemble clientBefore
        let anonymous = uName == ""
        let cHandle = Estuary.Types.Client.handle clientBefore
        case uName of
          "" -> do
            n <- countAnonymousParticipantsIO ss ctvar
            postLog db cHandle $ "(anonymous) leaving ensemble " <> eName
            sendEnsembleNoOrigin db ss cHandle eName $ EnsembleResponse $ AnonymousParticipants n
          otherwise -> do
            postLog db cHandle $ uName <> " leaving ensemble " <> eName
            sendEnsembleNoOrigin db ss cHandle eName $ EnsembleResponse $ ParticipantLeaves uName


updateLastEdit :: TVar Client -> UTCTime -> Transaction Participant
updateLastEdit ctvar now = liftSTM $ do
  modifyTVar ctvar $ \c -> c { lastEditInEnsemble = now }
  clientToParticipant <$> readTVar ctvar


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

sendThisClient :: SQLite.Connection -> ClientHandle -> WS.Connection -> Response -> IO ()
sendThisClient db originHandle c x = send db originHandle originHandle c x

sendClient :: SQLite.Connection -> ClientHandle -> Client -> Response -> IO ()
sendClient db originHandle c x = send db originHandle (Estuary.Types.Client.handle c) (connection c) x

sendClients :: SQLite.Connection -> ClientHandle -> [Client] -> Response -> IO ()
sendClients db originHandle cs r = mapM_ (\x -> sendClient db originHandle x r) cs

sendEnsemble :: SQLite.Connection -> ServerState -> ClientHandle -> Text -> Response -> IO ()
sendEnsemble db ss cHandle eName r = do
  cs <- getEnsembleClientsIO ss eName
  sendClients db cHandle cs r

sendEnsembleNoOrigin :: SQLite.Connection -> ServerState -> ClientHandle -> Text -> Response -> IO ()
sendEnsembleNoOrigin db ss cHandle eName r = do
  cs <- getEnsembleClientsNoOriginIO ss cHandle eName
  sendClients db cHandle cs r

getEnsembleClientsIO :: ServerState -> Text -> IO [Client]
getEnsembleClientsIO ss eName = do
  cMap <- atomically $ readTVar (clients ss)
  cMap' <- mapM (atomically . readTVar) cMap
  return $ IntMap.elems $ ensembleFilter (Just eName) cMap'

getEnsembleClientsNoOriginIO :: ServerState -> ClientHandle -> Text -> IO [Client]
getEnsembleClientsNoOriginIO ss cHandle eName = do
  cMap <- atomically $ readTVar (clients ss)
  cMap' <- mapM (atomically . readTVar) $ IntMap.delete cHandle cMap
  return $ IntMap.elems $ ensembleFilter (Just eName) cMap'

ensembleFilter :: Maybe Text -> IntMap.IntMap Client -> IntMap.IntMap Client
ensembleFilter (Just e) = IntMap.filter $ (==(Just e)) . memberOfEnsemble
ensembleFilter Nothing = const IntMap.empty
