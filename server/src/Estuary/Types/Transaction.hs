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


type Transaction = ReaderT (ServerState,ClientHandle) (ExceptT Text STM)

askServerState :: Transaction ServerState
askServerState = asks fst

askClientHandle :: Transaction ClientHandle
askClientHandle = asks snd

liftSTM :: STM a -> Transaction a
liftSTM = lift . lift

runTransaction :: SQLite.Connection -> ServerState -> ClientHandle -> Transaction a -> IO a
runTransaction db ss cHandle t = do
  x <- try $ atomically $ runExceptT (runReaderT t (ss,cHandle))
  case x of
    Right (Right a) -> return a
    Right (Left e) -> postLogToDatabase db $ "(" <> showt cHandle <> ") ERROR in runTransaction: " <> e
    Left (SomeException e) -> postLogToDatabase db $ "(" <> showt cHandle <> ") EXCEPTION in runTransaction: " <> (T.pack $ show e)

runTransactionIO :: SQLite.Connection -> ServerState -> ClientHandle -> Transaction (IO a) -> IO a
runTransactionIO db ss cHandle t = do
  x <- try $ join $ atomically $ runExceptT (runReaderT t (ss,cHandle))
  case x of
    Right (Right a) -> return a
    Right (Left e) -> postLogToDatabase db $ "(" <> showt cHandle <> ") ERROR in runTransactionIO: " <> e
    Left (SomeException e) -> postLogToDatabase db $ "(" <> showt cHandle <> ") EXCEPTION in runTransactionIO: " <> (T.pack $ show e)

addClient :: UTCTime -> WS.Connection -> Transaction ClientHandle
addClient t x = do
  s <- askServerState
  liftSTM $ do
    oldMap <- readTVar (clients s)
    i <- readTVar (nextClientHandle s)
    c <- newTVar $ newClient t i x
    let newMap = IntMap.insert i c oldMap
    writeTVar (clients s) newMap
    writeTVar (nextClientHandle s) (i+1)
    return i

deleteClient :: ClientHandle -> Transaction ()
deleteClient h = do
  s <- askServerState
  liftSTM $ do
    oldMap <- readTVar (clients s)
    let newMap = IntMap.delete h oldMap
    writeTVar (clients s) newMap

readClient :: Transaction Client
readClient = do
  s <- askServerState
  cHandle <- askClientHandle
  cMap <- liftSTM $ readTVar (clients s)
  ctvar <- justOrError (IntMap.lookup cHandle cMap) "current client not found in Server"
  liftSTM $ readTVar ctvar

modifyClient :: (Client -> Client) -> Transaction ()
modifyClient f = do
  s <- askServerState
  cHandle <- askClientHandle
  cMap <- liftSTM $ readTVar (clients s)
  ctvar <- justOrError (IntMap.lookup cHandle cMap) "current client not found in Server"
  liftSTM $ do
    c <- readTVar ctvar
    writeTVar ctvar $ f c

readEnsembleName :: Transaction Text
readEnsembleName = do
  c <- readClient
  justOrError (memberOfEnsemble c) $ "readEnsembleName for client not in ensemble"

readEnsembleS :: Transaction E.EnsembleS
readEnsembleS = readEnsembleName >>= readEnsembleSbyName

readEnsembleSbyName :: Text -> Transaction E.EnsembleS
readEnsembleSbyName eName = do
  s <- askServerState
  eMap <- liftSTM $ readTVar (ensembles s)
  etvar <- justOrError (Map.lookup eName eMap) $ "ensemble " <> eName <> " not found in Server"
  liftSTM $ readTVar etvar

readAllEnsembleNames :: Transaction [Text]
readAllEnsembleNames = do
  s <- askServerState
  eMap <- liftSTM $ readTVar (ensembles s)
  return $ Map.keys eMap

modifyEnsembleS :: (E.EnsembleS -> E.EnsembleS) -> Transaction ()
modifyEnsembleS f = do
  c <- readClient
  when (isNothing $ memberOfEnsemble c) $ postLog "modifyEnsembleS for client not in ensemble"
  when (isJust $ memberOfEnsemble c) $ do
    let eName = fromJust $ memberOfEnsemble c
    s <- askServerState
    eMap <- liftSTM $ readTVar (ensembles s)
    etvar <- justOrError (Map.lookup eName eMap) $ "ensemble " <> eName <> " not found in Server"
    liftSTM $ do
      e <- readTVar etvar
      writeTVar etvar $ f e

authenticate :: Text -> Transaction Bool
authenticate x = do
  pwd <- administrativePassword <$> askServerState
  modifyClient $ \c -> c { authenticated = x == pwd }
  return x == pwd

writeZone :: Int -> Definition -> Transaction ()
writeZone zone def = modifyEnsembleS $ E.modifyEnsemble (E.writeZone zone def)

writeView :: Text -> View -> Transaction ()
writeView vName v = modifyEnsembleS $ E.modifyEnsemble (E.writeView vName v)

writeTempo :: Tempo -> Transaction ()
writeTempo t = modifyEnsembleS $ E.modifyEnsemble (E.writeTempo t)

handleTakenInEnsemble :: Text -> Text -> Transaction Bool
handleTakenInEnsemble uName eName = do
  when (uName == "") $ throwError "handleTakenInEnsemble called with empty uName"
  when (eName == "") $ throwError "handleTakenInEnsemble called with empty eName"
  s <- askServerState
  liftSTM $ do
    x <- readTVar $ clients s
    x' <- mapM readTVar x
    let x'' = IntMap.filter (\c -> memberOfEnsemble c == Just eName && handleInEnsemble c == uName) x'
    return $ IntMap.size x'' > 0

countAnonymousParticipants :: Transaction Int
countAnonymousParticipants = readEnsembleName >>= countAnonymousParticipantsInEnsemble

countAnonymousParticipantsInEnsemble :: Text -> Transaction Int
countAnonymousParticipantsInEnsemble eName = do
  s <- askServerState
  liftSTM $ do
    x <- readTVar $ clients s
    x' <- mapM readTVar x
    let x'' = IntMap.filter (\c -> memberOfEnsemble c == Just eName && handleInEnsemble c == "") x'
    return $ IntMap.size x''

getServerClientCount :: Transaction Int
getServerClientCount = do
  s <- askServerState
  cMap <- liftSTM $ readTVar (clients s)
  return $ IntMap.size cMap

createEnsemble :: Text -> Text -> UTCTime -> Transaction E.EnsembleS
createEnsemble name pwd now = do
  whenNotAuthenticated $ throwError "ignoring CreateEnsemble from non-authenticated client"
  s <- askServerState
  oldMap <- readTVar (ensembles s)
  when (isJust $ Map.lookup name oldMap) $ throwError "ignoring CreateEnsemble for duplicate ensemble name"
  newEns <- newTVar $ E.writePassword pwd $ E.emptyEnsembleS now
  let newMap = Map.insert name newEns oldMap
  writeTVar (ensembles s) newMap
  return newEns


-- *** TODO: rework this (contains response semantics that should be elsewhere)
leaveEnsemble :: Transaction ()
leaveEnsemble = do
  c <- readClient
  let e = memberOfEnsemble c
  when (isJust e) $ do
    -- notify all other members of the ensemble of this client's departure
    let uName = handleInEnsemble c
    let anonymous = uName == ""
    when (not anonymous) $ do
      postLog $ uName <> " leaving ensemble " <> fromJust e
      respondEnsembleNoOrigin $ EnsembleResponse $ ParticipantLeaves uName
    when anonymous $ do
      postLog $ "(anonymous) leaving ensemble " <> fromJust e
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

isAuthenticated :: Transaction Bool
isAuthenticated = readClient >>= return . authenticated

isAuthenticatedInEnsemble :: Transaction Bool
isAuthenticatedInEnsemble = readClient >>= return . authenticatedInEnsemble

whenAuthenticated :: Transaction a -> Transaction ()
whenAuthenticated t = do
  x <- isAuthenticated
  when x $ void t
  return ()

whenNotAuthenticated :: Transaction a -> Transaction ()
whenNotAuthenticated t = do
  x <- isAuthenticated
  when (not x) $ void t
  return ()

whenAuthenticatedInEnsemble :: Transaction a -> Transaction ()
whenAuthenticatedInEnsemble t = do
  x <- isAuthenticatedInEnsemble
  when x $ void t
  return ()

whenNotAuthenticatedInEnsemble :: Transaction a -> Transaction ()
whenNotAuthenticatedInEnsemble t = do
  x <- isAuthenticatedInEnsemble
  when (not x) $ void t
  return ()

-- *** TODO: this has been refactored but should probably be moved to EstuaryServer.hs
send :: SQLite.Connection -> [Client] -> Response -> IO ()
send db cs x = forM_ cs $ \c -> do
  y <- liftIO $ try (WS.sendTextData (connection c) $ encode x)
  case y of
    Right x -> return ()
    Left (SomeException e) -> do
      let ce = fromException (SomeException e)
      let ch = Estuary.Types.Client.handle c
      case ce of
        Just (WS.CloseRequest _ _) -> postLogToDatabase db $ "CloseRequest exception sending to (" <> showt ch <> ")"
        Just WS.ConnectionClosed -> postLogToDatabase db $ "ConnectionClosed exception sending to (" <> showt ch <> ")"
        otherwise -> postLogToDatabase db $ "unusual exception sending to (" <> showt ch <> ") : " <> (T.pack $ show e)

ensembleClients :: Transaction [Client]
ensembleClients = do
  c <- readClient
  s <- askServerState
  liftSTM $ do
    cMap <- readTVar (clients s)
    cMap' <- mapM readTVar cMap
    return $ IntMap.elems $ ensembleFilter (memberOfEnsemble c) cMap'

ensembleClientsNoOrigin :: Transaction [Client]
ensembleClientsNoOrigin x = do
  cHandle <- askClientHandle
  c <- readClient
  s <- askServerState
  liftSTM $ do
    cMap <- readTVar (clients s)
    cMap' <- mapM readTVar cMap
    return $ IntMap.elems $ IntMap.delete cHandle $ ensembleFilter (memberOfEnsemble c) cMap'

ensembleFilter :: Maybe Text -> IntMap.IntMap Client -> IntMap.IntMap Client
ensembleFilter (Just e) = IntMap.filter $ (==(Just e)) . memberOfEnsemble
ensembleFilter Nothing = const IntMap.empty


saveEnsembleToDatabase :: Transaction ()
saveEnsembleToDatabase = do
  e <- readEnsembleS
  eName <- readEnsembleName
  db <- askDatabase
  liftIO $ writeEnsembleS db eName e
