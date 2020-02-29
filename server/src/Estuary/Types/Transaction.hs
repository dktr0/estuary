{-# LANGUAGE OverloadedStrings #-}

-- | Transactions in the Estuary server are IO computations that can fail with String error
-- messages, read the database connection and a handle for the client making the current request, and
-- can modify the global state of the server. This is implemented as a stack of monad transformers,
-- so Transaction a will be an instance of MonadState, MonadError, and MonadReader.

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


type Transaction = ReaderT (SQLite.Connection,ClientHandle,ServerState) (ExceptT Text IO)

askDatabase :: Transaction SQLite.Connection
askDatabase = do
  (db,_,_) <- ask
  return db

askClientHandle :: Transaction ClientHandle
askClientHandle = do
  (_,c,_) <- ask
  return c

askServerState :: Transaction ServerState
askServerState = do
  (_,_,s) <- ask
  return s

runTransaction ::  ServerState -> SQLite.Connection -> ClientHandle -> Transaction a -> IO ()
runTransaction s db cHandle t = do
  e <- try (runExceptT (runReaderT t (db,cHandle,s)))
  case e of
    Right (Right _) -> return () -- successful transaction
    Right (Left x) -> postLogToDatabase db x -- transaction with error
    Left (SomeException e) -> postLogToDatabase db $ "(" <> showt cHandle <> ") runTransaction caught unhandled exception: " <> (T.pack $ show e) -- unhandled exception in transaction

postLog :: Text -> Transaction ()
postLog msg = do
  db <- askDatabase
  ch <- askClientHandle
  liftIO $ postLogToDatabase db $ "(" <> showt ch <> ") " <> msg

justOrError :: Maybe a -> Text -> Transaction a
justOrError x e = maybe (throwError e) return x

readClient :: Transaction Client
readClient = do
  s <- askServerState
  cHandle <- askClientHandle
  cMap <- liftIO $ atomically $ readTVar (clients s)
  ctvar <- justOrError (IntMap.lookup cHandle cMap) "***strange error*** current client not found in Server"
  liftIO $ atomically $ readTVar ctvar

modifyClient :: (Client -> Client) -> Transaction ()
modifyClient f = do
  s <- askServerState
  cHandle <- askClientHandle
  cMap <- liftIO $ atomically $ readTVar (clients s)
  ctvar <- justOrError (IntMap.lookup cHandle cMap) "***strange error*** current client not found in Server"
  liftIO $ atomically $ do
    c <- readTVar ctvar
    writeTVar ctvar $ f c

readEnsembleName :: Transaction Text
readEnsembleName = do
  c <- readClient
  justOrError (memberOfEnsemble c) $ "***strange error*** readEnsembleName for client not in ensemble"

readEnsembleS :: Transaction E.EnsembleS
readEnsembleS = readEnsembleName >>= readEnsembleSbyName

readEnsembleSbyName :: Text -> Transaction E.EnsembleS
readEnsembleSbyName eName = do
  s <- askServerState
  eMap <- liftIO $ atomically $ readTVar (ensembles s)
  etvar <- justOrError (Map.lookup eName eMap) $ "***strange error*** ensemble " <> eName <> " not found in Server"
  liftIO $ atomically $ readTVar etvar

readAllEnsembleNames :: Transaction [Text]
readAllEnsembleNames = do
  s <- askServerState
  eMap <- liftIO $ atomically $ readTVar (ensembles s)
  return $ Map.keys eMap

modifyEnsembleS :: (E.EnsembleS -> E.EnsembleS) -> Transaction ()
modifyEnsembleS f = do
  c <- readClient
  when (isNothing $ memberOfEnsemble c) $ postLog "***strange error*** modifyEnsembleS for client not in ensemble"
  when (isJust $ memberOfEnsemble c) $ do
    let eName = fromJust $ memberOfEnsemble c
    s <- askServerState
    eMap <- liftIO $ atomically $ readTVar (ensembles s)
    etvar <- justOrError (Map.lookup eName eMap) $ "***strange error*** ensemble " <> eName <> " not found in Server"
    liftIO $ atomically $ do
      e <- readTVar etvar
      writeTVar etvar $ f e

writeZone :: Int -> Definition -> Transaction ()
writeZone zone def = modifyEnsembleS $ E.modifyEnsemble (E.writeZone zone def)

writeView :: Text -> View -> Transaction ()
writeView vName v = modifyEnsembleS $ E.modifyEnsemble (E.writeView vName v)

writeTempo :: Tempo -> Transaction ()
writeTempo t = modifyEnsembleS $ E.modifyEnsemble (E.writeTempo t)

handleTakenInEnsemble :: Text -> Text -> Transaction Bool
handleTakenInEnsemble uName eName = do
  when (uName == "") $ throwError "*** strange error: handleTakenInEnsemble called with empty uName"
  when (eName == "") $ throwError "*** strange error: handleTakenInEnsemble called with empty eName"
  s <- askServerState
  clientMap <- liftIO $ atomically $ readTVar $ clients s
  clientMap' <- liftIO $ mapM (atomically . readTVar) clientMap
  let clientMap'' = IntMap.filter (\c -> memberOfEnsemble c == Just eName && handleInEnsemble c == uName) clientMap'
  return $ IntMap.size clientMap'' > 0

countAnonymousParticipants :: Transaction Int
countAnonymousParticipants = readEnsembleName >>= countAnonymousParticipantsInEnsemble

countAnonymousParticipantsInEnsemble :: Text -> Transaction Int
countAnonymousParticipantsInEnsemble eName = do
  s <- askServerState
  clientMap <- liftIO $ atomically $ readTVar $ clients s
  clientMap' <- liftIO $ mapM (atomically . readTVar) clientMap
  let clientMap'' = IntMap.filter (\c -> memberOfEnsemble c == Just eName && handleInEnsemble c == "") clientMap'
  return $ IntMap.size clientMap''

getServerClientCount :: Transaction Int
getServerClientCount = do
  s <- askServerState
  cMap <- liftIO $ atomically $ readTVar (clients s)
  return $ IntMap.size cMap

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

removeClientFromEnsemble :: Client -> Transaction ()
removeClientFromEnsemble c = do
  let e = memberOfEnsemble c
  when (isJust e) $ do
    let e' = fromJust e
    let uName = handleInEnsemble c
    let anonymous = uName == ""
    when (not anonymous) $ do
      postLog $ uName <> " removed from ensemble " <> e'
      respondOtherEnsemble e' $ EnsembleResponse $ ParticipantLeaves uName
    when anonymous $ do
      postLog $ "(anonymous) removed from ensemble " <> e'
      n <- countAnonymousParticipantsInEnsemble e'
      respondOtherEnsemble e' $ EnsembleResponse $ AnonymousParticipants (n-1)

close :: Text -> Transaction ()
close msg = do
  cHandle <- askClientHandle
  postLog $ "closing connection " <> showt cHandle <> ": " <> msg
  leaveEnsemble
  s <- askServerState
  liftIO $ deleteClient s cHandle


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

send :: Response -> [Client] -> Transaction ()
send x cs = forM_ cs $ \c -> do
  y <- liftIO $ try (WS.sendTextData (connection c) $ encode x)
  case y of
    Right x -> return ()
    Left (SomeException e) -> do
      let ce = fromException (SomeException e)
      let ch = Estuary.Types.Client.handle c
      case ce of
        Just (WS.CloseRequest _ _) -> throwError $ "CloseRequest exception sending to (" <> showt ch <> ")"
        Just WS.ConnectionClosed -> throwError $ "ConnectionClosed exception sending to (" <> showt ch <> ")"
        otherwise -> throwError $ "unusual exception sending to (" <> showt ch <> ") : " <> (T.pack $ show e)

respond :: Response -> Transaction ()
respond x = do
  c <- readClient
  send x [c]

-- respondAll :: Response -> Transaction ()
-- respondAll x = gets (IntMap.elems . clients) >>= send x

-- respondAllNoOrigin :: Response -> Transaction ()
-- respondAllNoOrigin x = do
--  cHandle <- asks snd
--  cs <- gets (IntMap.elems . IntMap.delete cHandle . clients)
--  send x cs

respondEnsemble :: Response -> Transaction ()
respondEnsemble x = do
  c <- readClient
  s <- askServerState
  cMap <- liftIO $ atomically $ readTVar (clients s)
  cMap' <- liftIO $ mapM (atomically . readTVar) cMap
  let cs = IntMap.elems $ ensembleFilter (memberOfEnsemble c) cMap'
  send x cs

respondEnsembleNoOrigin :: Response -> Transaction ()
respondEnsembleNoOrigin x = do
  cHandle <- askClientHandle
  c <- readClient
  s <- askServerState
  cMap <- liftIO $ atomically $ readTVar (clients s)
  cMap' <- liftIO $ mapM (atomically . readTVar) cMap
  let cs = IntMap.elems $ IntMap.delete cHandle $ ensembleFilter (memberOfEnsemble c) cMap'
  send x cs

respondOtherEnsemble :: Text -> Response -> Transaction ()
respondOtherEnsemble eName x = do
  s <- askServerState
  cMap <- liftIO $ atomically $ readTVar (clients s)
  cMap' <- liftIO $ mapM (atomically . readTVar) cMap
  let cs = IntMap.elems $ ensembleFilter (Just eName) cMap'
  send x cs

ensembleFilter :: Maybe Text -> IntMap.IntMap Client -> IntMap.IntMap Client
ensembleFilter (Just e) = IntMap.filter $ (==(Just e)) . memberOfEnsemble
ensembleFilter Nothing = const IntMap.empty

saveNewEnsembleToDatabase :: Text -> Transaction ()
saveNewEnsembleToDatabase name = do
  s <- askServerState
  eMap <- liftIO $ atomically $ readTVar (ensembles s)
  etvar <- justOrError (Map.lookup name eMap) $ "***strange error*** saveNewEnsembleToDatabase for non-existent ensemble"
  e <- liftIO $ atomically $ readTVar etvar
  db <- askDatabase
  liftIO $ writeNewEnsembleS db name e

saveEnsembleToDatabase :: Transaction ()
saveEnsembleToDatabase = do
  e <- readEnsembleS
  eName <- readEnsembleName
  db <- askDatabase
  liftIO $ writeEnsembleS db eName e
