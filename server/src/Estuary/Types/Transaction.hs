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

import Estuary.Types.ServerState
import qualified Estuary.Types.Ensemble as E
import qualified Estuary.Types.EnsembleS as E
import Estuary.Types.Client
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.Database


type Transaction = StateT ServerState (ReaderT (SQLite.Connection,ClientHandle) (ExceptT Text IO))

runTransaction ::  Transaction a -> SQLite.Connection -> ClientHandle -> ServerState -> IO ServerState
runTransaction t db cHandle s = do
  e <- try (runExceptT (runReaderT (runStateT t s) (db,cHandle)))
  case e of
    Right (Right (_,x)) -> return x -- successful transaction
    Right (Left x) -> do -- transaction with error (returns previous server state)
      postLogToDatabase db x
      return s
    Left (SomeException e) -> do -- unhandled exception in transaction (returns previous server state)
      let x = "runTransaction caught unhandled exception: " <> (T.pack $ show e)
      postLogToDatabase db x
      return s

postLog :: Text -> Transaction ()
postLog msg = do
  db <- asks fst
  liftIO $ postLogToDatabase db msg

justOrError :: Maybe a -> Text -> Transaction a
justOrError x e = maybe (throwError e) return x

getClient :: Transaction Client
getClient = do
  s <- get
  cHandle <- asks snd
  justOrError (IntMap.lookup cHandle $ clients s) "***strange error*** current client not found in Server"

modifyClient :: (Client -> Client) -> Transaction ()
modifyClient f = do
  c <- getClient
  let c' = f c
  cHandle <- asks snd
  modify' $ \s -> s { clients = IntMap.insert cHandle c' (clients s) }

-- get the name of the ensemble the current connection is a member of, or fail
getEnsembleName :: Transaction Text -- !!! probably should rename to getNameOfClientsEnsemble or something like that
getEnsembleName = do
  c <- getClient
  justOrError (memberOfEnsemble c) $ "***strange error*** getEnsembleName for client not in ensemble"

-- get the data of the ensemble the current connection is a member of, or fail
getEnsembleS :: Transaction E.EnsembleS
getEnsembleS = do
  s <- get
  eName <- getEnsembleName
  justOrError (Map.lookup eName $ ensembles s) $ "***strange error*** getEnsembleS for non-existent ensemble"

getEnsembleSbyName :: Text -> Transaction E.EnsembleS
getEnsembleSbyName eName = do
  s <- get
  justOrError (Map.lookup eName $ ensembles s) $ "***strange error*** getEnsembleSbyName for non-existent ensemble"

-- modify the ensemble the current connection is a member of, or fail
modifyEnsembleS :: (E.EnsembleS -> E.EnsembleS) -> Transaction ()
modifyEnsembleS f = do
  eName <- getEnsembleName
  e <- getEnsembleS
  let e' = f e
  modify' $ \s -> s { ensembles = Map.insert eName e' (ensembles s) }

handleTakenInEnsemble :: Text -> Text -> Transaction Bool
handleTakenInEnsemble uName eName = do
  when (uName == "") $ throwError "*** strange error: handleTakenInEnsemble called with empty uName"
  when (eName == "") $ throwError "*** strange error: handleTakenInEnsemble called with empty eName"
  clientMap <- clients <$> get
  let clientMap' = IntMap.filter (\c -> memberOfEnsemble c == Just eName && handleInEnsemble c == uName) clientMap
  return $ IntMap.size clientMap' > 0

countAnonymousParticipants :: Transaction Int
countAnonymousParticipants = do
  eName <- getEnsembleName
  clientMap <- clients <$> get
  let clientMap' = IntMap.filter (\c -> memberOfEnsemble c == Just eName && handleInEnsemble c == "") clientMap
  return $ IntMap.size clientMap'

countAnonymousParticipantsInOtherEnsemble :: Text -> Transaction Int
countAnonymousParticipantsInOtherEnsemble eName = do
  clientMap <- clients <$> get
  let clientMap' = IntMap.filter (\c -> memberOfEnsemble c == Just eName && handleInEnsemble c == "") clientMap
  return $ IntMap.size clientMap'

getServerClientCount :: Transaction Int
getServerClientCount = gets (IntMap.size . clients)

leaveEnsemble :: Transaction ()
leaveEnsemble = do
  e <- memberOfEnsemble <$> getClient
  when (isJust e) $ do
    -- notify all other members of the ensemble of this client's departure
    uName <- handleInEnsemble <$> getClient
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
      n <- countAnonymousParticipantsInOtherEnsemble e'
      respondOtherEnsemble e' $ EnsembleResponse $ AnonymousParticipants (n-1)

close :: Text -> Transaction ()
close msg = do
  postLog $ "closing connection: " <> msg
  leaveEnsemble
  cHandle <- asks snd
  modify' $ deleteClient cHandle
  return ()

closeAnotherConnection :: Client -> Transaction ()
closeAnotherConnection c = do
  postLog $ "closing connection (for another client)"
  modify' $ deleteClient (Estuary.Types.Client.handle c)
  removeClientFromEnsemble c
  return ()

isAuthenticated :: Transaction Bool
isAuthenticated = getClient >>= return . authenticated

isAuthenticatedInEnsemble :: Transaction Bool
isAuthenticatedInEnsemble = getClient >>= return . authenticatedInEnsemble

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
      case ce of
        Just (WS.CloseRequest _ _) -> closeAnotherConnection c
        Just WS.ConnectionClosed -> closeAnotherConnection c
        otherwise -> throwError $ "send exception: " <> (T.pack $ show e)

respond :: Response -> Transaction ()
respond x = do
  c <- getClient
  send x [c]

respondAll :: Response -> Transaction ()
respondAll x = gets (IntMap.elems . clients) >>= send x

respondAllNoOrigin :: Response -> Transaction ()
respondAllNoOrigin x = do
  cHandle <- asks snd
  cs <- gets (IntMap.elems . IntMap.delete cHandle . clients)
  send x cs

respondEnsemble :: Response -> Transaction ()
respondEnsemble x = do
  c <- getClient
  cs <- gets (IntMap.elems . ensembleFilter (memberOfEnsemble c) . clients)
  send x cs

respondEnsembleNoOrigin :: Response -> Transaction ()
respondEnsembleNoOrigin x = do
  cHandle <- asks snd
  c <- getClient
  cs <- gets (IntMap.elems . IntMap.delete cHandle . ensembleFilter (memberOfEnsemble c) . clients)
  send x cs

respondOtherEnsemble :: Text -> Response -> Transaction ()
respondOtherEnsemble eName x = do
  cs <- gets (IntMap.elems . ensembleFilter (Just eName) . clients)
  send x cs

ensembleFilter :: Maybe Text -> IntMap.IntMap Client -> IntMap.IntMap Client
ensembleFilter (Just e) = IntMap.filter $ (==(Just e)) . memberOfEnsemble
ensembleFilter Nothing = const IntMap.empty

saveNewEnsembleToDatabase :: Text -> Transaction ()
saveNewEnsembleToDatabase name = do
  s <- get
  e <- justOrError (Map.lookup name $ ensembles s) $ "***strange error*** saveNewEnsembleToDatabase for non-existent ensemble"
  db <- asks fst
  liftIO $ writeNewEnsembleS db name e

saveEnsembleToDatabase :: Transaction ()
saveEnsembleToDatabase = do
  e <- getEnsembleS
  eName <- getEnsembleName
  db <- asks fst
  liftIO $ writeEnsembleS db eName e
