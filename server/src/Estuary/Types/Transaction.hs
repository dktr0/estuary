module Estuary.Types.Transaction where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception
import Data.Map.Strict as Map
import Text.JSON
import qualified Database.SQLite.Simple as SQLite
import qualified Network.WebSockets as WS
import qualified Data.Text as T

import Estuary.Types.Server
import qualified Estuary.Types.Ensemble as E
import Estuary.Types.Client
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.Database

-- | Transactions in the Estuary server are IO computations that can fail with String error
-- messages, read the database connection and a handle for the client making the current request, and
-- can modify the global state of the server. This is implemented as a stack of monad transformers,
-- so Transaction a will be an instance of MonadState, MonadError, and MonadReader. 

type Transaction = StateT Server (ReaderT (SQLite.Connection,ClientHandle) (ExceptT String IO))

runTransaction ::  Transaction a -> SQLite.Connection -> ClientHandle -> Server -> IO Server
runTransaction t db cHandle s = do
  e <- try (runExceptT (runReaderT (runStateT t s) (db,cHandle)))
  case e of
    Right (Right (_,x)) -> return x -- successful transaction
    Right (Left x) -> do -- transaction with error (returns previous server state)
      postLogToDatabase db x
      return s
    Left (SomeException e) -> do -- unhandled exception in transaction (returns previous server state)
      let x = "runTransaction caught unhandled exception: " ++ show e
      postLogToDatabase db x
      return s

postLog :: String -> Transaction ()
postLog msg = do
  db <- asks fst
  liftIO $ postLogToDatabase db msg

justOrError :: Maybe a -> String -> Transaction a
justOrError x e = maybe (throwError e) return x

getClient :: Transaction Client
getClient = do
  s <- get
  cHandle <- asks snd
  justOrError (Map.lookup cHandle $ clients s) "***strange error*** current client not found in Server"
  
modifyClient :: (Client -> Client) -> Transaction ()
modifyClient f = do
  c <- getClient
  let c' = f c
  cHandle <- asks snd
  modify' $ \s -> s { clients = insert cHandle c' (clients s) }

getEnsembleName :: Transaction String
getEnsembleName = do
  c <- getClient
  justOrError (ensemble c) $ "***strange error*** getEnsembleName for client not in ensemble"
  
getEnsemble :: Transaction E.Ensemble
getEnsemble = do
  s <- get
  eName <- getEnsembleName
  justOrError (Map.lookup eName $ ensembles s) $ "***strange error*** getEnsemble for non-existent ensemble"

modifyEnsemble :: (E.Ensemble -> E.Ensemble) -> Transaction ()
modifyEnsemble f = do
  eName <- getEnsembleName
  e <- getEnsemble
  let e' = f e
  modify' $ \s -> s { ensembles = insert eName e' (ensembles s) }

broadcastServerClientCount :: Transaction ()
broadcastServerClientCount = gets (size . clients) >>= respondAll . ServerClientCount

close :: String -> Transaction ()
close msg = do
  postLog $ "closing connection: " ++ msg
  cHandle <- asks snd
  modify' $ deleteClient cHandle
  return ()

closeAnotherConnection :: Client -> Transaction ()
closeAnotherConnection c = do
  postLog $ "closing connection (for another client)"
  let cHandle = Estuary.Types.Client.handle c
  modify' $ deleteClient cHandle
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
  y <- liftIO $ try (WS.sendTextData (connection c) $ (T.pack . encodeStrict) x) 
  case y of
    Right x -> return ()
    Left (SomeException e) -> do
      let ce = fromException (SomeException e)
      case ce of 
        Just (WS.CloseRequest _ _) -> closeAnotherConnection c
        Just WS.ConnectionClosed -> closeAnotherConnection c
        otherwise -> throwError $ "send exception: " ++ show e 

respond :: Response -> Transaction ()
respond x = do
  c <- getClient
  send x [c] 

respondAll :: Response -> Transaction ()
respondAll x = gets (elems . clients) >>= send x

respondAllNoOrigin :: Response -> Transaction ()
respondAllNoOrigin x = do 
  cHandle <- asks snd
  cs <- gets (elems . delete cHandle . clients)
  send x cs

respondEnsemble :: Response -> Transaction ()
respondEnsemble x = do
  c <- getClient
  cs <- gets (elems . ensembleFilter (ensemble c) . clients)
  send x cs

respondEnsembleNoOrigin :: Response -> Transaction ()
respondEnsembleNoOrigin x = do
  cHandle <- asks snd
  c <- getClient
  cs <- gets (elems . delete cHandle . ensembleFilter (ensemble c) . clients)
  send x cs

ensembleFilter :: Maybe String -> Map ClientHandle Client -> Map ClientHandle Client
ensembleFilter (Just e) = Map.filter $ (==(Just e)) . ensemble
ensembleFilter Nothing = const empty

saveNewEnsembleToDatabase :: String -> Transaction ()
saveNewEnsembleToDatabase name = do
  s <- get
  e <- justOrError (Map.lookup name $ ensembles s) $ "***strange error*** saveNewEnsembleToDatabase for non-existent ensemble"
  db <- asks fst
  liftIO $ writeNewEnsemble db name e 

saveEnsembleToDatabase :: Transaction ()
saveEnsembleToDatabase = do
  e <- getEnsemble
  eName <- getEnsembleName
  db <- asks fst
  liftIO $ writeEnsemble db eName e

