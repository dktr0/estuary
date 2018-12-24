module Estuary.Types.Transaction where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map
import qualified Database.SQLite.Simple as SQLite
import qualified Network.WebSockets as WS

import qualified Estuary.Types.Ensemble as E


-- | Transactions in the Estuary server are IO computations that can fail with String error
-- messages, read the database and a handle for the client making the current request, and
-- can modify the global state of the server. This is implemented as a stack of monad transformers,
-- so Transaction a will be an instance of MonadState, MonadError, and MonadReader. 

type Transaction = StateT Server (ReaderT (SQLite.Connection,ClientHandle) (ExceptT String IO))

runTransaction ::  Transaction a -> SQLite.Connection -> ClientHandle -> Server -> IO Server
runTransaction t db cHandle s = do
  e <- runExceptT (runReaderT (runStateT t s) (db,cHandle)) -- IO (Either String (a,Server))
       `catch` \ex -> return $ Left $ "IO exception: " ++ (show ex)
  case e of
    Left x -> postLogToDatabase db x >> putStrLn x >> return s
    Right (_,x) -> return x  

log :: String -> Transaction ()
log msg = do
  db' <- asks fst
  postLogToDatabase db msg
  liftIO $ putStrLn msg

justOrError :: Maybe a -> String -> Transaction a
justOrError x e t = maybe (throwError e) return 

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
getEnsemble x = do
  eName <- getEnsembleName
  justOrError (Map.lookup eName $ ensembles s) $ "***strange error*** getEnsemble for non-existent ensemble"

modifyEnsemble :: (E.Ensemble -> E.Ensemble) -> Transaction ()
modifyEnsemble f = do
  eName <- getEnsembleName
  e <- getEnsemble
  let e' = f e
  modify' $ \s -> s { ensembles = insert eName e' (ensembles s) }

close :: String -> Transaction ()
close msg = do
  log $ "closing connection: " ++ msg
  deleteClient h ----- TODO: complete the refactoring here...
  return ()

isAuthenticated :: Transaction Bool
isAuthenticated = getClient >>= return . authenticated

isAuthenticatedInEnsemble :: Transaction Bool
isAuthenticatedInEnsemble = getClient >>= return . authenticatedInEnsemble

whenAuthenticated :: Transaction a -> Transaction ()
whenAuthenticated t = do
  x <- isAuthenticated
  when x t
  return ()

whenNotAuthenticated :: Transaction a -> Transaction ()
whenNotAuthenticated t = do
  x <- isAuthenticated
  when (not x) t
  return ()

whenAuthenticatedInEnsemble :: Transaction a -> Transaction ()
whenAuthenticatedInEnsemble t = do
  x <- isAuthenticatedInEnsemble
  when x t
  return ()

whenNotAuthenticatedInEnsemble :: Transaction a -> Transaction ()
whenNotAuthenticatedInEnsemble t = do
  x <- isAuthenticatedInEnsemble
  when (not x) t
  return ()

send :: Response -> [Client] -> Transaction ()
send x cs = forM_ cs $ \y -> do
  liftIO $ (WS.sendTextData (connection y) $ (T.pack . encodeStrict) x)  --- **** this is probably not right
  `catch` \(SomeException e) -> throwError $ "send exception: " ++ (show e) -- **** because of throwError in IO exception handler...

respond :: Response -> Transaction ()
respond x = do
  c <- getClient
  send x [c] 

respondAll :: Response -> Transaction ()
respondAll x = gets (Map.elems . clients) >>= send x

respondAllNoOrigin :: Response -> Transaction ()
respondAllNoOrigin x = do 
  cHandle <- asks snd
  cs <- gets (Map.elems . Map.delete cHandle . clients)
  send x cs

respondEnsemble :: Response -> Transaction ()
respondEnsemble x = do
  c <- getClient
  cs <- gets (Map.elems . ensembleFilter (ensemble c) . clients)
  send x cs

respondEnsembleNoOrigin :: Response -> Transaction ()
respondEnsembleNoOrigin = do
  cHandle <- asks snd
  c <- getClient
  cs <- gets (Map.elems . Map.delete cHandle . ensembleFilter (ensemble c) . clients)
  send x cs

ensembleFilter :: Maybe String -> Map.Map ClientHandle Client -> Map.Map ClientHandle Client
ensembleFilter (Just e) = Map.filter $ (==(Just e)) . ensemble
ensembleFilter Nothing = Map.empty

saveNewEnsembleToDatabase :: String -> Transaction ()
saveNewEnsembleToDatabase name = do
  s <- get
  e <- justOrError (lookup name $ ensembles s) $ "***strange error*** saveNewEnsembleToDatabase for non-existent ensemble"
  db <- asks fst
  liftIO $ writeNewEnsemble db name (fromJust e) -- TODO: maybe rework this so any errors/exceptions in database access are thrown

saveEnsembleToDatabase :: String Transaction ()
saveEnsembleToDatabase name = do
  e <- getEnsemble
  eName <- getEnsembleName
  db <- asks fst
  liftIO $ writeEnsemble db eName e -- TODO: maybe rework this so any errors/exceptions in database access are thrownN




