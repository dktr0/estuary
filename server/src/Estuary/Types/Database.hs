{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.Database where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok
import Data.Map.Strict
import Data.Time.Clock
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Lazy

import Estuary.Types.View
import Estuary.Types.EnsembleS
import Estuary.Types.Ensemble

openDatabase :: IO Connection
openDatabase = do
  c <- open "Estuary.db"
  createLogTable c
  createEnsembleTable c
  postLogToDatabase c "database opened"
  return c

createEnsembleTable :: Connection -> IO ()
createEnsembleTable c = execute_ c "CREATE TABLE IF NOT EXISTS ensembles (name TEXT, json TEXT, lastUpdate TEXT)"

createLogTable :: Connection -> IO ()
createLogTable c = execute_ c "CREATE TABLE IF NOT EXISTS log (time TEXT,msg TEXT)"

postLogToDatabase :: Connection -> Text -> IO ()
postLogToDatabase c l = do
  now <- getCurrentTime
  T.putStrLn $ (T.pack $ show now) <> ": " <> l
  execute c "INSERT INTO log (time,msg) VALUES (?,?)" (now,l)

writeNewEnsembleS :: Connection -> Text -> EnsembleS -> IO ()
writeNewEnsembleS c eName e = do
  now <- getCurrentTime
  execute c "INSERT INTO ensembles (name,json,lastUpdate) VALUES (?,?,?)" (eName,e,now)

writeEnsembleS :: Connection -> Text -> EnsembleS -> IO ()
writeEnsembleS c eName e = do
  now <- getCurrentTime
  execute c "UPDATE ensembles SET json=?, lastUpdate=? WHERE name=?" (e,now,eName)

instance ToField EnsembleS where
  toField = SQLText . Lazy.toStrict . Lazy.decodeUtf8 . encode

instance FromField EnsembleS where
  fromField = f . eitherDecode . g . fieldData
    where g (SQLText t) = Lazy.encodeUtf8 $ Lazy.fromStrict t
          g _ = Lazy.encodeUtf8 ""
          f (Right x) = Database.SQLite.Simple.Ok.Ok x
          f (Left x) = error x

readEnsembles :: Connection -> IO (Map Text EnsembleS)
readEnsembles c = do
  r <- query_ c "SELECT name,json FROM ensembles" -- [(n,j)]
  return $ fromList r

closeDatabase :: Connection -> IO ()
closeDatabase = close
