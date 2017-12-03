{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.Database where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok

import Data.Time.Clock

import Estuary.Types.Ensemble

openDatabase :: IO Connection
openDatabase = do
  c <- open "Estuary.db"
  createLogTable c
  createEnsembleTable c
  postLogToDatabase c "database opened"
  return c

createEnsembleTable :: Connection -> IO ()
createEnsembleTable c = execute_ c "CREATE TABLE IF NOT EXISTS ensembles (name TEXT, password TEXT, defs TEXT, views TEXT, defaultView TEXT, tempo TEXT)"

createLogTable :: Connection -> IO ()
createLogTable c = execute_ c "CREATE TABLE IF NOT EXISTS log (time TEXT,msg TEXT)"

postLogToDatabase :: Connection -> String -> IO ()
postLogToDatabase c l = do
  now <- getCurrentTime
  execute c "INSERT INTO log (time,msg) VALUES (?,?)" (now,l)

{-
readEnsembles :: Connection -> IO (Map String Ensemble)
readEnsembles c = do
  r <- query_ c "SELECT name,password,defs,views,defaultView,tempo FROM ensembles" -- [(n,e)]
  fromList r


writeEnsemble :: Connection -> String -> Ensemble -> IO ()
writeEnsemble c eName e = do
  execute c "UPDATE ensembles (password,defs,views,defaultView,tempo) VALUES (?,?,?,?,?) WHERE name=?" (e,n)
-}

closeDatabase :: Connection -> IO ()
closeDatabase = close
