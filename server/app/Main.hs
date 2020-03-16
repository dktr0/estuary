{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite
import System.Environment (getArgs)

import EstuaryServer(runServerWithDatabase)
import Estuary.Types.Database(openDatabase, closeDatabase,postLogNoHandle)

main :: IO ()
main = do
  db <- openDatabase
  (pswd, port) <- getArgs >>= return . processArgs
  runServerWithDatabase (T.pack pswd) port False db -- False is hard-coded http redirect for now, will add an option later
    `catch` (closeDatabaseOnException db)

processArgs :: [String] -> (String,Int) -- (password,port)
processArgs xs = case length xs of
  0 -> ("",443)
  1 -> (xs!!0,443)
  _ -> (xs!!0,read (xs!!1))

closeDatabaseOnException :: SQLite.Connection -> SomeException -> IO ()
closeDatabaseOnException db e = do
  postLogNoHandle db $ "quitting due to unhandled exception (" <> (T.pack $ show e) <> ")..."
  closeDatabase db
  putStrLn "database connection closed."
