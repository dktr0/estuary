{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite
import System.Environment (getArgs)

import EstuaryServer(runServerWithDatabase)
import Estuary.Types.Database(openDatabase, closeDatabase,postLogToDatabase)

main :: IO ()
main = do
  db <- openDatabase
  (pswd, port) <- getArgs >>= return . processArgs
  runServerWithDatabase (T.pack pswd) port db
    `catch` (closeDatabaseOnException db)

processArgs :: [String] -> (String,Int) -- (password,port)
processArgs xs = case length xs of
  0 -> ("",8002)
  1 -> (xs!!0,8002)
  _ -> (xs!!0,read (xs!!1))

closeDatabaseOnException :: SQLite.Connection -> SomeException -> IO ()
closeDatabaseOnException db e = do
  postLogToDatabase db $ "quitting due to unhandled exception (" <> (T.pack $ show e) <> ")..."
  closeDatabase db
  putStrLn "database connection closed."
