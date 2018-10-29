module Main where

import Control.Exception

import qualified Database.SQLite.Simple as SQLite

import EstuaryServer(runServerWithDatabase, postLog)
import Estuary.Types.Database(openDatabase, closeDatabase)

import System.Environment (getArgs)

main :: IO ()
main = do
  db <- openDatabase
  (pswd, port) <- getArgs >>= return . processArgs
  runServerWithDatabase pswd port db 
    `catch` (closeDatabaseOnException db)

processArgs :: [String] -> (String,Int) -- (password,port)
processArgs xs = case length xs of
  0 -> ("",8002)
  1 -> (xs!!0,8002)
  _ -> (xs!!0,read (xs!!1))

closeDatabaseOnException :: SQLite.Connection -> SomeException -> IO ()
closeDatabaseOnException db e = do
  postLog db $ "quitting due to unhandled exception (" ++ (show e) ++ ")..."
  closeDatabase db
  putStrLn "database connection closed."