{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad
import Control.Monad.Except
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite
import System.Environment (getArgs)

import EstuaryServer(runServerWithDatabase)
import Estuary.Types.Database(openDatabase, closeDatabase)
import Estuary.Types.Name

main :: IO ()
main = do
  db <- openDatabase
  x <- processArgs <$> getArgs
  case x of
    Right (mpwd,cpwd,port) -> do
      runServerWithDatabase (T.pack mpwd) (T.pack cpwd) port False db -- False is hard-coded http redirect for now, will add an option later
      `catch` (closeDatabaseOnException db)
    Left e -> putStrLn $ "error: " ++ e

processArgs :: [String] -> Either String (String,String,Int)
processArgs xs = do
  when (length xs < 2) $ throwError "2 or 3 arguments must be provided: moderatorPassword communityPassword"
  let mpwd = xs!!0
  when (not $ nameIsLegal $ T.pack mpwd) $ throwError "moderator password must not contain spaces/newlines/control chars"
  let cpwd = xs!!1
  when (not $ nameIsLegal $ T.pack cpwd) $ throwError "community password must not contain spaces/newlines/control chars"
  let port = if length xs < 3 then 443 else read (xs!!2)
  return (mpwd,cpwd,port)

closeDatabaseOnException :: SQLite.Connection -> SomeException -> IO ()
closeDatabaseOnException db e = do
  putStrLn $ "quitting due to unhandled exception (" ++ show e ++ ")..."
  closeDatabase db
  putStrLn "database connection closed."
