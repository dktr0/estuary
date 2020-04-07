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
  x <- processArgs <$> getArgs
  case x of
    Just (mpwd,cpwd,port) -> do
      runServerWithDatabase (T.pack mpwd) (T.pack cpwd) port False db -- False is hard-coded http redirect for now, will add an option later
      `catch` (closeDatabaseOnException db)
    Nothing -> do
      putStrLn "error: 2 or 3 arguments must be provided: moderatorPassword communityPassword"

processArgs :: [String] -> Maybe (String,String,Int) -- (moderatorPassword,,port)
processArgs xs = case length xs of
  0 -> Nothing
  1 -> Nothing
  2 -> Just (xs!!0,xs!!1,443)
  _ -> Just (xs!!0,xs!!1,read (xs!!2))

closeDatabaseOnException :: SQLite.Connection -> SomeException -> IO ()
closeDatabaseOnException db e = do
  postLogNoHandle db $ "quitting due to unhandled exception (" <> (T.pack $ show e) <> ")..."
  closeDatabase db
  putStrLn "database connection closed."
