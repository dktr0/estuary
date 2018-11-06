import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

import Data.Maybe
import System.IO
import System.Environment
import Control.Monad
import System.Directory
import System.FilePath

main = do
  putStrLn "CUSTOM SETUP.HS"
  args <- getArgs
  forM args putStrLn
  defaultMainWithHooks $ simpleUserHooks {
    preBuild = preBuild'
  }

preBuild' :: Args -> BuildFlags -> IO HookedBuildInfo
preBuild' args flags = do
  putStrLn $ show flags
  let distPath = fromFlagOrDefault defaultDistPref $ buildDistPref flags
  absDistPath <- makeAbsolute distPath

  let allJsPathOf t = absDistPath </> "build" </> t </> (t ++ ".jsexe") </> "all.js"
  
  let tests = 
        catMaybes $ (flip fmap) (buildArgs flags) $ \t -> case t of
          't':'e':'s':'t':':':exe -> Just exe
          _ -> Nothing

  putStrLn "ALL TEST SCRIPTS"
  forM tests $ putStrLn . allJsPathOf

  let exeInfos = (flip fmap) tests $ \path -> emptyBuildInfo {
          ccOptions = ["-DTEST_SUITE_EXE=\"" ++ path ++ "\""]
        }
  return (Nothing, zip (fmap (\x -> 't':'e':'s':'t':':':x) tests) exeInfos)