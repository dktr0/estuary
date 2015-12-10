module Paths_src (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/gscale/.cabal/bin"
libdir     = "/Users/gscale/.cabal/lib/x86_64-osx-ghc-7.10.2/src-0.1-6KfRG9gfh1WGMYSdKSWZ49"
datadir    = "/Users/gscale/.cabal/share/x86_64-osx-ghc-7.10.2/src-0.1"
libexecdir = "/Users/gscale/.cabal/libexec"
sysconfdir = "/Users/gscale/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "src_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "src_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "src_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "src_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "src_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
