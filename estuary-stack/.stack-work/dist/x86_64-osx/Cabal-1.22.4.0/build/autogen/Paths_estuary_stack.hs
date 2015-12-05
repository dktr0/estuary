module Paths_estuary_stack (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/gscale/Documents/TidalProject/Estuary/estuary/estuary-stack/.stack-work/install/x86_64-osx/lts-3.16/7.10.2/bin"
libdir     = "/Users/gscale/Documents/TidalProject/Estuary/estuary/estuary-stack/.stack-work/install/x86_64-osx/lts-3.16/7.10.2/lib/x86_64-osx-ghc-7.10.2/estuary-stack-0.1.0.0-FPFXJ7xriwLCNEVZTgF7Yd"
datadir    = "/Users/gscale/Documents/TidalProject/Estuary/estuary/estuary-stack/.stack-work/install/x86_64-osx/lts-3.16/7.10.2/share/x86_64-osx-ghc-7.10.2/estuary-stack-0.1.0.0"
libexecdir = "/Users/gscale/Documents/TidalProject/Estuary/estuary/estuary-stack/.stack-work/install/x86_64-osx/lts-3.16/7.10.2/libexec"
sysconfdir = "/Users/gscale/Documents/TidalProject/Estuary/estuary/estuary-stack/.stack-work/install/x86_64-osx/lts-3.16/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "estuary_stack_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "estuary_stack_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "estuary_stack_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "estuary_stack_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "estuary_stack_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
