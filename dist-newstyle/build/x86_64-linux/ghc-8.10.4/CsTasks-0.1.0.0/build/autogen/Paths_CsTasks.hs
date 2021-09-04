{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_CsTasks (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/me/.cabal/bin"
libdir     = "/home/me/.cabal/lib/x86_64-linux-ghc-8.10.4/CsTasks-0.1.0.0-inplace"
dynlibdir  = "/home/me/.cabal/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/me/.cabal/share/x86_64-linux-ghc-8.10.4/CsTasks-0.1.0.0"
libexecdir = "/home/me/.cabal/libexec/x86_64-linux-ghc-8.10.4/CsTasks-0.1.0.0"
sysconfdir = "/home/me/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "CsTasks_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CsTasks_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "CsTasks_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "CsTasks_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CsTasks_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CsTasks_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
