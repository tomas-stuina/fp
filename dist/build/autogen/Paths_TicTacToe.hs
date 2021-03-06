{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_TicTacToe (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\tomas\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\tomas\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\TicTacToe-0.1.0.0-2GFTCrDE88EHXWV61MAl5D"
datadir    = "C:\\Users\\tomas\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\TicTacToe-0.1.0.0"
libexecdir = "C:\\Users\\tomas\\AppData\\Roaming\\cabal\\TicTacToe-0.1.0.0-2GFTCrDE88EHXWV61MAl5D"
sysconfdir = "C:\\Users\\tomas\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TicTacToe_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TicTacToe_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "TicTacToe_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TicTacToe_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TicTacToe_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
