module Paths_multichat (
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

bindir     = "D:\\applications\\stacki\\bin\\.stack-work\\install\\d66e132e\\bin"
libdir     = "D:\\applications\\stacki\\bin\\.stack-work\\install\\d66e132e\\lib\\x86_64-windows-ghc-7.10.3\\multichat-0.1.0.0-I6uTGAycWmqKClD0e3zHEy"
datadir    = "D:\\applications\\stacki\\bin\\.stack-work\\install\\d66e132e\\share\\x86_64-windows-ghc-7.10.3\\multichat-0.1.0.0"
libexecdir = "D:\\applications\\stacki\\bin\\.stack-work\\install\\d66e132e\\libexec"
sysconfdir = "D:\\applications\\stacki\\bin\\.stack-work\\install\\d66e132e\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "multichat_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "multichat_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "multichat_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "multichat_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "multichat_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
