module Paths_Multichat (
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

bindir     = "C:\\Applications\\stack\\.stack-work\\install\\x86_64-windows\\lts-3.4\\7.10.2\\bin"
libdir     = "C:\\Applications\\stack\\.stack-work\\install\\x86_64-windows\\lts-3.4\\7.10.2\\lib\\x86_64-windows-ghc-7.10.2\\Multichat-0.1.0.0-BJfyfPEVP0V9jAK0xkzh4Y"
datadir    = "C:\\Applications\\stack\\.stack-work\\install\\x86_64-windows\\lts-3.4\\7.10.2\\share\\x86_64-windows-ghc-7.10.2\\Multichat-0.1.0.0"
libexecdir = "C:\\Applications\\stack\\.stack-work\\install\\x86_64-windows\\lts-3.4\\7.10.2\\libexec"
sysconfdir = "C:\\Applications\\stack\\.stack-work\\install\\x86_64-windows\\lts-3.4\\7.10.2\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Multichat_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Multichat_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Multichat_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Multichat_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Multichat_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
