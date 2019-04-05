module Paths_challenge5 (
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

bindir     = "/home/domand/Documents/adventOfCode2017/challenge5/.cabal-sandbox/bin"
libdir     = "/home/domand/Documents/adventOfCode2017/challenge5/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/challenge5-0.1.0.0-Bpy64mc4kmo2AqkjwRZjkt"
datadir    = "/home/domand/Documents/adventOfCode2017/challenge5/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/challenge5-0.1.0.0"
libexecdir = "/home/domand/Documents/adventOfCode2017/challenge5/.cabal-sandbox/libexec"
sysconfdir = "/home/domand/Documents/adventOfCode2017/challenge5/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "challenge5_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "challenge5_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "challenge5_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "challenge5_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "challenge5_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
