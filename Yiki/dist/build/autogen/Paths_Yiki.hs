module Paths_Yiki (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/quantumman/.cabal/bin"
libdir     = "/Users/quantumman/.cabal/lib/Yiki-0.0.0/ghc-7.0.3"
datadir    = "/Users/quantumman/.cabal/share/Yiki-0.0.0"
libexecdir = "/Users/quantumman/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "Yiki_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "Yiki_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "Yiki_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "Yiki_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
