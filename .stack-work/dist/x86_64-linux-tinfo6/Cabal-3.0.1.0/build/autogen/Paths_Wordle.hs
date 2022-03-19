{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Wordle (
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

bindir     = "/home/angel/Documents/Universidad/zLenguajesDeProgramacion/laboratorio/wordle/.stack-work/install/x86_64-linux-tinfo6/7d9e5ddf6d9ea460f0a7a09d65f50efa76488af9842ccae18ba4ab26af08f919/8.8.4/bin"
libdir     = "/home/angel/Documents/Universidad/zLenguajesDeProgramacion/laboratorio/wordle/.stack-work/install/x86_64-linux-tinfo6/7d9e5ddf6d9ea460f0a7a09d65f50efa76488af9842ccae18ba4ab26af08f919/8.8.4/lib/x86_64-linux-ghc-8.8.4/Wordle-0.1.0.0-LVUawUWajZZ6hUuJDrIM1K"
dynlibdir  = "/home/angel/Documents/Universidad/zLenguajesDeProgramacion/laboratorio/wordle/.stack-work/install/x86_64-linux-tinfo6/7d9e5ddf6d9ea460f0a7a09d65f50efa76488af9842ccae18ba4ab26af08f919/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/angel/Documents/Universidad/zLenguajesDeProgramacion/laboratorio/wordle/.stack-work/install/x86_64-linux-tinfo6/7d9e5ddf6d9ea460f0a7a09d65f50efa76488af9842ccae18ba4ab26af08f919/8.8.4/share/x86_64-linux-ghc-8.8.4/Wordle-0.1.0.0"
libexecdir = "/home/angel/Documents/Universidad/zLenguajesDeProgramacion/laboratorio/wordle/.stack-work/install/x86_64-linux-tinfo6/7d9e5ddf6d9ea460f0a7a09d65f50efa76488af9842ccae18ba4ab26af08f919/8.8.4/libexec/x86_64-linux-ghc-8.8.4/Wordle-0.1.0.0"
sysconfdir = "/home/angel/Documents/Universidad/zLenguajesDeProgramacion/laboratorio/wordle/.stack-work/install/x86_64-linux-tinfo6/7d9e5ddf6d9ea460f0a7a09d65f50efa76488af9842ccae18ba4ab26af08f919/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Wordle_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Wordle_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Wordle_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Wordle_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Wordle_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Wordle_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
