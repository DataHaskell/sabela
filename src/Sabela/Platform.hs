{-# LANGUAGE CPP #-}

{- | Platform-specific filesystem conventions, kept in one place so the
rest of the code stays branch-free. The only divergence today is where a
Python virtualenv puts its executables: POSIX uses @bin/python3@ and
@bin/pip@; Windows uses @Scripts\\python.exe@ and @Scripts\\pip.exe@.
-}
module Sabela.Platform (
    systemPython,
    venvPythonPath,
    venvPipPath,
) where

import System.FilePath ((</>))

{- | Name of the system Python interpreter to look up on @PATH@ when no
virtualenv is present (@python3@ on POSIX, @python@ on Windows).
-}
systemPython :: String

-- | The Python interpreter inside a virtualenv directory.
venvPythonPath :: FilePath -> FilePath

-- | The pip executable inside a virtualenv directory.
venvPipPath :: FilePath -> FilePath

#if defined(mingw32_HOST_OS)
systemPython = "python"
venvPythonPath venvDir = venvDir </> "Scripts" </> "python.exe"
venvPipPath venvDir = venvDir </> "Scripts" </> "pip.exe"
#else
systemPython = "python3"
venvPythonPath venvDir = venvDir </> "bin" </> "python3"
venvPipPath venvDir = venvDir </> "bin" </> "pip"
#endif
