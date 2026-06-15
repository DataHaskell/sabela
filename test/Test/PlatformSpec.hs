{-# LANGUAGE CPP #-}

{- | Pins the platform-specific virtualenv layout so a refactor of
'Sabela.Platform' cannot silently flip the paths the Python backend and
installer rely on. Each expectation is evaluated for the host the suite
runs on (POSIX @bin/@ vs Windows @Scripts\\@).
-}
module Test.PlatformSpec (spec) where

import Sabela.Platform (systemPython, venvPipPath, venvPythonPath)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = describe "Sabela.Platform" $ do
#if defined(mingw32_HOST_OS)
    it "uses python on PATH and the Scripts/ venv layout on Windows" $ do
        systemPython `shouldBe` "python"
        venvPythonPath "v" `shouldBe` "v" </> "Scripts" </> "python.exe"
        venvPipPath "v" `shouldBe` "v" </> "Scripts" </> "pip.exe"
#else
    it "uses python3 on PATH and the bin/ venv layout on POSIX" $ do
        systemPython `shouldBe` "python3"
        venvPythonPath "v" `shouldBe` "v" </> "bin" </> "python3"
        venvPipPath "v" `shouldBe` "v" </> "bin" </> "pip"
#endif
