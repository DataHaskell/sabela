module Main (main) where

import qualified Test.GenerationSpec as GenerationSpec
import Test.Hspec (hspec)
import qualified Test.LeanExportSpec as LeanExportSpec
import qualified Test.OutputSpec as OutputSpec
import qualified Test.PreinstalledSpec as PreinstalledSpec
import qualified Test.SessionSpec as SessionSpec
import qualified Test.TopoSpec as TopoSpec

main :: IO ()
main = hspec $ do
    SessionSpec.spec
    TopoSpec.spec
    OutputSpec.spec
    LeanExportSpec.spec
    PreinstalledSpec.spec
    GenerationSpec.spec
