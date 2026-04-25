module Main (main) where

import qualified Test.AiDocSpec as AiDocSpec
import qualified Test.AiHandlesSpec as AiHandlesSpec
import qualified Test.AiHistorySpec as AiHistorySpec
import qualified Test.AiRestSpec as AiRestSpec
import qualified Test.CacheControlSpec as CacheControlSpec
import qualified Test.CompactResultSpec as CompactResultSpec
import qualified Test.CycleMsgSpec as CycleMsgSpec
import qualified Test.GenerationSpec as GenerationSpec
import Test.Hspec (hspec)
import qualified Test.OutputSpec as OutputSpec
import qualified Test.PreinstalledSpec as PreinstalledSpec
import qualified Test.ScratchpadRenderSpec as ScratchpadRenderSpec
import qualified Test.SessionSpec as SessionSpec
import qualified Test.ToolParseSpec as ToolParseSpec
import qualified Test.TopoSpec as TopoSpec
import qualified Test.UsageEventSpec as UsageEventSpec
import qualified Test.UsageMergeSpec as UsageMergeSpec

main :: IO ()
main = hspec $ do
    SessionSpec.spec
    TopoSpec.spec
    OutputSpec.spec
    PreinstalledSpec.spec
    GenerationSpec.spec
    AiDocSpec.spec
    AiHandlesSpec.spec
    AiHistorySpec.spec
    AiRestSpec.spec
    CacheControlSpec.spec
    CompactResultSpec.spec
    CycleMsgSpec.spec
    ScratchpadRenderSpec.spec
    ToolParseSpec.spec
    UsageEventSpec.spec
    UsageMergeSpec.spec
