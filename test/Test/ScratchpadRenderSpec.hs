{-# LANGUAGE OverloadedStrings #-}

module Test.ScratchpadRenderSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import qualified ScriptHs.Parser as Scripths
import qualified ScriptHs.Render as Scripths

{- | Mirror of Orchestrator/Capabilities.renderHaskellForGhci. Kept here so the
test has no dependency on internals.
-}
renderHaskellForGhci :: Text -> Text
renderHaskellForGhci src =
    let sf = Scripths.scriptLines (Scripths.parseScript src)
     in Scripths.toGhciScript sf

-- | Mirror of Capabilities.augmentGhciError for unit-level assertions.
augmentGhciError :: Text -> Text
augmentGhciError err
    | T.null err = err
    | mentionsLet err =
        err
            <> "\n[sabela hint] GHCi rejected a top-level `let` binding."
    | otherwise = err
  where
    mentionsLet t =
        ("parse error" `T.isInfixOf` t)
            && ( "let" `T.isInfixOf` t
                    || "=" `T.isInfixOf` t && "on input" `T.isInfixOf` t
               )

spec :: Spec
spec = do
    describe "renderHaskellForGhci (scratchpad parity with cells)" $ do
        it "strips a top-level `let` so scratchpad matches cell semantics" $ do
            let rendered = renderHaskellForGhci "let x = 1"
            T.isInfixOf "let x" rendered `shouldBe` False
            T.isInfixOf "x" rendered `shouldBe` True

        it "preserves `let ... in ...` expressions" $ do
            let src = "let x = 1 in x + 1"
                rendered = renderHaskellForGhci src
            T.isInfixOf "let x = 1 in x + 1" rendered `shouldBe` True

        it "passes imports through unchanged" $ do
            let rendered = renderHaskellForGhci "import Data.Text (Text)"
            T.isInfixOf "import Data.Text" rendered `shouldBe` True

        it "accepts a multi-line function definition without manual :{ :}" $ do
            let src =
                    T.unlines
                        [ "double :: Int -> Int"
                        , "double x = x * 2"
                        ]
                rendered = renderHaskellForGhci src
            rendered `shouldSatisfy` (not . T.null)
            -- scripths wraps multi-line defs in :{ :}; either form acceptable.
            (T.isInfixOf ":{" rendered || T.isInfixOf "double x" rendered)
                `shouldBe` True

        it "rewrites a top-level TH splice so it evaluates at the REPL" $ do
            let rendered = renderHaskellForGhci "$(someSplice df)"
            -- rewriteSplice turns $(...) into `_ = (); ...`
            T.isInfixOf "someSplice df" rendered `shouldBe` True

    describe "augmentGhciError" $ do
        it "passes empty stderr through unchanged" $
            augmentGhciError "" `shouldBe` ""

        it "adds a scripths hint on top-level `let` parse errors" $ do
            let err = "<interactive>:3:1: error: parse error on input `let'"
                out = augmentGhciError err
            T.isInfixOf "scripths" (T.toLower out)
                || T.isInfixOf "sabela hint" (T.toLower out)
                `shouldBe` True
            out `shouldSatisfy` T.isInfixOf err

        it "does not annotate unrelated errors" $ do
            let err = "Variable not in scope: undefinedName"
            augmentGhciError err `shouldBe` err
