{-# LANGUAGE OverloadedStrings #-}

module Test.ScratchpadRenderSpec (spec) where

import Data.Aeson (Value (..))
import Data.Functor.Identity (runIdentity)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Scratchpad (scratchpadPayload)
import Sabela.AI.Capabilities.Util (parseCellLang)
import Sabela.SessionTypes (CellLang (..))
import qualified ScriptHs.Parser as Scripths
import qualified ScriptHs.Render as Scripths

defaultLang :: Text -> Maybe CellLang
defaultLang rawLang =
    if T.null rawLang then Just Haskell else parseCellLang rawLang

renderHaskellForGhci :: Text -> Text
renderHaskellForGhci src =
    let sf = Scripths.scriptLines (Scripths.parseScript src)
     in Scripths.toGhciScript sf

-- | The whole payload as one text, so a claim in any field is visible.
payloadText :: Text -> Text -> Text
payloadText code stderr =
    T.pack (show (runIdentity (scratchpadPayload (pure . String) code "" stderr)))

-- | GHC naming @let@ as the offending token.
letParseError :: Text
letParseError = "<interactive>:3:1: error: parse error on input `let'"

{- | GHC's own suggested fix for a parse error on @=@. Its wording names @let@
whatever the snippet wrote; it is what the removed hint fired on.
-}
equalsParseError :: Text
equalsParseError =
    "<interactive>:2:5: error: [GHC-58481]\n\
    \    parse error on input \8216=\8217\n\
    \    Perhaps you need a 'let' in a 'do' block?"

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
            (T.isInfixOf ":{" rendered || T.isInfixOf "double x" rendered)
                `shouldBe` True

        it "rewrites a top-level TH splice so it evaluates at the REPL" $ do
            let rendered = renderHaskellForGhci "$(someSplice df)"
            T.isInfixOf "someSplice df" rendered `shouldBe` True

    describe "what a scratchpad payload says about a parse error" $ do
        it "reports a top-level `let` when the snippet writes one" $
            payloadText "let x = 1\n" letParseError
                `shouldSatisfy` T.isInfixOf "top-level"

        it "says nothing about `let` when the snippet writes none" $
            payloadText "main = do\n  x = 5\n  print x\n" equalsParseError
                `shouldSatisfy` (not . T.isInfixOf "top-level")

        it "says nothing about `let` for an unrelated error" $
            payloadText "answer = undefinedName\n" "Variable not in scope: undefinedName"
                `shouldSatisfy` (not . T.isInfixOf "top-level")

    describe "language field default (scratchpad / insert chokepoint)" $ do
        it "defaults an empty language field to Haskell" $
            defaultLang "" `shouldBe` Just Haskell

        it "parses an explicit Haskell language" $
            defaultLang "Haskell" `shouldBe` Just Haskell

        it "parses an explicit Python language" $
            defaultLang "Python" `shouldBe` Just Python

        it "rejects a non-empty unrecognised language" $
            defaultLang "Ruby" `shouldBe` Nothing
