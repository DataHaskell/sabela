{-# LANGUAGE OverloadedStrings #-}

module Test.LeanExportSpec (spec) where

import Data.Text (Text)
import Sabela.Handlers.Lean (
    classifyReplMessages,
    parseLeanExports,
    replMsgToError,
 )
import Sabela.LeanRepl (
    ReplMessage (..),
    ReplPos (..),
 )
import Sabela.Model (CellError (..))
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Helper to create a REPL message at a given line (1-based).
mkMsg :: Int -> Text -> Text -> ReplMessage
mkMsg line sev msg =
    ReplMessage
        { rmSeverity = sev
        , rmPos = ReplPos{rpLine = line, rpColumn = 0}
        , rmEndPos = Nothing
        , rmData = msg
        }

spec :: Spec
spec = describe "Lean REPL integration" $ do
    describe "parseLeanExports" $ do
        it "matches export annotation to #eval output by line number" $ do
            let src = "-- export: my_val\n#eval toString 42"
                msgs = [mkMsg 2 "information" "42"]
            parseLeanExports src msgs
                `shouldBe` [("my_val", "42")]

        it "returns empty when no info messages exist" $ do
            let src = "-- export: my_val\n#eval toString 42"
                msgs = [] :: [ReplMessage]
            parseLeanExports src msgs
                `shouldBe` []

        it "handles multiple exports with correct line matching" $ do
            let src = "-- export: a\n#eval 1\n-- export: b\n#eval 2"
                msgs =
                    [ mkMsg 2 "information" "1"
                    , mkMsg 4 "information" "2"
                    ]
            parseLeanExports src msgs
                `shouldBe` [("a", "1"), ("b", "2")]

        it "skips exports with no matching message" $ do
            let src = "-- export: missing\n#eval undefined\n-- export: present\n#eval 42"
                msgs = [mkMsg 4 "information" "42"]
            parseLeanExports src msgs
                `shouldBe` [("present", "42")]

    describe "classifyReplMessages" $ do
        it "separates info from error messages" $ do
            let msgs =
                    [ mkMsg 1 "information" "42"
                    , mkMsg 2 "error" "type mismatch"
                    ]
                (outputs, errText, cellErrors) = classifyReplMessages msgs "src"
            length outputs `shouldBe` 1
            errText `shouldBe` Just "type mismatch\n"
            length cellErrors `shouldBe` 1

        it "returns success HTML for declarations with no messages" $ do
            let (outputs, errText, cellErrors) = classifyReplMessages [] "def foo := 1"
            length outputs `shouldBe` 1
            errText `shouldBe` Nothing
            cellErrors `shouldBe` []

    describe "replMsgToError" $ do
        it "converts REPL message to cell error with position" $ do
            let msg = mkMsg 3 "error" "oops"
                err = replMsgToError msg
            ceLine err `shouldBe` Just 3
            ceCol err `shouldBe` Just 0
            ceMessage err `shouldBe` "oops"
