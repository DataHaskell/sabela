{-# LANGUAGE OverloadedStrings #-}

{- | R9-T5 x R8.4: a schema-recovered dispatch must be stamped as the call
its result answers. Red fixtures are the pre-fix shapes (the topMonth
@insert_cell?We@ turn and both live_test.md garbles, where the garbled
call went unanswered and the result had no recorded call); green fixtures
are the same turns with the recovered call stamped.
-}
module Test.RecoveredCallLintSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Test.Hspec

import Eval.TranscriptLint (LintIssue (..), lintMessages)

user :: Text -> Value
user c = object ["role" .= ("user" :: Text), "content" .= c]

asst :: Text -> [Text] -> Value
asst c calls =
    object
        ( ["role" .= ("assistant" :: Text), "content" .= c]
            <> [ "tool_calls"
                    .= [ object
                            [ "function"
                                .= object ["name" .= n, "arguments" .= object []]
                            ]
                       | n <- calls
                       ]
               | not (null calls)
               ]
        )

toolRes :: Text -> Text -> Value
toolRes n c = object ["role" .= ("tool" :: Text), "tool_name" .= n, "content" .= c]

rules :: [Value] -> [Text]
rules = map liRule . lintMessages

prefix :: [Value]
prefix = [user "Define topMonth."]

spec :: Spec
spec = describe
    "R9-T5 recovered-call fixtures: a recovered dispatch must be \
    \stamped as the call the result answers (red pre-fix, green stamped)"
    $ do
        it "RED topMonth shape: insert_cell?We call + replace result fails" $ do
            let msgs =
                    prefix
                        <> [ asst "" ["insert_cell?We"]
                           , toolRes "replace_cell_source" "{\"ok\":true}"
                           ]
            rules msgs `shouldContain` ["unanswered-call"]
            rules msgs `shouldContain` ["phantom-result"]
        it "GREEN topMonth shape: the stamped recovered call pairs cleanly" $
            rules
                ( prefix
                    <> [ asst "" ["replace_cell_source"]
                       , toolRes "replace_cell_source" "{\"ok\":true}"
                       ]
                )
                `shouldBe` []
        it "RED live_test punctuation garble: unanswered ……..???????????? call" $ do
            let msgs =
                    prefix
                        <> [ asst "" ["\x2026\x2026..????????????"]
                           , toolRes "discover" "{\"state\":\"found\"}"
                           ]
            rules msgs `shouldContain` ["unanswered-call"]
        it "GREEN live_test punctuation garble: stamped discover call pairs" $
            rules
                ( prefix
                    <> [ asst "" ["discover"]
                       , toolRes "discover" "{\"state\":\"found\"}"
                       ]
                )
                `shouldBe` []
        it "RED live_test ellipsis garble: ?…..?? call + insert result fails" $ do
            let msgs =
                    prefix
                        <> [ asst "" ["?\x2026..??...????..??..???]"]
                           , toolRes "insert_cell" "{\"ok\":true}"
                           ]
            rules msgs `shouldContain` ["unanswered-call"]
            rules msgs `shouldContain` ["phantom-result"]
        it "GREEN live_test ellipsis garble: stamped insert_cell call pairs" $
            rules
                ( prefix
                    <> [ asst "" ["insert_cell"]
                       , toolRes "insert_cell" "{\"ok\":true}"
                       ]
                )
                `shouldBe` []
