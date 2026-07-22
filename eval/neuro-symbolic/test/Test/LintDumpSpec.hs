{-# LANGUAGE OverloadedStrings #-}

{- | R6.10 x R8.4: the transcript-lint rules closing the jsonSum-off hole —
module-API dumps on a harness channel after a Succeeded write, and
version-qualified GHC FQNs in harness-channel notes. Split from
'Test.TranscriptLintSpec' (module-size cap).
-}
module Test.LintDumpSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Test.Hspec

import Eval.TranscriptLint (LintIssue (..), lintMessages)

sys :: Text -> Value
sys c = object ["role" .= ("system" :: Text), "content" .= c]

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

okWrite :: Value
okWrite = toolRes "insert_cell" "{\"cellId\":1,\"ok\":true}"

failedWrite :: Value
failedWrite =
    toolRes
        "insert_cell"
        "{\"cellId\":1,\"ok\":false,\"error\":\"Variable not in scope: bars\"}"

banner :: Text -> Value
banner m =
    toolRes
        "discover"
        ( "Discovered API of newly installed module "
            <> m
            <> " (use these real names, do not invent any):\n"
            <> "decode :: ByteString -> Maybe aeson-2.3.1.0:Data.Aeson.Types.Internal.Value\n"
            <> "encode :: aeson-2.3.1.0:Data.Aeson.Types.Internal.Value -> ByteString\n"
            <> "eitherDecode :: ByteString -> Either String aeson-2.3.1.0:Data.Aeson.Types.Internal.Value"
        )

card :: Value
card =
    toolRes
        "discover"
        "## Live API grammar (synthesised from :browse)\n\
        \### Data.Aeson\n\
        \  decode :: ByteString -> Maybe Value\n\
        \  encode :: Value -> ByteString\n\
        \  eitherDecode :: ByteString -> Either String Value"

-- | The real jsonSum-off leak shape: two banners after a Succeeded write.
jsonSumLeak :: [Value]
jsonSumLeak =
    [ sys "s"
    , user "Define jsonSum."
    , asst "" ["insert_cell"]
    , okWrite
    , banner "Data.Aeson"
    , banner "Data.ByteString.Lazy.Char8"
    , asst "done" []
    ]

spec :: Spec
spec = describe "R6.10 x R8.4: post-success module dumps in harness channels" $ do
    it "FAILS the real jsonSum-off leak shape (two post-success banners)" $ do
        let rs = rules jsonSumLeak
        rs `shouldSatisfy` elem "post-success-module-dump"
        length (filter (== "post-success-module-dump") rs) `shouldBe` 2
    it "flags the version-qualified FQNs the banners carried" $
        rules jsonSumLeak `shouldSatisfy` elem "version-qualified-name"
    it "the fixed transcript (no cards after the success) passes" $
        lintMessages
            [ sys "s"
            , user "Define jsonSum."
            , asst "" ["insert_cell"]
            , okWrite
            , asst "done" []
            ]
            `shouldBe` []
    it "a card after a FAILED write is the legal seam, not a dump" $
        rules
            [ sys "s"
            , user "u"
            , asst "" ["insert_cell"]
            , failedWrite
            , card
            , asst "retry" []
            ]
            `shouldSatisfy` notElem "post-success-module-dump"
    it "a proactive card before any write is legal" $
        rules [sys "s", user "u", card, asst "go" []]
            `shouldSatisfy` notElem "post-success-module-dump"
    it "a version-qualified FQN in a MODEL message is not the harness's leak" $
        rules
            [ sys "s"
            , user "u"
            , asst "I saw aeson-2.3.1.0:Data.Aeson.Types.Internal.Value" []
            ]
            `shouldSatisfy` notElem "version-qualified-name"
    it "a plain versioned package name is not a version-qualified FQN" $
        rules [toolRes "discover" "dataframe-2.0.0.0 exports DataFrame.col"]
            `shouldSatisfy` notElem "version-qualified-name"
