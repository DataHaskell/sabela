{-# LANGUAGE OverloadedStrings #-}

module Test.TranscriptLintSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Eval.TranscriptLint (
    LintIssue (..),
    lintLine,
    lintMessages,
    singleShapedCells,
    stopIssues,
 )

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

-- | A well-formed episode: every call answered once, harness channels exempt.
clean :: [Value]
clean =
    [ sys "Pair on a live notebook."
    , user "Define dateDays."
    , asst "" ["insert_cell"]
    , toolRes "insert_cell" "{\"cellId\":1,\"ok\":true}"
    , toolRes "discover" "grammar: bars :: [(Text, Double)] -> Plot -> Text"
    , toolRes "health_gate" "act now"
    , asst "Done: dateDays = 100." []
    ]

-- | Real defect shapes from the 2026-07-18 bench transcripts.
httpDump :: Text
httpDump =
    "transport error: HttpExceptionRequest Request { host = \"localhost\" \
    \port = 3101 secure = False requestHeaders = [...] } ResponseTimeout"

hashWall :: Text
hashWall =
    "Discovered API (use these real names, do not invent any): \
    \dataframe-2.0.0.0-8f7c29ab34d1e5f60112abcd:DataFrame.Internal.col"

hiddenSentence :: Text
hiddenSentence =
    "Package granite is hidden; add build-depends granite to expose it in \
    \this session"

launderedDecode :: Text
launderedDecode = "transport error: Trailing garbage: 404 page not found"

spec :: Spec
spec = describe "Eval.TranscriptLint (R8.4 transcript lint)" $ do
    describe "negative fixtures seeded from the 2026-07-18 defects" $ do
        it "flags a raw HttpExceptionRequest dump" $
            rules (clean <> [toolRes "insert_cell2" httpDump])
                `shouldSatisfy` elem "raw-exception"
        it "flags internal package-hash names" $
            rules [toolRes "discover" hashWall]
                `shouldSatisfy` elem "package-hash"
        it "flags the 15x-repeated hidden-package sentence" $
            rules [toolRes "discover" (T.unlines (replicate 15 hiddenSentence))]
                `shouldSatisfy` elem "repeated-sentence"
        it "flags an unanswered tool call" $
            rules [sys "s", user "u", asst "" ["insert_cell"], user "next"]
                `shouldSatisfy` elem "unanswered-call"
        it "flags a serialisation error laundered into a string (M8)" $
            rules [toolRes "discover" launderedDecode]
                `shouldSatisfy` elem "serialisation-in-string"
        it "flags the jsonSum triple-result for one call (P10)" $
            rules
                [ asst "" ["insert_cell"]
                , toolRes "insert_cell" "err"
                , toolRes "insert_cell" "banner"
                , toolRes "insert_cell" "banner"
                ]
                `shouldSatisfy` elem "phantom-result"
        it "passes the clean fixture (proving detection, not mere running)" $
            lintMessages clean `shouldBe` []
        it "passes a classified transport failure line" $
            lintMessages
                [ asst "" ["insert_cell"]
                , toolRes
                    "insert_cell"
                    "[infra] HTTP 404: endpoint missing. Your request was not \
                    \the problem."
                ]
                `shouldBe` []

    describe "general invariants (content-shape, not task patterns)" $ do
        it "a sentence may appear at most twice per payload" $
            mapM_
                ( \n ->
                    ( n
                    , "repeated-sentence"
                        `elem` rules
                            [toolRes "t" (T.unlines (replicate n hiddenSentence))]
                    )
                        `shouldBe` (n, n > 2)
                )
                [0 .. 5 :: Int]
        it
            "a MODEL call to a harness-channel-named tool is still an answer \
            \(the discover tool collision, run-20260720 false positive)"
            $ mapM_
                ( \name ->
                    rules
                        [ sys "s"
                        , asst "" [name]
                        , toolRes name "{}"
                        , asst "done" []
                        ]
                        `shouldBe` []
                )
                ["discover", "health_gate", "verify", "salvage"]
        it "an unanswered call to a harness-channel-named tool still flags" $
            rules [sys "s", asst "" ["discover"], asst "done" []]
                `shouldSatisfy` elem "unanswered-call"
        it "exactly one result per call, over all call/result counts" $
            mapM_
                ( \(k, j) -> do
                    let msgs =
                            [sys "s", asst "" (replicate k "execute_cell")]
                                <> replicate j (toolRes "execute_cell" "{}")
                                <> [asst "done" []]
                        rs = rules msgs
                    (k, j, "unanswered-call" `elem` rs) `shouldBe` (k, j, j < k)
                    (k, j, "phantom-result" `elem` rs) `shouldBe` (k, j, j > k)
                )
                [(k, j) | k <- [0 .. 3 :: Int], j <- [0 .. 3 :: Int]]
        it "no serialisation format inside a string, wherever it appears" $
            mapM_
                ( \c ->
                    rules [toolRes "t" c]
                        `shouldSatisfy` elem "serialisation-in-string"
                )
                [ "Error in $: key \"result\" not found"
                , "oops: Trailing garbage: <html>"
                ]
        it "does not flag ordinary prose, versions, or module paths" $
            lintMessages
                [ toolRes
                    "discover"
                    "dataframe-2.0.0.0 exports DataFrame.Functions.col; add \
                    \-- cabal: build-depends: dataframe"
                ]
                `shouldBe` []

    describe
        "note ledger (section 9.2, M16): injected notes assert only \
        \evidenced state"
        $ do
            let revenuePrompt =
                    "A CSV `revenue.csv` with columns `month` and `revenue` is in \
                    \the working directory. Using the dataframe library, first \
                    \load it and show the DataFrame in one cell. Then in a second \
                    \cell, plot revenue by month and show the chart."
                baselineNote =
                    "A DataFrame `df` is already loaded from the CSV and is in \
                    \scope. Write the cell that computes the requested result \
                    \from `df`."
                disclosure ok =
                    "Setup write: inserted a cell loading `revenue.csv` into \
                    \`df`. Outcome: "
                        <> if ok
                            then "{\"cellId\":1,\"ok\":true}"
                            else "TOOL ERROR: {\"error\":\"parse error\"}"
                verifiedNote =
                    "Setup: a cell loading `revenue.csv` into `df` ran \
                    \successfully, so `df` is in scope. The request above still \
                    \stands in full: write every cell it asks for."
            it "flags the baseline revenuePipeline scaffold note (no evidence)" $
                rules [sys "s", user revenuePrompt, user baselineNote]
                    `shouldSatisfy` elem "unverified-note"
            it "passes the note when a successful scaffold result evidences it" $
                lintMessages
                    [ sys "s"
                    , user revenuePrompt
                    , toolRes "scaffold" (disclosure True)
                    , user verifiedNote
                    ]
                    `shouldBe` []
            it "still flags the note when the scaffold result was a failure" $
                rules
                    [ sys "s"
                    , user revenuePrompt
                    , toolRes "scaffold" (disclosure False)
                    , user verifiedNote
                    ]
                    `shouldSatisfy` elem "unverified-note"
            it "the first user message (the task prompt) is exempt ground truth" $
                lintMessages [sys "s", user "The binding `df` is already loaded."]
                    `shouldBe` []
            it
                "an unrequested scaffold result is a harness channel, not a \
                \phantom"
                $ rules [sys "s", toolRes "scaffold" (disclosure True), asst "done" []]
                    `shouldSatisfy` notElem "phantom-result"

    describe "stopIssues (R8.4: a cap-class stop must carry a final line)" $ do
        it
            "flags max_turns with an empty final \
            \(run-20260720-085948 symbolicRegression-s1-off, 43.7k, final:)"
            $ map liRule (stopIssues "max_turns" "") `shouldBe` ["empty-final"]
        it "flags a whitespace-only final the same way" $
            map liRule (stopIssues "max_turns" "  \n ") `shouldBe` ["empty-final"]
        it "flags every cap-class stop, exempts the announced ones (grid)" $
            mapM_
                ( \(stopped, final) ->
                    ( stopped
                    , final
                    , map liRule (stopIssues stopped final)
                    )
                        `shouldBe` ( stopped
                                   , final
                                   , [ "empty-final"
                                     | T.null (T.strip final)
                                     , stopped
                                        `elem` [ "max_turns"
                                               , "repair_budget"
                                               , "deadline"
                                               ]
                                     ]
                                   )
                )
                [ (stopped, final)
                | stopped <-
                    [ "max_turns"
                    , "repair_budget"
                    , "deadline"
                    , "done"
                    , "stuck"
                    , "error"
                    ]
                , final <- ["", "  ", "Defined dateDays = 100."]
                ]
        it "the wired verdict reads FAIL empty-final" $
            lintLine (stopIssues "max_turns" "") `shouldBe` "FAIL empty-final"

    describe "lintLine (the header verdict)" $ do
        it "renders ok for a clean transcript" $
            lintLine [] `shouldBe` "ok"
        it "names each failed rule once" $
            lintLine
                [ LintIssue "raw-exception" "a"
                , LintIssue "raw-exception" "b"
                , LintIssue "package-hash" "c"
                ]
                `shouldBe` "FAIL raw-exception,package-hash"

    describe "singleShapedCells (R9.1 list_cells single shape)" $ do
        let cell i =
                object
                    [ "id" .= (i :: Int)
                    , "cellType" .= ("CodeCell" :: Text)
                    , "firstLine" .= ("x = 1" :: Text)
                    , "defines" .= (["x"] :: [Text])
                    , "hasError" .= False
                    ]
        it "accepts a uniform cell listing" $
            singleShapedCells (object ["cells" .= [cell 0, cell 1, cell 2]])
                `shouldBe` True
        it "rejects a listing whose entries have differing shapes" $
            singleShapedCells
                (object ["cells" .= [cell 0, object ["id" .= (1 :: Int)]]])
                `shouldBe` False
