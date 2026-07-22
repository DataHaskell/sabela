{-# LANGUAGE OverloadedStrings #-}

{- | Section 9.2 injected-note truthfulness (M16): the note ledger — a note may
assert a binding live only when the asserting event verifiably happened — and
deliverable consistency — no note shrinks the prompt's stated ask. General
invariants over generated (note, state) pairs, never task- or library-keyed.
-}
module Test.NoteLedgerSpec (noteLedgerSpec) where

import Data.List (subsequences)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.NoteLedger (
    askConsistent,
    assertedLive,
    declaredCellCount,
    noteLedgerOk,
 )
import Siza.Agent.Scaffold (scaffoldNoteFor)

-- | The baseline revenuePipeline scaffold note (the M16 defect, verbatim).
baselineNote :: Text
baselineNote =
    "A DataFrame `df` is already loaded from the CSV and is in scope. Write the \
    \cell that computes the requested result from `df` (read a column with \
    \`D.columnAsList (D.col @Type \"name\") df`)."

-- | The bench revenuePipeline prompt: two declared cells, no typed deliverable.
revenuePipelinePrompt :: Text
revenuePipelinePrompt =
    "A CSV `revenue.csv` with columns `month` and `revenue` is in the working \
    \directory. Using the dataframe library, first load it and show the \
    \DataFrame in one cell. Then in a second cell, plot revenue by month as a \
    \bar chart with the granite library and show the chart."

namePool :: [Text]
namePool = ["df", "model", "totals"]

-- | A state-asserting sentence for one binding name.
asserting :: Text -> Text
asserting n = "A value `" <> n <> "` is already loaded and is in scope."

-- | A prompt declaring @k@ typed deliverables over a named data file.
promptWith :: Int -> Text
promptWith k =
    "Load data.csv and define "
        <> T.intercalate ", " ["`d" <> tshow i <> " :: Int`" | i <- [1 .. k]]
        <> "."
  where
    tshow = T.pack . show

noteLedgerSpec :: Spec
noteLedgerSpec = describe "Siza.Agent.NoteLedger (section 9.2 note ledger)" $ do
    describe "note-ledger property: assert only live bindings" $ do
        it
            "over generated (note, state) pairs: a state-asserting note passes \
            \iff its asserted binding is live"
            $ mapM_
                ( \(n, live) ->
                    (n, live, noteLedgerOk (Set.fromList live) (asserting n))
                        `shouldBe` (n, live, n `elem` live)
                )
                [(n, live) | n <- namePool, live <- subsequences namePool]
        it "a note with no state assertion passes against any state" $
            mapM_
                ( \live ->
                    noteLedgerOk
                        (Set.fromList live)
                        "Write every cell the request asks for."
                        `shouldBe` True
                )
                (subsequences namePool)
        it "non-identifier backtick segments are never state claims" $
            assertedLive "The file `data.csv` is already loaded." `shouldBe` []
        it "the baseline scaffold note asserts `df`" $
            assertedLive baselineNote `shouldBe` ["df"]
        it "the baseline note fails the ledger when nothing is live (M16)" $
            noteLedgerOk Set.empty baselineNote `shouldBe` False
        it "the baseline note passes only in a world where `df` is live" $
            noteLedgerOk (Set.singleton "df") baselineNote `shouldBe` True

    describe "deliverable consistency: no note shrinks the stated ask" $ do
        it
            "the baseline note's singular ask fails iff the prompt declares \
            \more than one deliverable cell"
            $ mapM_
                ( \k ->
                    (k, askConsistent Set.empty (promptWith k) baselineNote)
                        `shouldBe` (k, k <= 1)
                )
                [0 .. 3 :: Int]
        it "the baseline note contradicts the two-cell revenuePipeline prompt" $
            askConsistent Set.empty revenuePipelinePrompt baselineNote
                `shouldBe` False
        it "a note claiming a requested deliverable live needs it verified" $ do
            let note = "The binding `d1` is already loaded."
            askConsistent Set.empty (promptWith 1) note `shouldBe` False
            askConsistent (Set.singleton "d1") (promptWith 1) note
                `shouldBe` True

    describe "declaredCellCount (typed deliverables or ordinal cells)" $ do
        it "counts the revenuePipeline prompt's two ordinal cells" $
            declaredCellCount revenuePipelinePrompt `shouldBe` 2
        it "counts typed deliverables" $
            mapM_
                (\k -> declaredCellCount (promptWith k) `shouldBe` max 1 k)
                [0 .. 3 :: Int]

    describe "scaffoldNoteFor is a ledger citizen by construction" $ do
        it
            "over generated prompts: passes the ledger in the verified world \
            \and fails in the unverified one"
            $ mapM_
                ( \prompt -> do
                    let note = scaffoldNoteFor prompt "data.csv"
                    noteLedgerOk (Set.singleton "df") note `shouldBe` True
                    noteLedgerOk Set.empty note `shouldBe` False
                )
                (revenuePipelinePrompt : map promptWith [0 .. 3])
        it "over generated prompts: never shrinks the ask" $
            mapM_
                ( \prompt ->
                    askConsistent
                        (Set.singleton "df")
                        prompt
                        (scaffoldNoteFor prompt "data.csv")
                        `shouldBe` True
                )
                (revenuePipelinePrompt : map promptWith [0 .. 3])
        it "names every typed deliverable still to write" $
            mapM_
                ( \k ->
                    mapM_
                        ( \i ->
                            scaffoldNoteFor (promptWith k) "data.csv"
                                `shouldSatisfy` T.isInfixOf
                                    ("`d" <> T.pack (show i) <> "`")
                        )
                        [1 .. k]
                )
                [0 .. 3 :: Int]
