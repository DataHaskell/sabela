{-# LANGUAGE OverloadedStrings #-}

module Test.ApplicabilitySpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (
    createDirectoryIfMissing,
    getTemporaryDirectory,
    removePathForcibly,
 )
import System.FilePath ((</>))
import Test.Hspec

import Eval.Applicability (
    PairCategory (..),
    classifyPair,
    discoverClassCalls,
    excludeFlagged,
    naNote,
    readNaFlags,
    readSaturatedFlags,
    readVoidFlags,
    saturatedNote,
    surfaceAnswered,
    voidNote,
 )

header :: Text -> Text
header arm = T.unlines ["<!-- episode-config", "arm: " <> arm, "-->"]

-- | A search-free body: a plain insert-cell trajectory, no discover markers.
searchFreeBody :: Text
searchFreeBody =
    T.unlines
        [ "# Session: t"
        , ""
        , "## 1. assistant"
        , ""
        , "**tool calls:**"
        , "- `insert_cell` {\"source\":\"x = 1\"}"
        , ""
        ]

-- | A discover-class CALL line with no answering result (lever never fired).
callOnlyLine :: Text
callOnlyLine = "- `find_function` {\"query\":\"col\"}\n"

-- | One discover-class result header: the surface ANSWERED (lever fired).
discoverHeader :: Text
discoverHeader = T.unlines ["## 2. tool (discover)", "", "```", "surface", "```", ""]

{- | A body with @c@ unanswered discover-class call lines and @r@ answering
result headers; (0, 0) is search-free.
-}
bodyWith :: Int -> Int -> Text
bodyWith c r =
    searchFreeBody
        <> mconcat (replicate c callOnlyLine)
        <> mconcat (replicate r discoverHeader)

file :: Text -> Text -> Text
file arm body = header arm <> body

{- | The three-way law over a generated (lever-fired x body-equal) pair:
search-free on both arms is the category NA; differing bodies measured; equal
bodies split on the lever-fired evidence — answered surface = saturated,
unanswered = dead (VOID). Never conflated.
-}
expected :: (Int, Int) -> (Int, Int) -> PairCategory
expected off@(_, r1) on
    | off == (0, 0) && on == (0, 0) = PairNotApplicable
    | off /= on = PairSound
    | r1 > 0 = PairSaturated
    | otherwise = PairVoid

spec :: Spec
spec = describe "Eval.Applicability (lever-axis applicability, R8.2/section 13)" $ do
    describe "discoverClassCalls (structural markers only)" $ do
        it "counts nothing in a search-free body" $
            discoverClassCalls (bodyWith 0 0) `shouldBe` 0
        it "counts a harness discover result header" $
            discoverClassCalls (bodyWith 0 1) `shouldBe` 1
        it "counts a model discover-class call line" $
            discoverClassCalls "- `find_function` {\"query\":\"col\"}" `shouldBe` 1
        it "ignores the prompt cheat-sheet's `discover <query>` prose" $
            discoverClassCalls "  discover <query>   ANY function/package" `shouldBe` 0
        it "ignores prose mentioning the (@discover@) tool" $
            discoverClassCalls "use the search tool (@discover@) to look" `shouldBe` 0

    describe "surfaceAnswered (the lever-fired marker)" $ do
        it "is False on a search-free body" $
            surfaceAnswered (bodyWith 0 0) `shouldBe` False
        it "is False when discover was only CALLED, never answered" $
            surfaceAnswered (bodyWith 2 0) `shouldBe` False
        it "is True when a discover-class result header answered" $
            surfaceAnswered (bodyWith 0 1) `shouldBe` True
        it "is True for a call-and-answer trajectory" $
            surfaceAnswered (bodyWith 1 1) `shouldBe` True

    describe "classifyPair" $ do
        it "search-free on both arms is not-applicable (even when identical)" $
            classifyPair (file "off" (bodyWith 0 0)) (file "on" (bodyWith 0 0))
                `shouldBe` PairNotApplicable
        it "search-free on both arms but differing bodies is still not-applicable" $
            classifyPair
                (file "off" searchFreeBody)
                (file "on" (searchFreeBody <> "## 2. assistant\n"))
                `shouldBe` PairNotApplicable
        it "byte-identical arms whose discover surface ANSWERED are saturated" $
            classifyPair (file "off" (bodyWith 1 1)) (file "on" (bodyWith 1 1))
                `shouldBe` PairSaturated
        it "byte-identical arms with only unanswered discover calls are VOID" $
            classifyPair (file "off" (bodyWith 1 0)) (file "on" (bodyWith 1 0))
                `shouldBe` PairVoid
        it "search-using arms that differ are a sound measurement" $
            classifyPair (file "off" (bodyWith 1 1)) (file "on" (bodyWith 1 2))
                `shouldBe` PairSound

    describe "classification grid invariant (lever-fired x body-equal)" $
        it "saturated is never a measurement and never mislabelled VOID" $
            sequence_
                [ classifyPair (file "off" (bodyWith c1 r1)) (file "on" (bodyWith c2 r2))
                    `shouldBe` expected (c1, r1) (c2, r2)
                | c1 <- [0 .. 2]
                , r1 <- [0 .. 2]
                , c2 <- [0 .. 2]
                , r2 <- [0 .. 2]
                ]

    describe "excludeFlagged (no flagged pair reaches a lever number)" $ do
        it "drops VOID, NA and saturated (task, seed) pairs, stripping the seed" $
            excludeFlagged
                [("na", 1), ("void", 2), ("sat", 4)]
                [ ("na", 1, "off" :: Text, True)
                , ("na", 1, "on", True)
                , ("void", 2, "off", False)
                , ("void", 2, "on", True)
                , ("sat", 4, "off", True)
                , ("sat", 4, "on", True)
                , ("sound", 3, "off", True)
                , ("sound", 3, "on", False)
                ]
                `shouldBe` [("sound", "off", True), ("sound", "on", False)]

    describe "flag files over a directory" $
        it "parses each flag's (task, seed) by its suffix, saturated included" $ do
            tmp <- getTemporaryDirectory
            let dir = tmp </> "siza-eval-applicability-flags"
            removePathForcibly dir
            createDirectoryIfMissing True dir
            TIO.writeFile (dir </> "barChart-s1.VOID") "v"
            TIO.writeFile (dir </> "symbolicRegression-s1.NA") "n"
            TIO.writeFile (dir </> "barChart-s2.SATURATED") "s"
            voids <- readVoidFlags dir
            nas <- readNaFlags dir
            sats <- readSaturatedFlags dir
            voids `shouldBe` [("barChart", 1)]
            nas `shouldBe` [("symbolicRegression", 1)]
            sats `shouldBe` [("barChart", 2)]
            voidNote voids `shouldSatisfy` T.isInfixOf "barChart s1"
            naNote nas `shouldSatisfy` T.isInfixOf "symbolicRegression s1"
            saturatedNote sats `shouldSatisfy` T.isInfixOf "barChart s2"
            saturatedNote sats `shouldSatisfy` T.isInfixOf "lever fired"
