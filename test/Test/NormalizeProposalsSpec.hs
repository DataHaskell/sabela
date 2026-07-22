{-# LANGUAGE OverloadedStrings #-}

{- | R6-T1: write-boundary normalizer generators through the ONE acceptance
law, specified over a GENERATED grid (confusable hyphens x cabal-key
misspellings x keyword bindings x library substitution; no bench library).
-}
module Test.NormalizeProposalsSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.NormalizeGate (
    gatedNormalizeInsert,
    parseHealth,
 )
import Sabela.AI.NormalizeProposals (
    bindingKeywords,
    confusableHyphens,
    foldCabalComments,
    proposedRename,
    renameKeywordBindings,
 )
import Sabela.AI.RepairDispatch (acceptRepair)
import Sabela.Model (CellType (..))
import Sabela.Parse.Normalize (normalizeInsert)

-- | The confusable-hyphen grid of the task spec: U+2010..U+2015 and U+2212.
hyphenGrid :: [Char]
hyphenGrid = ['\x2010' .. '\x2015'] ++ ['\x2212']

-- | The build-depends key-misspelling class (never a package name).
keyGrid :: [Text]
keyGrid =
    [ "build-depends"
    , "build-depend"
    , "build-dependency"
    , "build-dependencies"
    , "build-deps"
    ]

{- | Synthetic library names — substitution across these must not change any
decision (bitter-lesson invariance; no bench library anywhere).
-}
libGrid :: [Text]
libGrid = ["zephyr", "stratus", "cumulus"]

-- | A corrupted cabal line: the key's ASCII hyphens replaced by @h@.
corruptLine :: Char -> Text -> Text -> Text
corruptLine h key lib =
    "-- cabal: " <> T.map swap key <> ": " <> lib
  where
    swap '-' = h
    swap c = c

-- | A barChart-shaped cell body under any library name.
cellWith :: Text -> Text -> Text
cellWith cabal lib =
    cabal <> "\nimport " <> T.toTitle lib <> ".Core\nx = 1"

canonical :: Text -> Text
canonical lib = "-- cabal: build-depends: " <> lib

spec :: Spec
spec = describe "write-boundary normalizer proposals (R6-T1)" $ do
    describe "cabal-comment fold: hyphens x keys x libraries" $ do
        it "folds every (hyphen, key) corruption to the canonical key" $
            forM_ hyphenGrid $ \h -> forM_ keyGrid $ \key -> forM_ libGrid $
                \lib -> do
                    let src = cellWith (corruptLine h key lib) lib
                        (_, src', _) = gatedNormalizeInsert CodeCell src
                    T.lines src' `shouldSatisfy` \ls ->
                        take 1 ls == [canonical lib]
        it "folds an ASCII-hyphen misspelled key too" $
            forM_ keyGrid $ \key -> forM_ libGrid $ \lib -> do
                let src = cellWith ("-- cabal: " <> key <> ": " <> lib) lib
                    (_, src', _) = gatedNormalizeInsert CodeCell src
                T.lines src' `shouldSatisfy` \ls ->
                    take 1 ls == [canonical lib]
        it "discloses a kept fold with a note carrying the post-rewrite source" $
            forM_ libGrid $ \lib -> do
                let src = cellWith (corruptLine '\x2011' "build-dependency" lib) lib
                    (_, src', notes) = gatedNormalizeInsert CodeCell src
                src' `shouldNotBe` src
                notes `shouldSatisfy` any (T.isInfixOf (canonical lib))
                notes `shouldSatisfy` any (T.isInfixOf src')
        it "never touches a non-cabal comment or code hyphens" $ do
            let src = "-- a plain \x2013 comment\ny = 3 - 1"
                (_, src', notes) = gatedNormalizeInsert CodeCell src
            src' `shouldBe` src
            notes `shouldBe` []
        it "preserves the package tokens verbatim (fold never renames a package)" $
            forM_ libGrid $ \lib -> do
                let line = corruptLine '\x2212' "build-deps" (lib <> ", extra-pkg")
                fst (foldCabalComments line)
                    `shouldBe` ("-- cabal: build-depends: " <> lib <> ", extra-pkg")
        it "folds confusable hyphens inside the value field of a cabal line" $ do
            let line = "-- cabal: build-depends: some\x2011pkg"
            fst (foldCabalComments line)
                `shouldBe` "-- cabal: build-depends: some-pkg"
        it "exposes exactly the task's confusable set" $
            confusableHyphens `shouldBe` (['\x2010' .. '\x2015'] ++ ['\x2212'])
        it "a trailing newline alone triggers no rewrite and no note (R7.2)" $
            forM_ libGrid $ \lib -> do
                let src = cellWith (canonical lib) lib <> "\n"
                    (_, src', notes) = gatedNormalizeInsert CodeCell src
                src' `shouldBe` src
                notes `shouldBe` []

    describe "keyword-binding rename proposals" $ do
        it "renames a keyword binding and its uses so the cell parses" $
            forM_ bindingKeywords $ \kw -> do
                let src = kw <> " = 5\nresult = " <> kw <> " + 1"
                    (_, src', _) = gatedNormalizeInsert CodeCell src
                src'
                    `shouldBe` ( proposedRename kw
                                    <> " = 5\nresult = "
                                    <> proposedRename kw
                                    <> " + 1"
                               )
        it "renames through the let path (the smoke's `let data = 5`)" $ do
            let (_, src', _) = gatedNormalizeInsert CodeCell "let data = 5"
            src' `shouldBe` "data' = 5"
        it "leaves a legitimate keyword declaration alone" $ do
            let src = "data Gale = Gale Int\ntype Wind = Int"
                (_, src', notes) = gatedNormalizeInsert CodeCell src
            src' `shouldBe` src
            notes `shouldBe` []
        it "never rewrites inside a string literal" $ do
            let src = "type = 3\nmsg = \"type = 3\""
                (_, src', _) = gatedNormalizeInsert CodeCell src
            src' `shouldBe` "type' = 3\nmsg = \"type = 3\""
        it "a proposal that still fails to parse reverts byte-identically" $ do
            let src = "data = (\n"
                (_, src', notes) = gatedNormalizeInsert CodeCell src
            src' `shouldBe` src
            notes `shouldSatisfy` any (T.isInfixOf "reverted")
        it "every proposed rename is a valid non-keyword identifier" $
            forM_ bindingKeywords $ \kw -> do
                proposedRename kw `shouldNotBe` kw
                proposedRename kw `shouldSatisfy` (`notElem` bindingKeywords)
                renameKeywordBindings (proposedRename kw <> " = 1")
                    `shouldBe` (proposedRename kw <> " = 1", [])

    describe "library-name substitution invariance" $ do
        it "decisions are byte-identical under library renaming" $
            forM_ hyphenGrid $ \h -> forM_ keyGrid $ \key -> do
                let outcomeFor lib =
                        let src = cellWith (corruptLine h key lib) lib
                            (_, src', notes) = gatedNormalizeInsert CodeCell src
                            scrub = T.replace lib "LIB" . T.replace (T.toTitle lib) "Lib"
                         in (scrub src', map scrub notes)
                    outs = map outcomeFor libGrid
                length (Set.toList (Set.fromList outs)) `shouldBe` 1

    describe "the verdict is literally acceptRepair (no second rule)" $
        it "holds over the whole generated grid" $ do
            let grid =
                    [ cellWith (corruptLine h key lib) lib
                    | h <- hyphenGrid
                    , key <- keyGrid
                    , lib <- libGrid
                    ]
                        ++ [kw <> " = 5" | kw <- bindingKeywords]
                        ++ ["data = (\n", "let type = 3", "x = 1"]
            forM_ grid $ \src -> do
                let (_, kept, _) = gatedNormalizeInsert CodeCell src
                    cand = normalizeCandidate src
                    lawVerdict =
                        acceptRepair
                            Set.empty
                            [("candidate", parseHealth src)]
                            [("candidate", parseHealth cand)]
                            "candidate"
                if cand == src
                    then kept `shouldBe` src
                    else kept `shouldBe` (if lawVerdict then cand else src)

-- | The ungated generator composition (what the gate is asked to vet).
normalizeCandidate :: Text -> Text
normalizeCandidate src = cand
  where
    (_, cand, _) = normalizeInsert CodeCell src
