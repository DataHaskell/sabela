{-# LANGUAGE OverloadedStrings #-}

{- | R9-T1: the not-in-scope extractor grid (casefold, every phrase
variant), the qualified-alias import generator, the module-rename tier and
hole-fit liveness — one dispatcher, decisions invariant under library-name
substitution (R1.6, R3.1, R5.1, R7.5-R7.7).
-}
module Test.RepairGridSpec (repairGridSpec) where

import Control.Monad (forM_)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.HoleRepair (editDistance, holeSpans)
import Sabela.AI.RepairDispatch (
    DiagClass (..),
    RepairReport (..),
    RepairTier (..),
    classifyDiag,
    notInScopeFromDiag,
    renderRepairReport,
    reportCharBudget,
    tiersFor,
 )
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Model (bareCellError)
import Siza.Agent.RepairTiers (
    Candidate (..),
    TierInput (..),
    candidatesFor,
    tierCandidates,
 )
import Test.DiscoverFixtures (runCat, stateOf)

-- | The synthetic (module, export) catalogue the locator answers from.
synCatalogue :: [(Text, Text)]
synCatalogue =
    [ ("Zephyr.Core", "gust")
    , ("Stratus.Air", "stratify")
    , ("Cumulus.Plot", "bars")
    ]

-- | Exact-bare-name locator over 'synCatalogue' (the discover path stand-in).
catLocate :: Text -> [(Text, Maybe Text)]
catLocate n = [(m, Nothing) | (m, e) <- synCatalogue, e == n]

-- | Nearest-module locator over the catalogue, edit-distance ranked.
catModules :: Text -> [Text]
catModules w =
    take 3 [m | (m, _) <- sortOn (editDistance w . fst) synCatalogue]

inputWith :: Text -> Text -> TierInput
inputWith diag src =
    TierInput
        { tiDiag = diag
        , tiSource = src
        , tiHoleFits = ""
        , tiLocate = catLocate
        , tiModules = catModules
        }

{- | The GHC phrase grid: every not-in-scope spelling, bare and
alias-qualified, built over a substitutable name.
-}
phraseGrid :: [(String, Text -> Text)]
phraseGrid =
    [ ("Variable not in scope", ("Variable not in scope: " <>))
    , ("Not in scope", \n -> "Not in scope: \8216" <> n <> "\8217")
    ,
        ( "Not in scope: type constructor or class"
        , \n ->
            "Not in scope: type constructor or class \8216" <> n <> "\8217"
        )
    ,
        ( "Data constructor not in scope"
        , ("Data constructor not in scope: " <>)
        )
    ]

qualify :: Text -> Text -> Text
qualify alias n = alias <> "." <> n

noModuleHint :: Text -> Text
noModuleHint alias =
    "\n  Suggested fix: No module named \8216" <> alias <> "\8217 is imported."

repairGridSpec :: Spec
repairGridSpec = describe "not-in-scope extractor coverage (R9-T1)" $ do
    extractorGridSpec
    qualifiedAliasSpec
    moduleRenameSpec
    holeLivenessSpec
    fixtureSpec
    reportBudgetSpec

extractorGridSpec :: Spec
extractorGridSpec = describe "phrase x {bare, qualified} x spelling grid" $ do
    it "every grid member classifies not-in-scope and yields >=1 candidate" $
        forM_ phraseGrid $ \(label, mk) ->
            forM_ synCatalogue $ \(_, name) ->
                forM_ [mk name, mk (qualify "Q" name) <> noModuleHint "Q"] $
                    \diag -> do
                        (label, name, classifyDiag diag)
                            `shouldBe` (label, name, ClassNotInScope)
                        let cands =
                                candidatesFor
                                    (tiersFor ClassNotInScope)
                                    (inputWith diag ("x = " <> name))
                        (label, name, null cands)
                            `shouldBe` (label, name, False)
    it "extraction is casefold-invariant (capitalised phrase included)" $
        forM_ phraseGrid $ \(label, mk) ->
            forM_ ["gust", "stratify"] $ \name -> do
                let diag = mk name
                    upper = T.toUpper (T.take 1 diag) <> T.drop 1 diag
                (label, notInScopeFromDiag upper)
                    `shouldBe` (label, notInScopeFromDiag diag)
                notInScopeFromDiag diag `shouldBe` Just name
    it "decisions are byte-invariant under library-name substitution" $
        forM_ phraseGrid $ \(_, mk) -> do
            let shapeFor n =
                    map cdTier $
                        candidatesFor
                            (tiersFor ClassNotInScope)
                            (inputWith (mk n) ("x = " <> n))
                shapes = map (shapeFor . snd) synCatalogue
            length shapes `shouldBe` 3
            forM_ (zip shapes (drop 1 shapes)) (uncurry shouldBe)

qualifiedAliasSpec :: Spec
qualifiedAliasSpec = describe "qualified-alias import generator" $ do
    it "resolves the BARE name and proposes the qualified import (top-3)" $
        forM_ synCatalogue $ \(m, name) -> do
            let diag =
                    "Variable not in scope: "
                        <> qualify "Q" name
                        <> noModuleHint "Q"
                cands =
                    candidatesFor
                        (tiersFor ClassNotInScope)
                        (inputWith diag ("x = Q." <> name <> " 1"))
                imports =
                    [ c
                    | c <- cands
                    , ("import qualified " <> m <> " as Q")
                        `T.isInfixOf` cdSource c
                    ]
            take 3 (concatMap cdProposes cands) `shouldContain` [m]
            null imports `shouldBe` False
    it "the lookup uses the bare name, never the qualified spelling" $ do
        -- A locator that only knows bare names: a generator querying the
        -- qualified spelling would get nothing and produce no candidate.
        let diag =
                "Variable not in scope: Q.gust :: Int -> Wind"
                    <> noModuleHint "Q"
            cands =
                candidatesFor (tiersFor ClassNotInScope) $
                    (inputWith diag "x = Q.gust 1")
                        { tiLocate = \n -> [("Zephyr.Core", Nothing) | n == "gust"]
                        }
        concatMap cdProposes cands `shouldContain` ["Zephyr.Core"]
    it "every proposed module is findable through the discover catalogue (R7.6)" $
        forM_ synCatalogue $ \(_, name) -> do
            let diag =
                    "Variable not in scope: "
                        <> qualify "Q" name
                        <> noModuleHint "Q"
                cands =
                    candidatesFor
                        (tiersFor ClassNotInScope)
                        (inputWith diag ("x = Q." <> name <> " 1"))
            forM_ (concatMap cdProposes cands) $ \p -> do
                v <- runCat p
                (p, stateOf v) `shouldBe` (p, "found")

moduleRenameSpec :: Spec
moduleRenameSpec = describe "TierModuleRename is a live generator" $ do
    it "renames a near-miss module to the nearest catalogue module" $
        forM_ [("Zephyr.Cor", "Zephyr.Core"), ("Cumulus.Plt", "Cumulus.Plot")] $
            \(wrong, right) -> do
                let diag = "Could not find module \8216" <> wrong <> "\8217"
                    src = "import " <> wrong <> "\nx = 1"
                    cands =
                        candidatesFor
                            (tiersFor (classifyDiag diag))
                            (inputWith diag src)
                classifyDiag diag `shouldBe` ClassModuleNotFound
                map cdSource cands
                    `shouldSatisfy` any (T.isInfixOf ("import " <> right))
                concatMap cdProposes cands `shouldContain` [right]
    it "every proposed rename is findable through the discover catalogue (R7.6)" $ do
        let diag = "Could not find module \8216Zephyr.Cor\8217"
            cands =
                candidatesFor
                    (tiersFor ClassModuleNotFound)
                    (inputWith diag "import Zephyr.Cor")
        cands `shouldSatisfy` (not . null)
        forM_ (concatMap cdProposes cands) $ \p -> do
            v <- runCat p
            (p, stateOf v) `shouldBe` (p, "found")

holeLivenessSpec :: Spec
holeLivenessSpec = describe "a literal Found hole engages the hole-fit tier" $ do
    let holeDiag =
            "Found hole: _ :: ColRef\n\
            \  In the expression: scatter (_ :: ColRef)"
        blob =
            "Valid hole fits include\n\
            \  colRef :: ColRef\n\
            \  mkCol :: Text -> ColRef"
    it "dispatches ClassRefinement candidates for the hole token" $ do
        classifyDiag holeDiag `shouldBe` ClassRefinement
        let cands =
                candidatesFor
                    (tiersFor ClassRefinement)
                    (inputWith holeDiag "cell = scatter _"){tiHoleFits = blob}
        cands `shouldSatisfy` (not . null)
        map cdSource cands `shouldSatisfy` any (T.isInfixOf "colRef")
    it "reads fits from the diagnostic itself when no blob was fetched" $ do
        let diag = holeDiag <> "\n" <> blob
            cands =
                candidatesFor
                    (tiersFor ClassRefinement)
                    (inputWith diag "cell = scatter _")
        cands `shouldSatisfy` (not . null)
    it "holeSpans exposes the hole goal for the live insert path" $ do
        let er =
                ExecutionResult
                    []
                    Nothing
                    [bareCellError (Just 2) (Just 16) holeDiag]
                    []
        holeSpans (Right er) `shouldBe` [("_", "ColRef", Just (2, 16))]
        holeSpans (Left holeDiag) `shouldBe` [("_", "ColRef", Nothing)]

fixtureSpec :: Spec
fixtureSpec = describe "barChart/topMonth fixtures heal through the grid" $ do
    it "T.unpack under an unimported alias proposes the qualified import" $ do
        let diag =
                "Variable not in scope: T.unpack :: Text -> String"
                    <> noModuleHint "T"
            cands =
                candidatesFor (tiersFor ClassNotInScope) $
                    (inputWith diag "s = T.unpack name")
                        { tiLocate = \n -> [("Data.Text", Nothing) | n == "unpack"]
                        }
        map cdSource cands
            `shouldSatisfy` any (T.isInfixOf "import qualified Data.Text as T")
    it "the capitalised Text tycon denial reaches the add-import tier" $ do
        let diag = "Not in scope: type constructor or class \8216Text\8217"
            cands =
                candidatesFor (tiersFor (classifyDiag diag)) $
                    (inputWith diag "f :: Text -> Int\nf = length")
                        { tiLocate = \n -> [("Data.Text", Nothing) | n == "Text"]
                        }
        classifyDiag diag `shouldBe` ClassNotInScope
        map cdSource cands
            `shouldSatisfy` any (T.isInfixOf "import Data.Text (Text)")
    it "bare maximumBy heals via add-import (no goal type needed)" $ do
        let diag = "Variable not in scope: maximumBy"
            cands =
                candidatesFor (tiersFor ClassNotInScope) $
                    (inputWith diag "m = maximumBy cmp xs")
                        { tiLocate = \n -> [("Data.List", Nothing) | n == "maximumBy"]
                        }
        map cdSource cands
            `shouldSatisfy` any (T.isInfixOf "import Data.List (maximumBy)")
    it "the Data.Frame module guess renames through the locator" $ do
        let diag = "Could not find module \8216Data.Frame\8217"
            cands =
                candidatesFor (tiersFor ClassModuleNotFound) $
                    (inputWith diag "import Data.Frame")
                        { tiModules = \w -> ["DataFrame" | w == "Data.Frame"]
                        }
        map cdSource cands `shouldSatisfy` any (T.isInfixOf "import DataFrame")

reportBudgetSpec :: Spec
reportBudgetSpec = describe "the report stays bounded for the new tiers" $
    it "module-rename and add-import reports fit the 400-char budget" $
        forM_ [TierModuleRename, TierAddImport, TierHoleFit] $ \tier ->
            forM_ [1, 25, 400 :: Int] $ \k -> do
                let rep =
                        RepairReport
                            ClassModuleNotFound
                            k
                            k
                            "budget exhausted"
                            (Just (tier, T.replicate 900 "m"))
                            []
                T.length (renderRepairReport rep)
                    `shouldSatisfy` (<= reportCharBudget)
