{-# LANGUAGE OverloadedStrings #-}

module Test.GateCandidatesSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.GateRepair.Candidates (
    costOrdered,
    exactMatchOnly,
    missingModuleCandidates,
    repairCandidates,
    roundRobin,
 )
import Sabela.AI.PackageIndex (PackageEntry (..))
import System.Directory (findExecutable)

fakeEntry :: PackageEntry
fakeEntry = PackageEntry "somepkg" "1.0" "" ["Some.Module"]

spec :: Spec
spec = orderingSpec >> costSpec >> missingModuleSpec

orderingSpec :: Spec
orderingSpec = describe "no class of repair starves another out of the probe budget" $ do
    it "takes one candidate from each class before a second from any" $
        take 4 (roundRobin [["w1", "w2", "w3"], ["m1"], ["a1", "a2"], ["r1" :: Text]])
            `shouldBe` ["w1", "m1", "a1", "r1"]

    it "keeps every candidate, only reorders them" $ do
        let classes = [["w1", "w2", "w3"], ["m1"], ["a1", "a2" :: Text]]
        length (roundRobin classes) `shouldBe` length (concat classes)

    it "drains a class that runs out without dropping the rest" $
        roundRobin [["w1", "w2"], ["m1" :: Text]] `shouldBe` ["w1", "m1", "w2"]

    it "an empty class costs nothing" $
        roundRobin [[], ["m1" :: Text], []] `shouldBe` ["m1"]

plain :: Text -> (Text, [Text])
plain name = ("x = " <> name, [name])

declaresDep :: Text -> (Text, [Text])
declaresDep pkg =
    ( "-- cabal: build-depends: " <> pkg <> "\nx = 1"
    , ["declared build-depends: " <> pkg]
    )

{- | A dependency probe measured 52.8s against 6.7s for every other candidate,
so declaring one must never be tried before the cheap candidates.
-}
costSpec :: Spec
costSpec = describe "a dependency candidate never spends the budget the cheap ones need" $ do
    let src = "x = 1"

    it "sinks a dependency candidate below every candidate that needs no build" $
        map fst (costOrdered src [declaresDep "split", plain "a", plain "b"])
            `shouldBe` map fst [plain "a", plain "b", declaresDep "split"]

    it "keeps the round-robin order within a cost tier" $
        map
            snd
            (costOrdered src [plain "w1", declaresDep "split", plain "m1", plain "a1"])
            `shouldBe` [["w1"], ["m1"], ["a1"], ["declared build-depends: split"]]

    it "keeps every candidate, only reorders them" $ do
        let cands = [declaresDep "split", plain "a", declaresDep "text", plain "b"]
        length (costOrdered src cands) `shouldBe` length cands

    it "counts only dependencies the source did not already declare" $ do
        let declared = "-- cabal: build-depends: split\nx = 1"
            widened = ("-- cabal: build-depends: split\nx = 2", ["renamed"])
        map fst (costOrdered declared [widened, plain "a"])
            `shouldBe` map fst [widened, plain "a"]

    it "is a no-op when nothing declares a dependency" $
        costOrdered src [plain "a", plain "b"] `shouldBe` [plain "a", plain "b"]

missingModuleSpec :: Spec
missingModuleSpec = describe "a missing module is repaired only when verified, not guessed" $ do
    describe "hidden packages are declared pinned to the version GHC named" $
        it "the fix candidate carries text ==2.0.2, never a solver-free name" $ do
            let diag =
                    "Could not load module \8216Data.Text\8217\n\
                    \It is a member of the hidden package \8216text-2.0.2\8217."
                fixes = concatMap snd (repairCandidates diag "import Data.Text\nx = T.length")
            fixes `shouldSatisfy` elem "declared build-depends: text ==2.0.2"
            fixes `shouldNotSatisfy` elem "declared build-depends: text"

    describe "exactMatchOnly (the G2 safety boundary)" $ do
        it "accepts a resolution whose name matches verbatim" $
            exactMatchOnly "DataFrame" (Just ("DataFrame", fakeEntry))
                `shouldBe` Just fakeEntry

        it "rejects a near-spelling fallback — never silently guess the wrong package" $
            exactMatchOnly "DataFram" (Just ("DataFrame", fakeEntry)) `shouldBe` Nothing

        it "rejects when nothing resolved at all" $
            exactMatchOnly "Nope" Nothing `shouldBe` Nothing

    describe "missingModuleCandidates (live local store)" $ do
        let missingModuleDiag =
                "<no location info>: error: [GHC-35235]\n\
                \    Could not find module \8216Data.Text\8217.\n\
                \    It is not a module in the current program, or in any known package."
        it "declares build-depends for a module genuinely in the local store" $ do
            mGhc <- findExecutable "ghc"
            case mGhc of
                Nothing -> pendingWith "ghc not on PATH"
                Just _ -> do
                    cs <-
                        missingModuleCandidates
                            missingModuleDiag
                            "import Data.Text\nmain = print (1 :: Int)"
                    case cs of
                        ((c, fixes) : _) -> do
                            c `shouldSatisfy` T.isInfixOf "build-depends: text =="
                            case fixes of
                                [fix] ->
                                    fix
                                        `shouldSatisfy` T.isPrefixOf
                                            "declared build-depends: text =="
                                _ -> expectationFailure (show fixes)
                        [] -> expectationFailure "text is a project dependency; must resolve"

        it "never invents a package for a module that does not exist anywhere" $ do
            mGhc <- findExecutable "ghc"
            case mGhc of
                Nothing -> pendingWith "ghc not on PATH"
                Just _ -> do
                    cs <-
                        missingModuleCandidates
                            "Could not find module \8216Zzznope.Totally.Fake\8217."
                            "x = 1"
                    cs `shouldBe` []
