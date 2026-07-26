{-# LANGUAGE OverloadedStrings #-}

{- | R8-T3 x R3.10 (x G3): candidate writability over GENERATED ledgers on
the synthetic catalogue — paste-valid lines, only ledger-verified names,
nothing from a fact-free ledger, and never a hole: an argument slot is a
literal or a producer a harness hole probe established, else there is no
candidate. Plus the R7.6 discover-findability cross-check.
-}
module Test.CandidateSpec (candidateSpec) where

import Control.Monad (forM_, unless, when)
import Data.Aeson (Value)
import Data.List (nub, subsequences)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import qualified Data.Set as Set

import Siza.Agent.Discover.Candidate (
    candidateCell,
    candidateClause,
    candidateClauseAgainst,
    candidateGaps,
    candidateNames,
 )
import Siza.Agent.Discover.Goal (argTypesOf, literalFill)
import Siza.Agent.Discover.History (emptyLedger, heldFacts, ledgerRecord)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Ledger (normaliseSource)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    NotebookEnv (..),
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (
    SynPkg (..),
    installNamesFile,
    runCat,
    stateOf,
    synHoogle,
 )

envT :: NotebookEnv
envT = seededBuiltins (NotebookEnv [] [] [] [] [] [])

-- | One catalogue entry: package, hidden flag, module, export name, type.
type Entry = (Text, Bool, Text, Text, Text)

entries :: [Entry]
entries =
    [ (spName p, spHidden p, m, n, ty)
    | p <- synHoogle
    , (m, es) <- spModules p
    , (n, ty) <- es
    ]

{- | The found envelope an entry's discover answer would carry: an exact
typed hit, with the cabal line when the package is not plainly installed.
-}
entryEnvelope :: Entry -> Value
entryEnvelope (pkg, hidden, m, n, ty) =
    discoverEnvelope
        envT
        (interpret envT n)
        8
        [okAnswer "session" [hit]]
        (HackageInfo True [])
  where
    hit =
        (mkHit n m pkg)
            { dhType = ty
            , dhVersion = "1.0"
            , dhInstall = if hidden then InstHidden else InstInstalled
            , dhCabal =
                if hidden
                    then Just ("-- cabal: build-depends: " <> pkg)
                    else Nothing
            }

{- | The held facts of a ledger fed the given entries in order, PLUS the
hole-probe conclusion for every argument type no literal can fill — the
harness answers those before any candidate can be proposed (G3).
-}
factsFor :: [Entry] -> [Text]
factsFor es = harvested ++ map probedFact (gapTypes es)
  where
    harvested =
        heldFacts $
            foldl
                (\led e@(_, _, _, n, _) -> fst (ledgerRecord n (entryEnvelope e) led))
                emptyLedger
                es

gapTypes :: [Entry] -> [Text]
gapTypes es =
    nub [t | (_, _, _, _, ty) <- es, t <- argTypesOf ty, isNothing (literalFill t)]

probedFact :: Text -> Text
probedFact t = "`" <> t <> "` is produced by: `mk" <> t <> "` (via: hole-probe)"

-- | Generated ledger grid: every 1- and 2-entry sequence over the catalogue.
ledgerGrid :: [[Entry]]
ledgerGrid = [es | es <- subsequences entries, not (null es), length es <= 2]

{- | The fill of one argument slot: a literal when constructible, else the
probed producer — mirroring 'Siza.Agent.Discover.Candidate.fillArg'.
-}
expectedArg :: Text -> Text
expectedArg t = fromMaybe ("mk" <> t) (literalFill t)

expectedCall :: Text -> Text -> Text
expectedCall n ty = T.unwords (n : map expectedArg (argTypesOf ty))

candidateSpec :: Spec
candidateSpec = describe "candidate cell (R8-T3 / R3.10 / G3)" $ do
    describe "writability over the generated ledger grid" $ do
        it "every candidate is paste-valid: cabal / import / complete application" $
            forM_ ledgerGrid $ \es -> do
                let facts = factsFor es
                case candidateCell facts of
                    Nothing ->
                        expectationFailure
                            ("no candidate despite held consumer: " <> show es)
                    Just src -> forM_ (T.lines src) $ \l -> do
                        let ok =
                                "-- cabal: build-depends: " `T.isPrefixOf` l
                                    || "import " `T.isPrefixOf` l
                                    || isApplication facts l
                        (l, ok) `shouldBe` (l, True)
        it "no candidate ever contains a typed hole" $
            forM_ ledgerGrid $ \es ->
                candidateCell (factsFor es)
                    `shouldSatisfy` maybe True (not . T.isInfixOf "_ ::")
        it "the application line fills literals and probed producers" $
            forM_ entries $ \e@(_, _, _, n, ty) -> do
                let facts = factsFor [e]
                case candidateCell facts of
                    Nothing -> expectationFailure "no candidate"
                    Just src -> last (T.lines src) `shouldBe` expectedCall n ty
        it "an unprobed genuine gap proposes nothing at all (G3)" $ do
            -- bars :: [(Text, Double)] -> Plot -> Text: the tuple-list fills,
            -- but nothing is known to produce Plot, so there is no candidate.
            let facts =
                    [ "`bars` :: [(Text, Double)] -> Plot -> Text \
                      \— found in Cumulus.Plot (cumulus)"
                    ]
            candidateGaps facts `shouldBe` ["Plot"]
            candidateCell facts `shouldBe` Nothing
        it "a probed producer fills the gap and closes it (G3)" $ do
            let facts =
                    [ "`bars` :: [(Text, Double)] -> Plot -> Text \
                      \— found in Cumulus.Plot (cumulus)"
                    , "`Plot` is produced by: `defaultPlot` (via: hole-probe)"
                    ]
            candidateGaps facts `shouldBe` []
            (last . T.lines <$> candidateCell facts)
                `shouldBe` Just "bars [(\"\", 0.0)] defaultPlot"
        it "a probe that found nothing leaves the gap open, never holed (G3)" $ do
            let facts =
                    [ "`bars` :: [(Text, Double)] -> Plot -> Text \
                      \— found in Cumulus.Plot (cumulus)"
                    , "no producer of `Plot` found in scope (via: hole-probe)"
                    ]
            candidateCell facts `shouldBe` Nothing
        it "an all-literal signature synthesises a hole-free candidate (R3.10)" $ do
            let facts = ["`gust` :: Int -> Int — found in Zephyr.Core (zephyr)"]
            (last . T.lines <$> candidateCell facts)
                `shouldBe` Just "gust 0"
        it "the import names the held module; cabal appears iff a line is held" $
            forM_ entries $ \e@(pkg, hidden, m, _, _) -> do
                let facts = factsFor [e]
                case candidateCell facts of
                    Nothing -> expectationFailure "no candidate"
                    Just src -> do
                        T.lines src `shouldSatisfy` elem ("import " <> m)
                        any ("-- cabal:" `T.isPrefixOf`) (T.lines src)
                            `shouldBe` hidden
                        when hidden $
                            head (T.lines src)
                                `shouldBe` "-- cabal: build-depends: " <> pkg
        it "every candidate name is ledger-verified (nothing invented)" $
            forM_ ledgerGrid $ \es -> do
                let facts = factsFor es
                forM_ (candidateNames facts) $ \nm ->
                    facts `shouldSatisfy` any (nm `T.isInfixOf`)

    describe "G5.4: a candidate the gate refused is retired" $ do
        it "zombie-candidate: the same source is never handed back once refuted" $
            forM_ (map (: []) entries) $ \es -> do
                let facts = factsFor es
                    clause = candidateClause facts
                unless (T.null clause) $ do
                    -- Refute exactly what the ledger would propose.
                    let Just src = candidateCell facts
                        refuted = Set.singleton (normaliseSource src)
                    candidateClauseAgainst refuted Nothing facts `shouldBe` ""

        it "an unrelated refutation leaves the candidate standing" $
            forM_ (map (: []) entries) $ \es -> do
                let facts = factsFor es
                    clause = candidateClause facts
                unless (T.null clause) $
                    candidateClauseAgainst
                        (Set.singleton "something = elseEntirely")
                        Nothing
                        facts
                        `shouldBe` clause

        it "matches a refuted source that only differs in whitespace" $ do
            let facts = ["`gust` :: Int -> Int — found in Zephyr.Core (zephyr)"]
                Just src = candidateCell facts
                refuted = Set.singleton (normaliseSource ("  " <> src <> "\n\n"))
            candidateClauseAgainst refuted Nothing facts `shouldBe` ""

    describe "G5.1: the clause never claims evidence it does not have" $
        it "frames the cell as a proposal, never as compiler-established" $
            forM_ (map (: []) entries) $ \es -> do
                let clause = candidateClause (factsFor es)
                unless (T.null clause) $ do
                    clause `shouldSatisfy` (not . T.isInfixOf "compiler-established")
                    clause `shouldSatisfy` (not . T.isInfixOf "closest complete source")

    describe "a fact-free ledger synthesises no candidate" $ do
        it "no facts, no candidate, no clause" $ do
            candidateCell [] `shouldBe` Nothing
            candidateClause [] `shouldBe` ""
            candidateNames [] `shouldBe` []
        it "install-only facts (no consumer signature) synthesise nothing" $ do
            let facts = ["cumulus (hidden): -- cabal: build-depends: cumulus"]
            candidateCell facts `shouldBe` Nothing

    describe "search can find every name the scaffold proposes (R7.6)" $
        beforeAll_ installNamesFile $
            it "consumer, module and package are all discover-findable" $
                forM_ (map (: []) entries) $ \es ->
                    forM_ (candidateNames (factsFor es)) $ \nm -> do
                        v <- runCat nm
                        (nm, stateOf v) `shouldBe` (nm, "found")
  where
    -- The application line: a held consumer name applied to typed holes
    -- drawn from its own held signature.
    isApplication facts l = case T.words l of
        (h : _) ->
            any (("`" <> h <> "` :: ") `T.isInfixOf`) facts
                && ( l
                        `elem` [ expectedCall h ty
                               | (_, _, _, n, ty) <- entries
                               , n == h
                               ]
                   )
        [] -> False
