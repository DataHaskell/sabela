{-# LANGUAGE OverloadedStrings #-}

{- | The non-mutating write frontier: what try admits relative to what a write
admits (C1-5a), how it classifies GHCi meta-commands (C2-try-meta), and whether
its refusals are stated as a grammar the surface can publish (C1-5d).
-}
module Test.TryFrontierSpec (spec) where

import Data.Either (isRight)
import Data.List (foldl', isInfixOf)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Sabela.AI.Capabilities.Edit.ScratchVet (
    cellImportLines,
    scopeEstablishLines,
 )
import Sabela.AI.Capabilities.Query (importScopeLines)
import Sabela.AI.Capabilities.Try.Payload (
    refusalClauseFor,
    trialPlanErrorText,
    trialRefusalClauses,
 )
import Sabela.AI.Capabilities.Try.Tier (tierPairs, tierText)
import Sabela.AI.Capabilities.TryPlan
import Sabela.AI.ToolDoc (
    trialRefusalGrammar,
    trialTierNames,
    tryDescription,
 )
import Sabela.Model (CellType (..))
import Sabela.Output (displayPrelude)
import Sabela.Parse (validateCellShape)
import Test.Live (requireExecutable, requireLiveFor)
import Test.ScopeModel (
    applyScopeLine,
    askedModule,
    earlierModule,
    observedScope,
    preludeScope,
 )
import Test.TryFrontierGen

writeAccepts :: Candidate -> Bool
writeAccepts c = isNothing (validateCellShape CodeCell (candSource c))

refusalNamesTheWriter :: Candidate -> Bool
refusalNamesTheWriter c = case planTrial (candSource c) of
    Right _ -> True
    Left err -> "insert_cell" `T.isInfixOf` trialPlanErrorText err

{- | The admissibility gap: every grid source a write's shape gate accepts and
the planner refuses. Measured, not asserted; the tests below state it as the
shapes that remain, so shrinking or widening it moves them.
-}
gapCandidates :: [Candidate]
gapCandidates =
    [ c
    | c <- fullGrid
    , writeAccepts c
    , not (isRight (planTrial (candSource c)))
    ]

{- | Phrases that would make a refusal a prediction about a tool try did not
run. The refusal may name the tool; it may not say what that tool will decide.
-}
verdictClaims :: [Text]
verdictClaims =
    [ "compiles it"
    , "will compile"
    , "under the same gate"
    , "accepts it"
    , "would accept"
    , "will accept"
    ]

-- | One sample per constructor of 'TrialPlanError', all six classes.
refusalSamples :: [TrialPlanError]
refusalSamples =
    [ TrialEmpty
    , TrialMultipleExpressions
    , TrialExpressionNotFinal
    , TrialMetaCommand ":! rm -rf ."
    , TrialMetaQuery (MetaQuery ":type" "insert" ["import Data.Map"])
    , TrialUnsafeSyntax "candidate contains an FFI declaration"
    ]

spec :: Spec
spec = describe "the non-mutating write frontier" $ do
    describe "C1-5a: whatever a write accepts, try can check" $ do
        prop "admits it, or its refusal names the tool that does accept it" $
            forAll genCandidate $ \c ->
                writeAccepts c ==> refusalNamesTheWriter c

        prop "admits every effect spelling, and forces containment on it" $
            forAll genEffectCandidate $ \c ->
                case planTrial (candSource c) of
                    Left err ->
                        counterexample
                            ("refused: " <> T.unpack (trialPlanErrorText err))
                            False
                    Right plan ->
                        counterexample "an effect may not take the live fast path" $
                            candidateNeedsDisposable plan
                                .&&. trialTier plan === TierContained

        it "closes the admissibility gap over the whole grid" $
            map candShape gapCandidates
                `shouldMatchList` [ candShape c
                                  | c <- fullGrid
                                  , candShape c == STwoExprs
                                  ]

        it "no effect spelling is left in the gap" $
            [candShape c | c <- gapCandidates, candShape c `elem` effectShapes]
                `shouldBe` []

        it "every source left in the gap is refused by naming the accepting tool" $
            [candSource c | c <- gapCandidates, not (refusalNamesTheWriter c)]
                `shouldBe` []

    describe "C2-try-meta: a meta-command is classified by its effect" $ do
        prop "a type-environment query is a question, not a refusal" $
            forAll (genMetaSource queryCommands) $ \(cmd, subject, imports, src) ->
                case planTrial src of
                    Left (TrialMetaQuery q) ->
                        mqCommand q
                            === cmd
                            .&&. mqSubject q
                                === subject
                            .&&. mqImports q
                                === imports
                    other ->
                        counterexample ("expected a routed query, got " <> show other) False

        prop "a command that can touch state stays refused, by name" $
            forAll (genMetaSource effectfulCommands) $ \(cmd, _, _, src) ->
                case planTrial src of
                    Left (TrialMetaCommand got) ->
                        counterexample "the refusal must name the command" $
                            cmd `T.isInfixOf` got
                    other ->
                        counterexample ("expected a refusal, got " <> show other) False

        prop "no meta-command is ever smuggled into a plan's setup" $
            forAll (genMetaSource (queryCommands <> effectfulCommands)) $
                \(cmd, _, _, src) ->
                    case planTrial src of
                        Right plan ->
                            counterexample "a meta-command reached the setup" $
                                not (cmd `T.isInfixOf` trialSetup plan)
                        Left _ -> property True

        prop "a refusal names a tool that does answer the question" $
            forAll (genMetaSource effectfulCommands) $ \(_, _, _, src) ->
                case planTrial src of
                    Left err ->
                        let msg = T.unpack (trialPlanErrorText err)
                         in counterexample msg $
                                "check_type" `isInfixOf` msg || "discover" `isInfixOf` msg
                    Right _ -> counterexample "expected a refusal" False

    describe "C1-5a: the surface states the contract the payload carries" $ do
        it "names every tier a trial's payload can carry" $
            map tierText [minBound .. maxBound] `shouldMatchList` trialTierNames

        it "publishes each of them in the description both surfaces read" $
            [t | t <- trialTierNames, not (t `T.isInfixOf` tryDescription)]
                `shouldBe` []

        it "publishes the execution fact the tier is read from" $
            ("executed" :: Text) `shouldSatisfy` (`T.isInfixOf` tryDescription)

        prop "states a tier only beside the execution that decided it" $
            forAll (elements [Nothing, Just True, Just False]) $ \evaluated ->
                map fst (tierPairs evaluated)
                    === maybe [] (const ["executed", "tier"]) evaluated

    describe "C2-checktype-imports: a scope can be asked for by name" $ do
        prop "answers in the scope asked for, not the one asked for before" $
            forAll genImportRequest $ \earlier ->
                forAll genImportRequest $ \asked ->
                    let established =
                            foldl' applyScopeLine [] (linesFor earlier <> linesFor asked)
                        linesFor = scopeEstablishLines . T.unlines . importScopeLines
                     in established
                            === preludeScope <> map moduleOf (importScopeLines asked)

        prop "never withdraws the scratchpad's own prelude from scope" $
            forAll genImportRequest $ \asked ->
                let established =
                        foldl'
                            applyScopeLine
                            []
                            (scopeEstablishLines (T.unlines (importScopeLines asked)))
                 in conjoin
                        [ counterexample (T.unpack m) (m `elem` established)
                        | m <- preludeScope
                        ]

        it "establishes that scope in a real GHCi, not only in the model" $ do
            requireLiveFor "the scratchpad scope probe"
            requireExecutable "ghci"
            scope <-
                observedScope
                    ( cellImportLines displayPrelude
                        <> scopeEstablishLines ("import " <> earlierModule)
                        <> scopeEstablishLines ("import " <> askedModule)
                    )
            scope `shouldContain` [askedModule]
            scope `shouldNotContain` [earlierModule]
            [m | m <- preludeScope, m `notElem` scope] `shouldBe` []

        prop "every module asked for survives, exactly once prefixed" $
            forAll genImportRequest $ \asked ->
                let out = importScopeLines asked
                    wanted = [T.strip a | a <- asked, not (T.null (T.strip a))]
                 in length out
                        === length wanted
                        .&&. conjoin
                            [ counterexample (T.unpack line) $
                                "import " `T.isPrefixOf` line
                                    && not ("import import" `T.isInfixOf` line)
                                    && moduleOf want `T.isInfixOf` line
                            | (line, want) <- zip out wanted
                            ]

    describe "C1-5d: try's refusals are publishable as a grammar" $ do
        it "states one clause per refusal class" $
            trialRefusalClauses `shouldNotBe` []

        it "returns exactly the clauses the published grammar states" $
            trialRefusalClauses `shouldMatchList` trialRefusalGrammar

        it "states each of them in the description both surfaces read" $
            [c | c <- trialRefusalGrammar, not (c `T.isInfixOf` tryDescription)]
                `shouldBe` []

        it "every refusal the planner can return is covered by a clause" $
            mapM_
                ( \err ->
                    let msg = trialPlanErrorText err
                     in (err, refusalClauseFor err `T.isInfixOf` msg)
                            `shouldBe` (err, True)
                )
                refusalSamples

        it "publishes one clause per refusal the planner can return" $
            length trialRefusalClauses `shouldBe` length refusalSamples

        it "each published clause names exactly one refusal class" $
            [ ( err
              , [ e
                | e <- refusalSamples
                , refusalClauseFor err `T.isInfixOf` trialPlanErrorText e
                ]
              )
            | err <- refusalSamples
            ]
                `shouldSatisfy` all ((== 1) . length . snd)

        prop "a refusal never predicts what the tool it names will decide" $
            forAll genCandidate $ \c ->
                case planTrial (candSource c) of
                    Right _ -> property True
                    Left err ->
                        let msg = T.toLower (trialPlanErrorText err)
                         in counterexample (T.unpack msg) $
                                conjoin [not (claim `T.isInfixOf` msg) | claim <- verdictClaims]
