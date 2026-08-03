{-# LANGUAGE OverloadedStrings #-}

{- | C1-22 and C3-5: the harness's own messages report facts. They do not tell
the model what to conclude, they are not stamped with the name of a tool the
model never called, and an ok verdict carries the check that produced it.
-}
module Test.HarnessTruthSpec (harnessTruthSpec) where

import Data.Aeson (Value)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.CellResult (CellId)
import Sabela.AI.Verdict (VerdictClass (..), parseVerdict)
import Siza.Agent.Check (noVerdictNote, noVerdicts)
import Siza.Agent.Messages (
    doneSignalMsg,
    noCheckSignalMsg,
    reenterAlarmMsg,
    reenterMsg,
    streakMsg,
    unconfirmedMsgWith,
    verifyMsgWith,
 )
import Siza.Agent.Owned (OwnedCell (..), hasArtifact, noWriteReason)
import Siza.Agent.Tools (offeredNames)
import Test.DiscoverFixtures (textField)
import Test.TruthGen (genCheckOver, genGhcDiagnostic, genIdent, genSource)

{- | Phrases that tell the model what to do next. A tool result states what is
so; the loop decides on its own when the episode ends.
-}
imperatives :: [Text]
imperatives =
    [ "reply with"
    , "and stop"
    , "do not stop"
    , "finish with"
    , "then stop"
    ]

harnessTruthSpec :: Spec
harnessTruthSpec = describe "harness-authored messages (C1-22, C3-5)" $ do
    it "never issues an imperative on the verdict channel" $
        property $
            forAll harnessMessages $ \msgs ->
                [ (p, textField "content" m)
                | m <- msgs
                , p <- imperatives
                , p `T.isInfixOf` T.toLower (textField "content" m)
                ]
                    `shouldBe` []

    it "is never stamped with the name of a tool the model can call" $
        property $
            forAll harnessMessages $ \msgs ->
                [ textField "tool_name" m
                | m <- msgs
                , textField "tool_name" m `elem` offeredNames
                ]
                    `shouldBe` []

    it "every message it emits carries a decodable verdict class" $
        property $
            forAll verdictMessages $ \msgs ->
                [ textField "content" m
                | m <- msgs
                , isNothing (parseVerdict (textField "content" m))
                ]
                    `shouldBe` []

    it "an ok verdict quotes the check that produced it" $
        property $
            forAll verdictMessages $ \msgs ->
                [ textField "content" m
                | m <- msgs
                , parseVerdict (textField "content" m) == Just VerdictOk
                , not ("`" `T.isInfixOf` textField "content" m)
                ]
                    `shouldBe` []

    it "a check that reached no verdict is could-not-run, never ok" $
        property $
            forAll (elements (Nothing : map (Just . noVerdictNote) noVerdicts)) $
                \why ->
                    parseVerdict (textField "content" (noCheckSignalMsg why))
                        `shouldBe` Just VerdictCouldNotRun

    it "names which reason applied whenever one is known" $
        property $
            forAll (elements noVerdicts) $ \r ->
                textField "content" (noCheckSignalMsg (Just (noVerdictNote r)))
                    `shouldSatisfy` T.isInfixOf (noVerdictNote r)

    noWriteReasonSpec

{- | The reason a turn had nothing to check is read off the cells the turn
recorded, so it can state neither more nor less than that map holds.
-}
noWriteReasonSpec :: Spec
noWriteReasonSpec = describe "the no-write reason answers to its own map" $ do
    it "gives a reason exactly when no committed cell is executable" $
        property $
            forAll (listOf genSource) $ \srcs ->
                isJust (noWriteReason (ownedOf srcs))
                    `shouldBe` not (hasArtifact (ownedOf srcs))

    it "says no cell has been committed only when the map holds none" $
        property $
            forAll (listOf genSource) $ \srcs ->
                ("no cell has been committed" `T.isInfixOf` reasonNote srcs)
                    `shouldBe` null srcs

    it "counts the cells it did commit when none of them is executable" $
        property $
            forAll (listOf genSource `suchThat` preambleOnly) $ \srcs ->
                reasonNote srcs
                    `shouldSatisfy` T.isInfixOf (T.pack (show (length srcs)))
  where
    preambleOnly srcs = not (null srcs) && not (hasArtifact (ownedOf srcs))

reasonNote :: [Text] -> Text
reasonNote = maybe "" noVerdictNote . noWriteReason . ownedOf

-- | Cells a turn might have committed, executable or not.
ownedOf :: [Text] -> Map.Map CellId OwnedCell
ownedOf srcs = Map.fromList (zip [0 ..] [OwnedCell True "" s False | s <- srcs])

{- | Every message the harness itself writes, over generated owned counts,
missing names, diagnostics and guidance — so a property about what the harness
may say holds for each constructor rather than for the ones under suspicion.
-}
harnessMessages :: Gen [Value]
harnessMessages = do
    verdicts <- verdictMessages
    reds <- listOf1 ((,) <$> choose (0, 40) <*> genGhcDiagnostic)
    alarms <- vectorOf (length reds) arbitrary
    contrast <- genGhcDiagnostic
    pure $
        verdicts
            ++ [ reenterMsg reds
               , reenterAlarmMsg [(c, d, a) | ((c, d), a) <- zip reds alarms]
               , streakMsg contrast
               ]

{- | The subset stamped with a verdict class: the channel the loop's own
verdicts go out on.
-}
verdictMessages :: Gen [Value]
verdictMessages = do
    n <- genIdent
    check <- genCheckOver [n]
    cids <- listOf (choose (0, 40))
    ownedCount <- choose (0, 5)
    missing <- listOf genIdent
    guidance <- elements [Nothing, Just "print the value and re-check"]
    why <- elements (Nothing : map (Just . noVerdictNote) noVerdicts)
    ce <-
        elements [Nothing, Just ("This required example fails: `" <> check <> "`.")]
    pure
        [ doneSignalMsg cids check
        , noCheckSignalMsg why
        , verifyMsgWith ownedCount missing ce
        , unconfirmedMsgWith ownedCount missing guidance
        ]
