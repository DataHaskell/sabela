{-# LANGUAGE OverloadedStrings #-}

module Test.DiscoverLedgerSpec (discoverLedgerSpec) where

import Control.Monad (forM_, replicateM)
import Data.Aeson (Value, object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.PromptCore (builtinNames)
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.Dedup (ledgerShortcutStep)
import Siza.Agent.Discover.Envelope (
    boundEnvelope,
    envelopeCharBudget,
    envelopeChars,
 )
import Siza.Agent.Discover.History (
    emptyLedger,
    ledgerRecord,
    ledgerSeed,
    ledgerShortcut,
    ledgerWorldChanged,
 )
import Siza.Agent.Discover.HistoryGuard (
    guardDiscover,
    newSearchLedger,
    seedSearchLedger,
 )
import Siza.Agent.Discover.Interpret (interpret, stripDecoration)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    NotebookEnv (..),
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (hitText, hitsOf, stateOf, textField)

discoverLedgerSpec :: Spec
discoverLedgerSpec = describe "discover assertion ledger (R1.4, R3.8)" $ do
    ledgerMonotonicitySpec
    answerHashSpec

envT :: NotebookEnv
envT = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hkT :: HackageInfo
hkT = HackageInfo True [] [] []

foundFor :: Text -> Value
foundFor n =
    discoverEnvelope
        envT
        (interpret envT n)
        8
        [okAnswer "session" [(mkHit n "Syn.Mod" "synpkg"){dhVersion = "1.0"}]]
        hkT

missFor :: Text -> Value
missFor q =
    discoverEnvelope
        envT
        (interpret envT q)
        8
        [okAnswer "session" [], okAnswer "hoogle" []]
        hkT

data Ev = Assert Text | Deny Text | World

evAlphabet :: [Ev]
evAlphabet =
    [ Assert "alpha"
    , Deny "alpha"
    , Deny "alpha @Int"
    , Assert "beta"
    , Deny "beta"
    , World
    , Deny "gamma"
    , Deny "displayHtml"
    ]

seededFacts :: Set Text
seededFacts = Set.fromList (map T.toLower ("gamma" : builtinNames))

clusterName :: Text -> Text
clusterName = T.toLower . T.takeWhile (/= ' ') . stripDecoration

replay :: [Ev] -> [Text]
replay evs = viols
  where
    led0 = ledgerSeed ("gamma" : builtinNames) emptyLedger
    (_, _, viols) = foldl step (led0, Set.empty, []) evs
    step (led, asserted, bad) ev = case ev of
        Assert n ->
            let (led', _) = ledgerRecord n (foundFor n) led
             in (led', Set.insert (clusterName n) asserted, bad)
        World -> (ledgerWorldChanged led, Set.empty, bad)
        Deny q ->
            let c = clusterName q
                (led', out) = case ledgerShortcut led q of
                    Just v -> (led, v)
                    Nothing -> ledgerRecord q (missFor q) led
                protected = c `Set.member` asserted || c `Set.member` seededFacts
             in ( led'
                , asserted
                , bad ++ [q | protected, textField "state" out == "not_found"]
                )

ledgerMonotonicitySpec :: Spec
ledgerMonotonicitySpec = describe "ledger monotonicity (R1.4): asserted facts are never denied" $ do
    it "holds over every generated assert/deny/world sequence of length 4" $ do
        let seqs = replicateM 4 evAlphabet
        concatMap replay seqs `shouldBe` []
    it "after an announced world change a denial is legal again" $ do
        let led0 = ledgerSeed ["gamma"] emptyLedger
            (led1, _) = ledgerRecord "alpha" (foundFor "alpha") led0
            led2 = ledgerWorldChanged led1
            (_, out) = ledgerRecord "alpha @Int" (missFor "alpha @Int") led2
        stateOf out `shouldBe` "not_found"
    it "a seeded fact survives even a world change (imports persist a restart)" $ do
        let led0 = ledgerWorldChanged (ledgerSeed ["gamma"] emptyLedger)
            (_, out) = ledgerRecord "gamma" (missFor "gamma") led0
        stateOf out `shouldNotBe` "not_found"
    it "guardDiscover blocks a backend denial of a seeded import end-to-end" $ do
        ref <- newSearchLedger
        seedSearchLedger seedDispatch ref
        let inner _ = pure (Right (ToolOk (missFor "DataFrame")))
        Right (ToolOk out) <-
            guardDiscover
                ref
                inner
                (ToolCall "discover" (object ["query" .= ("DataFrame" :: Text)]))
        stateOf out `shouldNotBe` "not_found"
  where
    seedDispatch (ToolCall "list_cells" _) =
        pure . Right . ToolOk $
            object
                [ "cells"
                    .= [ object
                            [ "source"
                                .= ("import qualified DataFrame as D" :: Text)
                            , "defines" .= ([] :: [Text])
                            ]
                       ]
                ]
    seedDispatch _ = pure (Left "unsupported")

answerHashSpec :: Spec
answerHashSpec = describe "answer-hash dedup: an unchanged answer is a one-line reference" $ do
    let hitsCol =
            [ (mkHit "col" "DataFrame.Functions" "dataframe"){dhVersion = "2.0"}
            ]
        vFor q =
            discoverEnvelope
                envT
                (interpret envT q)
                8
                [okAnswer "session" hitsCol]
                hkT
    it "a DIFFERENT query with a byte-identical ranked answer dedups" $ do
        let (led1, out1) = ledgerRecord "col @Int" (vFor "col @Int") emptyLedger
            (_, out2) = ledgerRecord "`col`" (vFor "`col`") led1
        stateOf out1 `shouldBe` "found"
        stateOf out2 `shouldBe` "duplicate"
        textField "ref" out2 `shouldSatisfy` (not . T.null)
        envelopeChars (boundEnvelope out2)
            `shouldSatisfy` (<= envelopeCharBudget)
        textField "summary" out2
            `shouldSatisfy` T.isInfixOf "same ranked answer"
        map (hitText "name") (hitsOf out2)
            `shouldBe` map (hitText "name") (hitsOf out1)
    it "a different answer is never deduped" $ do
        let other =
                discoverEnvelope
                    envT
                    (interpret envT "gust")
                    8
                    [okAnswer "session" [mkHit "gust" "Zephyr.Core" "zephyr"]]
                    hkT
            (led1, _) = ledgerRecord "col @Int" (vFor "col @Int") emptyLedger
            (_, out2) = ledgerRecord "gust" other led1
        stateOf out2 `shouldBe` "found"
    it "re-runs fully after the world changed" $ do
        let (led1, _) = ledgerRecord "col @Int" (vFor "col @Int") emptyLedger
            led2 = ledgerWorldChanged led1
            (_, out2) = ledgerRecord "`col`" (vFor "`col`") led2
        stateOf out2 `shouldBe` "found"
    it "hard-fires act-or-blocker after repeated answer hashes and stops dispatch" $ do
        ref <- newSearchLedger
        calls <- newIORef (0 :: Int)
        let inner _ = do
                modifyIORef' calls (+ 1)
                pure (Right (ToolOk (vFor "col")))
            ask q =
                guardDiscover
                    ref
                    inner
                    (ToolCall "discover" (object ["query" .= (q :: Text)]))
        mapM_ ask (["col", "`col`", "col @Int"] :: [Text])
        Right (ToolOk stopped) <- ask "col scoped differently"
        readIORef calls `shouldReturn` 3
        stateOf stopped `shouldBe` "duplicate"
        stopped `shouldSatisfy` hardStopped
        hitsOf stopped `shouldSatisfy` (not . null)
    it "has teeth over generated exact, answer-identical, and new sequences" $ do
        forM_ (replicateM 4 [ExactRepeat, AnswerRepeat, FreshAnswer]) checkTeeth
    it
        "B4/2026-07-21 specimen: ×8 query-varied asks of the same card stop \
        \re-serving it well before the 8th"
        $ do
            ref <- newSearchLedger
            calls <- newIORef (0 :: Int)
            let dfCard =
                    discoverEnvelope
                        envT
                        (interpret envT "col")
                        8
                        [okAnswer "session" hitsCol]
                        hkT
                inner _ = do
                    modifyIORef' calls (+ 1)
                    pure (Right (ToolOk dfCard))
                ask q =
                    guardDiscover ref inner (ToolCall "discover" (object ["query" .= q]))
                phrasings =
                    [ "how do I plot this dataframe"
                    , "plot the dataframe"
                    , "dataframe plotting"
                    , "render dataframe as chart"
                    , "chart the dataframe"
                    , "visualize the dataframe"
                    , "dataframe visualization"
                    , "show the dataframe as a plot"
                    ] ::
                        [Text]
            outs <- mapM ask phrasings
            let ok (Right (ToolOk v)) = v
                ok _ = object []
                states = map (stateOf . ok) outs
                lastOut = ok (last outs)
            length (filter (== "found") states) `shouldBe` 1
            stateOf lastOut `shouldBe` "duplicate"
            lastOut `shouldSatisfy` hardStopped
            dispatched <- readIORef calls
            dispatched `shouldSatisfy` (< 8)

{- | The repeat limit is identified by the reference it carries, not by a
phrase telling the caller what to do: C1-11 removed the advice.
-}
hardStopped :: Value -> Bool
hardStopped v = textField "ref" v == "discovery closed: repeat limit"

data RepeatKind = ExactRepeat | AnswerRepeat | FreshAnswer deriving (Eq)

{- | The repeat limit shows on the call it short-circuits. A step that reached
the limit inside 'ledgerRecord' already paid for its answer and returns it.
-}
checkTeeth :: [RepeatKind] -> Expectation
checkTeeth kinds = go 0 False initial (zip [1 :: Int ..] kinds)
  where
    (initial, _) = ledgerRecord "base" (foundFor "base") emptyLedger
    go _ _ _ [] = pure ()
    go repeats closed led ((i, kind) : rest) = do
        let q = case kind of
                ExactRepeat -> "base"
                AnswerRepeat -> "base variant " <> T.pack (show i)
                FreshAnswer -> "fresh" <> T.pack (show i)
            payload = case kind of
                FreshAnswer -> foundFor q
                _ -> foundFor "base"
            (shortcutLed, shortcut) = ledgerShortcutStep led q
            (led', out) = case shortcut of
                Just v -> (shortcutLed, v)
                Nothing -> ledgerRecord q payload shortcutLed
            repeats' = if kind == FreshAnswer then 0 else repeats + 1
            closed' = closed || repeats' >= 2
            hardNow = closed || (kind == ExactRepeat && repeats' >= 2)
        if hardNow
            then out `shouldSatisfy` hardStopped
            else out `shouldSatisfy` (not . hardStopped)
        go repeats' closed' led' rest
