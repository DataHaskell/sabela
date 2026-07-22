{-# LANGUAGE OverloadedStrings #-}

{- | Assertion-ledger truthfulness (search-api.md sections 10/11, R1.4, R3.8):
monotonicity over GENERATED assert\/deny\/world sequences — a fact once
asserted (or seeded at turn 0) is never later denied absent an announced
install\/restart — and answer-hash dedup: a DIFFERENT query whose ranked
answer is byte-identical returns a one-line duplicate reference.
-}
module Test.DiscoverLedgerSpec (discoverLedgerSpec) where

import Control.Monad (replicateM)
import Data.Aeson (Value, object, (.=))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.PromptCore (builtinNames)
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.Envelope (envelopeChars)
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
import Test.DiscoverFixtures (stateOf, textField)

discoverLedgerSpec :: Spec
discoverLedgerSpec = describe "discover assertion ledger (R1.4, R3.8)" $ do
    ledgerMonotonicitySpec
    answerHashSpec

envT :: NotebookEnv
envT = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hkT :: HackageInfo
hkT = HackageInfo True []

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

-- Monotonicity over generated sequences (R1.4) ------------------------------

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

{- | Replay a generated event sequence through the guard discipline
(shortcut, else record); return the protected facts that got denied.
-}
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

-- Answer-hash dedup (R3.8 extension, section 10) ----------------------------

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
        envelopeChars out2 `shouldSatisfy` (< 500)
        textField "summary" out2
            `shouldSatisfy` T.isInfixOf "did not change the answer"
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
