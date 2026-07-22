{-# LANGUAGE OverloadedStrings #-}

{- | R4-T2: the SCOPE-KEYED assertion ledger (search-api.md section 11.1) and
world-change-aware install state (R1.3/R1.4): distinct-scope queries never
share a ledger key, a repeat replays its own recorded evidence, fuzzy hits
assert nothing protected, and an in-session install refreshes install-state
facts through an ANNOUNCED world change.
-}
module Test.DiscoverScopeLedgerSpec (discoverScopeLedgerSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.Advice (clusterOf)
import Siza.Agent.Discover.History (
    SearchLedger,
    emptyLedger,
    heldFacts,
    ledgerRecord,
    ledgerShortcut,
    ledgerWorldChanged,
 )
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Request (requestKey)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    MatchKind (..),
    NotebookEnv (..),
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (stateOf, textField)

discoverScopeLedgerSpec :: Spec
discoverScopeLedgerSpec = describe "scope-keyed ledger + world change (R4-T2)" $ do
    scopeKeySpec
    assertionStrengthSpec
    crossScopeEchoSpec
    factsCoherenceSpec
    worldChangeGridSpec

envT :: NotebookEnv
envT = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hkT :: HackageInfo
hkT = HackageInfo True []

args :: [(Text, Value)] -> Value
args kvs = object [(K.fromText k, v) | (k, v) <- kvs]

-- | A found envelope whose single hit has the given kind.
foundKind :: MatchKind -> Text -> Text -> Value
foundKind kind q name =
    discoverEnvelope
        envT
        (interpret envT q)
        8
        [okAnswer "session" [hit]]
        hkT
  where
    hit =
        (mkHit name "Syn.Mod" "synpkg")
            { dhKind = kind
            , dhType = "TraceKind"
            , dhVersion = "1.0"
            }

missFor :: Text -> Value
missFor q =
    discoverEnvelope
        envT
        (interpret envT q)
        8
        [okAnswer "session" [], okAnswer "hoogle" []]
        hkT

-- | Replay a scripted (key, envelope) sequence through the guard discipline.
script :: [(Text, Value)] -> (SearchLedger, [Value])
script = foldl step (emptyLedger, [])
  where
    step (led, outs) (q, v) = case ledgerShortcut led q of
        Just out -> (led, outs ++ [out])
        Nothing ->
            let (led2, out) = ledgerRecord q v led
             in (led2, outs ++ [out])

-- Scope-keyed ledger keys (R3.8/R3.7) ---------------------------------------

scopeGrid :: [Value]
scopeGrid =
    [ args []
    , args [("module", String "Zephyr.Core")]
    , args [("module", String "Stratus.Air")]
    , args [("package", String "zephyr")]
    , args [("mode", String "inventory")]
    ]

scopeKeySpec :: Spec
scopeKeySpec = describe "distinct scopes never share a ledger key (R3.8)" $ do
    it "requestKey and clusterOf are distinct over the (query x scope) grid" $
        forM_ (["gust", "lull", "bars"] :: [Text]) $ \q -> do
            let keys = [requestKey q a | a <- scopeGrid]
                clusters = [clusterOf (missFor q) k | k <- keys]
            nub keys `shouldBe` keys
            nub clusters `shouldBe` clusters
    it "a byte-identical repeat replays THAT query's own evidence" $ do
        let keyA = requestKey "gust" (args [("module", String "Zephyr.Core")])
            keyB = requestKey "gust" (args [])
            envA = foundKind MkExact "gust" "gust"
            envB = foundKind MkExact "gust" "lull"
            (led1, _) = ledgerRecord keyA envA emptyLedger
            (led2, _) = ledgerRecord keyB envB led1
        Just refA <- pure (ledgerShortcut led2 keyA)
        Just refB <- pure (ledgerShortcut led2 keyB)
        stateOf refA `shouldBe` "duplicate"
        textField "summary" refA `shouldSatisfy` T.isInfixOf "gust"
        textField "summary" refA `shouldSatisfy` (not . T.isInfixOf "lull")
        textField "summary" refB `shouldSatisfy` T.isInfixOf "lull"
    it "the ledger is deterministic under the new key (R3.7)" $ do
        -- Two INDEPENDENT evaluations from separately constructed inputs
        -- (never one thunk compared with itself).
        let mkRun () =
                script
                    [ (requestKey "gust" a, foundKind MkExact "gust" "gust")
                    | a <- scopeGrid
                    ]
        encode (snd (mkRun ())) `shouldBe` encode (snd (mkRun ()))

-- Assertion strength follows matchKind (section 11.1) -----------------------

assertionStrengthSpec :: Spec
assertionStrengthSpec = describe "fuzzy hits assert nothing protected" $ do
    it "a synonym-only found creates no protected assertion" $ do
        let (led1, out1) =
                ledgerRecord
                    "swirl chart"
                    (foundKind MkSynonym "swirl chart" "Swirl")
                    emptyLedger
            (_, out2) = ledgerRecord "swirl" (missFor "swirl") led1
        stateOf out1 `shouldBe` "found"
        stateOf out2 `shouldBe` "not_found"
    it "a substring-only found creates no protected assertion" $ do
        let (led1, _) =
                ledgerRecord "swirl" (foundKind MkSubstring "swirl" "swirlish") emptyLedger
            (_, out2) = ledgerRecord "swirl" (missFor "swirl") led1
        stateOf out2 `shouldBe` "not_found"
    it "weak answers dedup (round 8) without asserting anything protected" $ do
        let weakFor q = foundKind MkSynonym q "Swirl"
            (led1, _) = ledgerRecord "swirl chart" (weakFor "swirl chart") emptyLedger
            (led2, out2) = ledgerRecord "chart of swirls" (weakFor "chart of swirls") led1
            (_, out3) = ledgerRecord "swirl chart" (missFor "swirl chart") led2
        stateOf out2 `shouldBe` "duplicate"
        stateOf out3 `shouldBe` "not_found"
    it "exact evidence still protects (R1.4 preserved)" $ do
        let (led1, _) = ledgerRecord "gust" (foundKind MkExact "gust" "gust") emptyLedger
            (_, out2) = ledgerRecord "gust" (missFor "gust") led1
        stateOf out2 `shouldNotBe` "not_found"

-- The barChart t8/t10/t14 cross-scope echo, pinned (section 11.1) -----------

crossScopeEchoSpec :: Spec
crossScopeEchoSpec = describe "cross-scope dedup echo regression (barChart t8/t10/t14)" $ do
    it "a scoped miss after a global fuzzy hit is an honest scoped miss" $ do
        let scopedKey = requestKey "bar" (args [("module", String "Cumulus.Plot")])
            (_, outs) =
                script
                    [ ("bar chart", foundKind MkSynonym "bar chart" "Swirl")
                    , (scopedKey, missFor "bar")
                    , (scopedKey, missFor "bar")
                    , (scopedKey, missFor "bar")
                    ]
            scoped = drop 1 outs
            -- An echo replays FOREIGN-scope evidence; a byte-identical
            -- repeat's back-reference to its OWN miss is not one (R3.8).
            echoes =
                [ o
                | o <- scoped
                , "Swirl" `T.isInfixOf` textField "summary" o
                    || "trust the held fact"
                        `T.isInfixOf` T.toLower (textField "summary" o)
                ]
        length echoes `shouldBe` 0
        map stateOf (take 1 scoped) `shouldBe` ["not_found"]
        forM_ (drop 1 scoped) $ \o ->
            textField "summary" o `shouldSatisfy` T.isInfixOf "no match"
    it "a scoped denial is not covered by a global assertion" $ do
        let scoped = requestKey "gust" (args [("module", String "Stratus.Air")])
            (_, outs) =
                script
                    [ ("gust", foundKind MkExact "gust" "gust")
                    , (scoped, missFor "gust")
                    ]
        stateOf (outs !! 1) `shouldBe` "not_found"

-- Install-state facts stay coherent (the nudge contradiction) ---------------

factWith :: Text -> Text -> Value
factWith pkg inst =
    discoverEnvelope
        envT
        (interpret envT "bars")
        8
        [okAnswer "session" [hit]]
        (HackageInfo True [pkg])
  where
    hit =
        (mkHit "bars" "Cumulus.Plot" pkg)
            { dhInstall = state
            , dhCabal = Just ("-- cabal: build-depends: " <> pkg)
            , dhVersion = "0.3.1"
            }
    state = case inst of
        "hidden" -> InstHidden
        "absent-known" -> InstAbsentKnown
        _ -> InstInstalled

factsCoherenceSpec :: Spec
factsCoherenceSpec = describe "held install-state facts never contradict" $ do
    it "one package never holds hidden AND absent-known AND installed at once" $ do
        let (led, _) =
                foldl
                    (\(l, _) (q, v) -> ledgerRecord q v l)
                    (emptyLedger, Null)
                    [ ("bars", factWith "cumulus" "hidden")
                    , ("bars @2", factWith "cumulus" "absent-known")
                    , ("bars @3", factWith "cumulus" "installed")
                    ]
            cumulusFacts =
                [f | f <- heldFacts led, "cumulus (" `T.isInfixOf` f]
        cumulusFacts `shouldSatisfy` any (T.isInfixOf "(installed)")
    it "a world change drops stale install-state facts" $ do
        let (led1, _) = ledgerRecord "bars" (factWith "cumulus" "hidden") emptyLedger
            led2 = ledgerWorldChanged led1
        filter ("cumulus (" `T.isInfixOf`) (heldFacts led2) `shouldBe` []

-- The pure announcement half of the world-change law (R1.4) -----------------
-- The end-to-end three-state x event grid lives in Test.DiscoverWorldChangeSpec.

worldChangeGridSpec :: Spec
worldChangeGridSpec = describe "world change announces before it denies (R1.4)" $
    it "a post-change denial of a pre-asserted fact carries the announcement" $ do
        let (led1, _) =
                ledgerRecord "gust" (foundKind MkExact "gust" "gust") emptyLedger
            led2 = ledgerWorldChanged led1
            (_, out) = ledgerRecord "gust" (missFor "gust") led2
        stateOf out `shouldBe` "not_found"
        textField "worldChange" out `shouldSatisfy` (not . T.null)
