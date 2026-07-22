{-# LANGUAGE OverloadedStrings #-}

{- | R8-T1 (search-api.md 10): dedup participation is not assertion strength —
every found answer dedups from its 2nd occurrence, any stratum; strata 1-5
alone assert (11.1). Plus the 'heldHitLine' disclose-not-truncate clamp audit.
-}
module Test.AdviceSpec (adviceSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.Closure (heldHitLine)
import Siza.Agent.Discover.History (emptyLedger, ledgerRecord)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    MatchKind (..),
    NotebookEnv (..),
    hitJson,
    matchKindText,
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (stateOf, textField)

envT :: NotebookEnv
envT = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hkT :: HackageInfo
hkT = HackageInfo True []

{- | A found envelope whose single hit has the given kind (nullary type, so
no standing goal interferes with the dedup path under test).
-}
foundKind :: MatchKind -> Text -> Text -> Value
foundKind kind q name =
    discoverEnvelope envT (interpret envT q) 8 [okAnswer "session" [hit]] hkT
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

allKinds :: [MatchKind]
allKinds = [minBound .. maxBound]

adviceSpec :: Spec
adviceSpec = describe "dedup participation vs assertion strength (R8-T1)" $ do
    describe "every stratum dedups from its 2nd occurrence (section 10)" $
        forM_ allKinds $ \kind ->
            it (T.unpack (matchKindText kind) <> ": respelled repeat is a duplicate") $ do
                let wallFor q = foundKind kind q "swirlish"
                    (led1, out1) =
                        ledgerRecord "swirl chart" (wallFor "swirl chart") emptyLedger
                    (_, out2) =
                        ledgerRecord
                            "chart of swirls"
                            (wallFor "chart of swirls")
                            led1
                stateOf out1 `shouldBe` "found"
                stateOf out2 `shouldBe` "duplicate"
                -- From the SECOND occurrence, referencing call 1 — never a
                -- 30-turn wall (the barChart six-replay class).
                textField "ref" out2 `shouldSatisfy` T.isInfixOf "call 1"

    describe "the reference discloses evidence strength" $ do
        it "a weak wall dedups labelled weak, never promoted to a fact" $ do
            let wallFor q = foundKind MkSubstring q "swirlish"
                (led1, _) =
                    ledgerRecord "swirl chart" (wallFor "swirl chart") emptyLedger
                (_, out2) =
                    ledgerRecord "chart of swirls" (wallFor "chart of swirls") led1
            textField "summary" out2 `shouldSatisfy` T.isInfixOf "weak"
        it "a strong answer keeps the ranked-answer reference" $ do
            let wallFor q = foundKind MkExact q "swirlish"
                (led1, _) =
                    ledgerRecord "swirl chart" (wallFor "swirl chart") emptyLedger
                (_, out2) =
                    ledgerRecord "chart of swirls" (wallFor "chart of swirls") led1
            textField "summary" out2 `shouldSatisfy` T.isInfixOf "same ranked answer"

    describe "assertion strength is unchanged (11.1 / R1.4)" $ do
        forM_ [MkPrefix, MkModule, MkType, MkSubstring, MkSynonym, MkSemantic] $
            \kind ->
                it
                    ( T.unpack (matchKindText kind)
                        <> ": dedup never blocks a later honest miss"
                    )
                    $ do
                        let wallFor q = foundKind kind q "swirlish"
                            (led1, _) =
                                ledgerRecord "swirl chart" (wallFor "swirl chart") emptyLedger
                            (led2, _) =
                                ledgerRecord
                                    "chart of swirls"
                                    (wallFor "chart of swirls")
                                    led1
                            (_, out3) = ledgerRecord "swirl chart" (missFor "swirl chart") led2
                        stateOf out3 `shouldBe` "not_found"
        it "exact evidence still protects its cluster (R1.4)" $ do
            let (led1, _) =
                    ledgerRecord "gust" (foundKind MkExact "gust" "gust") emptyLedger
                (_, out2) = ledgerRecord "gust" (missFor "gust") led1
            stateOf out2 `shouldNotBe` "not_found"

    describe "heldHitLine clamp discloses, never bare-truncates" $ do
        let longType = T.intercalate " -> " (replicate 20 "Maybe (Either Text Double)")
            longHit =
                (mkHit "bars" "Cumulus.Plot" "cumulus")
                    { dhType = longType
                    , dhVersion = "0.3.1"
                    }
            shortHit =
                (mkHit "bars" "Cumulus.Plot" "cumulus")
                    { dhType = "[(Text, Double)] -> Plot -> Text"
                    , dhVersion = "0.3.1"
                    }
        it "a clamped signature names the omission and its recovery call" $ do
            let line = heldHitLine (hitJson longHit)
            line `shouldSatisfy` T.isInfixOf "truncated"
            line `shouldSatisfy` T.isInfixOf "check_type bars"
        it "a short signature travels whole, with no disclosure noise" $ do
            let line = heldHitLine (hitJson shortHit)
            line `shouldSatisfy` T.isInfixOf "[(Text, Double)] -> Plot -> Text"
            line `shouldSatisfy` (not . T.isInfixOf "truncated")
