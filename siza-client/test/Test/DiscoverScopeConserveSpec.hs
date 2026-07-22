{-# LANGUAGE OverloadedStrings #-}

{- | The post-union scope predicate (search-api.md 3.3, R3.3/R2.7): filters
apply over ATTRIBUTED modules at the merge, never pre-query; scoped-empty
while the unscoped union is non-empty always discloses what was removed.
-}
module Test.DiscoverScopeConserveSpec (discoverScopeConserveSpec) where

import Data.Aeson (Value)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelopeScoped)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    NotebookEnv (..),
    Scope (..),
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (hitsOf, stateOf, textField)

env0 :: NotebookEnv
env0 = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hk0 :: HackageInfo
hk0 = HackageInfo True []

{- | The re-export shape: one name evidenced in a re-exporting module
(session) and in its defining internal module (hoogle).
-}
reExportHits :: [DHit]
reExportHits =
    [ (mkHit "colList" "Frame" ""){dhOrigin = "session"}
    , (mkHit "colList" "Ops.Internal" "frameio"){dhOrigin = "hoogle"}
    ]

scoped :: Scope -> [DHit] -> Value
scoped scope hits =
    discoverEnvelopeScoped
        env0
        (interpret env0 "colList")
        scope
        8
        [okAnswer "hoogle" hits]
        hk0

discoverScopeConserveSpec :: Spec
discoverScopeConserveSpec =
    describe "post-union scope predicate (section 3.3)" $ do
        it "keeps an exact hit whose attributed sibling module satisfies the filter" $ do
            let v = scoped (Scope (Just "Frame") Nothing) reExportHits
            stateOf v `shouldBe` "found"
            map (textField "module") (hitsOf v)
                `shouldMatchList` ["Frame", "Ops.Internal"]

        it "conserves totals: the filtered total equals the attributed-kept count" $ do
            let v = scoped (Scope (Just "Frame") Nothing) reExportHits
            length (hitsOf v) `shouldBe` 2

        it "a filter that excludes everything discloses the removed candidates' modules" $ do
            let v = scoped (Scope (Just "Granite") Nothing) reExportHits
            stateOf v `shouldBe` "not_found"
            let narrow = textField "narrow" v
            narrow `shouldSatisfy` ("removed" `T.isInfixOf`)
            narrow `shouldSatisfy` ("Frame" `T.isInfixOf`)
            narrow `shouldSatisfy` ("Ops.Internal" `T.isInfixOf`)

        it
            "package filters honour same-name attribution (a session hit missing its package)"
            $ do
                let hits =
                        [ (mkHit "colList" "Frame" ""){dhOrigin = "session"}
                        , (mkHit "colList" "Frame.Ops" "frameio"){dhOrigin = "hoogle"}
                        ]
                    v = scoped (Scope Nothing (Just "frameio")) hits
                stateOf v `shouldBe` "found"
                length (hitsOf v) `shouldBe` 2
