{-# LANGUAGE OverloadedStrings #-}

{- | The post-union scope predicate (search-api.md 3.3, R3.3/R2.7): filters
apply over ATTRIBUTED modules at the merge, never pre-query; scoped-empty
while the unscoped union is non-empty always discloses what was removed.
-}
module Test.DiscoverScopeConserveSpec (discoverScopeConserveSpec) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.Facts (factPackages)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (
    discoverEnvelopeRecent,
    discoverEnvelopeScoped,
 )
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    NotebookEnv (..),
    Scope (..),
    SourceAnswer (..),
    emptyScope,
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (field, hitsOf, stateOf, textField)

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
discoverScopeConserveSpec = do
    refinementSpec
    factPackagesSpec
    scopedCardSpec
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

{- | live_test36: `module=Data.Csv query=!?` answered with a card listing
Data.ByteString's exports, because the install-state probe browses the top
hit's module. A card for a module the caller did not scope to reads as an
answer about the module it asked for.
-}
scopedCardSpec :: Spec
scopedCardSpec = describe "a scoped request never carries a foreign card" $ do
    let cardFor m =
            object
                [ "module" .= (m :: T.Text)
                , "status" .= ("ok" :: T.Text)
                , "exports" .= (["x :: Int"] :: [T.Text])
                ]
        envWith scope c =
            discoverEnvelopeScoped
                env0
                (interpret env0 "!?")
                scope
                8
                [(okAnswer "session" []){saCard = Just c}]
                hk0
    it "drops a card whose module is not the scoped one" $
        field
            "card"
            (envWith (Scope (Just "Data.Csv") Nothing) (cardFor "Data.ByteString"))
            `shouldBe` Nothing
    it "keeps the card when it IS the scoped module" $
        fmap
            (textField "module")
            (field "card" (envWith (Scope (Just "Data.Csv") Nothing) (cardFor "Data.Csv")))
            `shouldBe` Just "Data.Csv"
    it "leaves an unscoped request's card alone" $
        fmap
            (textField "module")
            (field "card" (envWith (Scope Nothing Nothing) (cardFor "Data.ByteString")))
            `shouldBe` Just "Data.ByteString"

{- | Refinement: a search RANKS by what the session has already established
(the held facts' packages), so successive searches narrow instead of starting
blind. live_test33: every prior call had been about dataframe, then `summary`
ranked blaze-html's attribute top. Order only — never a filter, never a
suppression: the goal gate already showed what ledger memory does when it
withholds results (honeycomb).
-}
refinementSpec :: Spec
refinementSpec = describe "a search refines what the session established" $ do
    let hit n p =
            (mkHit n "M" p)
                { dhType = "X -> Y"
                , dhInstall = InstHidden
                }
        -- The established package's hit LOSES every static tie-break (later
        -- name, same-length package), so a flipped order can only be the
        -- footprint band.
        answers = [okAnswer "hoogle" [hit "colAaa" "strange", hit "colZzz" "session"]]
        rankedWith recent =
            map
                (textField "name")
                ( hitsOf
                    ( discoverEnvelopeRecent
                        recent
                        env0
                        (interpret env0 "col")
                        emptyScope
                        8
                        answers
                        hk0
                    )
                )
    it "a session-established package leads its stratum" $
        head (rankedWith ["session"]) `shouldBe` "colZzz"
    it "with no session footprint the order is the static one" $
        head (rankedWith []) `shouldBe` "colAaa"
    it "refinement never drops the stranger" $
        length (rankedWith ["session"]) `shouldBe` 2

-- | 'factPackages': the packages held facts establish, from both fact shapes.
factPackagesSpec :: Spec
factPackagesSpec = describe "the session footprint from held facts" $ do
    it "reads an install fact's package" $
        factPackages
            ["dataframe (hidden): -- cabal: build-depends: dataframe — provides `readCsv`"]
            `shouldBe` ["dataframe"]
    it "reads a signature fact's provenance" $
        factPackages
            ["`bars` :: [(Text, Double)] -> Plot -> Text — found in Cumulus.Plot (cumulus)"]
            `shouldBe` ["cumulus"]
    it "holds nothing for a compiler fact with no provenance" $
        factPackages
            ["`defaultReadOptions` :: ReadOptions — confirmed by the compiler (check_type)"]
            `shouldBe` []
