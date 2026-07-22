{-# LANGUAGE OverloadedStrings #-}

{- | General envelope invariants (testing-plan R3.6, R3.8-R3.10, R1.7): every
render path stays under the 2,500-char budget by construction, decodes against
ONE declared schema with no serialisation-inside-a-string and no package-hash
names, and the tool description is generated from that same schema.
-}
module Test.DiscoverEnvelopeSpec (discoverEnvelopeSpec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Grammar (discoverGrammarBlock)
import Siza.Agent.Discover.Envelope (
    badRequest,
    boundEnvelope,
    envelopeCharBudget,
    envelopeChars,
    envelopeViolations,
    requiredHitKeys,
    schemaPromise,
 )
import Siza.Agent.Discover.History (duplicateEnvelope)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    NotebookEnv (..),
    SourceAnswer (..),
    mkHit,
    okAnswer,
    seededBuiltins,
    unavailableAnswer,
 )
import Siza.Agent.DiscoverTool (discoverToolDescription)
import Test.DiscoverFixtures (
    discoverables,
    field,
    hitText,
    hitsOf,
    installNamesFile,
    runCat,
    textField,
 )

env0 :: NotebookEnv
env0 = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hk0 :: HackageInfo
hk0 = HackageInfo True []

-- | A worst-legal hit: every optional field present and long.
wideHit :: Int -> DHit
wideHit i =
    (mkHit "col" ("Extremely.Deep.Module.Path.Number" <> tShow i) pkg)
        { dhType = T.replicate 6 "Columnable abcdefgh => Text -> Expr a -> "
        , dhVersion = "10.20.30.40"
        , dhCabal = Just ("-- cabal: build-depends: " <> pkg)
        , dhUse = Just "already imported as Q (notebook import)"
        }
  where
    pkg = "very-long-package-name-" <> tShow i

tShow :: Int -> Text
tShow = T.pack . show

-- | A worst-legal browse card: 60 long export lines.
wideCard :: Value
wideCard =
    object
        [ "module" .= ("Extremely.Deep.Module.Path" :: Text)
        , "status" .= ("ok" :: Text)
        , "exports"
            .= [ "export" <> tShow i <> " :: " <> T.replicate 4 "Text -> "
               | i <- [1 .. 60 :: Int]
               ]
        ]

ambiguous :: Int -> Int -> Value
ambiguous n limit =
    boundEnvelope
        ( discoverEnvelope
            env0
            (interpret env0 "col")
            limit
            [okAnswer "hoogle" (map wideHit [1 .. n])]
            hk0
        )

intField :: Text -> Value -> Int
intField k v = case field k v of
    Just (Number x) -> round x
    _ -> -1

discoverEnvelopeSpec :: Spec
discoverEnvelopeSpec =
    beforeAll_ installNamesFile $
        describe "discover envelope: bound, one schema, generated contract" $ do
            describe "R3.9 every render path is <= 2,500 chars by construction" $ do
                it "holds for generated ambiguous catalogues incl. limit=25" $ do
                    let cases =
                            [ ambiguous n limit
                            | n <- [0, 1, 5, 25, 40, 100]
                            , limit <- [1, 8, 25]
                            ]
                    map envelopeChars cases
                        `shouldSatisfy` all (<= envelopeCharBudget)
                it "still reconciles shown+omitted == total after bounding" $ do
                    let v = ambiguous 100 25
                    length (hitsOf v) `shouldBe` intField "shown" v
                    intField "shown" v + intField "omitted" v
                        `shouldBe` intField "total" v
                it "bounds a wide card by moving exports into moreExports" $ do
                    let v =
                            boundEnvelope
                                ( discoverEnvelope
                                    env0
                                    (interpret env0 "Extremely.Deep.Module.Path")
                                    8
                                    [ (okAnswer "session" [])
                                        { saCard = Just wideCard
                                        }
                                    ]
                                    hk0
                                )
                    envelopeChars v `shouldSatisfy` (<= envelopeCharBudget)
                it "bounds the miss, error and duplicate paths" $ do
                    let miss =
                            boundEnvelope
                                ( discoverEnvelope
                                    env0
                                    (interpret env0 "frobwizzle")
                                    8
                                    [ okAnswer "session" []
                                    , unavailableAnswer "hoogle" "no local DB"
                                    ]
                                    hk0
                                )
                        err = boundEnvelope (badRequest "" "query must be non-blank")
                        dup = duplicateEnvelope "col" "call 3" "3 hits; top: col"
                    map
                        envelopeChars
                        [miss, err, dup]
                        `shouldSatisfy` all (<= envelopeCharBudget)
                it "the whole synthetic catalogue renders under budget" $ do
                    vs <- mapM runCat discoverables
                    map envelopeChars vs
                        `shouldSatisfy` all (<= envelopeCharBudget)

            describe "R3.6/R3.10 one declared schema on every outcome" $ do
                it "hit, miss, ambiguous, error and duplicate all decode clean" $ do
                    catalogue <- mapM runCat discoverables
                    miss <- runCat "frobwizzle"
                    blank <- runCat "   "
                    let all' =
                            catalogue
                                ++ [ miss
                                   , blank
                                   , ambiguous 40 25
                                   , boundEnvelope (badRequest "" "blank")
                                   , duplicateEnvelope "q" "call 2" "no match"
                                   ]
                    concatMap envelopeViolations all' `shouldBe` []
                it "flags a serialisation format inside a string" $ do
                    let bad =
                            object
                                [ "query" .= ("q" :: Text)
                                , "state" .= ("found" :: Text)
                                , "next" .= ("{\"hits\": []}" :: Text)
                                ]
                    envelopeViolations bad `shouldSatisfy` (not . null)
                it "flags a package-hash-qualified name" $ do
                    let bad =
                            object
                                [ "query" .= ("q" :: Text)
                                , "state" .= ("found" :: Text)
                                , "next"
                                    .= ( "dataframe-2.0.0.0-8b3ca9f1e2d4b6a8c0f1\
                                         \e2d4b6a8c0f1:DataFrame" ::
                                            Text
                                       )
                                ]
                    envelopeViolations bad `shouldSatisfy` (not . null)

            describe "R3.8 a byte-identical repeat is a one-line reference" $
                it "the duplicate envelope is a fraction of the budget" $ do
                    let dup =
                            duplicateEnvelope
                                "col"
                                "call 12"
                                "3 hits; top: col :: Columnable a => Text -> Expr a"
                    envelopeChars dup `shouldSatisfy` (< 400)
                    textField "state" dup `shouldBe` "duplicate"

            describe "R1.7 the description is generated from the schema" $ do
                it "the tool description embeds the schema promise verbatim" $
                    discoverToolDescription
                        `shouldSatisfy` T.isInfixOf schemaPromise
                it "every promised hit field is delivered on every call shape" $ do
                    vs <- mapM runCat discoverables
                    let missing =
                            [ (k, hitText "name" h)
                            | v <- vs
                            , h <- hitsOf v
                            , k <- requiredHitKeys
                            , T.null (hitText k h)
                            ]
                    missing `shouldBe` []
                it "a hidden or absent-known hit always carries its cabal line" $ do
                    vs <- mapM runCat discoverables
                    let bad =
                            [ hitText "name" h
                            | v <- vs
                            , h <- hitsOf v
                            , hitText "install" h
                                `elem` ["hidden", "absent-known"]
                            , T.null (hitText "cabal" h)
                            ]
                    bad `shouldBe` []

            describe "the cheat-sheet advertises only delivered behaviour" $ do
                it "discoverGrammarBlock drops the never-empty promise" $ do
                    discoverGrammarBlock
                        `shouldSatisfy` (not . T.isInfixOf "never empty")
                    discoverGrammarBlock
                        `shouldSatisfy` (not . T.isInfixOf "Never returns nothing")
                it "discoverGrammarBlock promises the delivered provenance" $ do
                    discoverGrammarBlock
                        `shouldSatisfy` T.isInfixOf "install state"
                    discoverGrammarBlock
                        `shouldSatisfy` T.isInfixOf "what was consulted"
