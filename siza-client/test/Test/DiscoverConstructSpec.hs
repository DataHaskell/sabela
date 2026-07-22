{-# LANGUAGE OverloadedStrings #-}

{- | R4-T3(a): the constructibility facet (search-api.md section 7.1): a
"value of type T" question ranks producers of T nullary-first over the same
union-merge catalogue, keyed by the needs-a-value evidence class — never a
library name — with conservation, provenance and the bounded envelope intact.
-}
module Test.DiscoverConstructSpec (discoverConstructSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.Envelope (
    envelopeCharBudget,
    envelopeChars,
    envelopeViolations,
 )
import Test.CatalogueSim (
    SimWorld (..),
    producerPkgs,
    resultLike,
    runWorld,
    runWorldArgs,
 )
import Test.DiscoverFixtures (
    SynPkg (..),
    hitText,
    hitsOf,
    installNamesFileWith,
    stateOf,
    textField,
 )

discoverConstructSpec :: Spec
discoverConstructSpec = describe "the constructibility facet (section 7.1)" $ do
    wholeCatalogueSpec
    renameInvarianceSpec
    conservationSpec
    schemaSpec
    smokeSpec

-- | The default spellings; structure lives in 'producerPkgs'.
baseNames :: (Text, Text, Text)
baseNames = ("plume", "framing", "styling")

{- | Session sees the exposed producers; the hidden styling package is
universe-only (a hoogle-known, uninstalled producer of @Style@).
-}
worldOf :: (Text, Text, Text) -> SimWorld
worldOf names = SimWorld [p | p <- pkgs, not (spHidden p)] pkgs
  where
    pkgs = producerPkgs names

seedNames :: (Text, Text, Text) -> IO ()
seedNames (a, b, c) = installNamesFileWith [a, b, c]

-- | Every export of a world, as (name, type) pairs.
exportsOf :: SimWorld -> [(Text, Text)]
exportsOf w =
    [(n, ty) | p <- swUniverse w, (_, es) <- spModules p, (n, ty) <- es]

-- | Ground-truth producers of a goal type (result type is the goal).
producersOf :: SimWorld -> Text -> [Text]
producersOf w goal = [n | (n, ty) <- exportsOf w, resultLike goal ty]

-- | Nullary ground truth: the binding IS a value of the goal type.
nullaryOf :: SimWorld -> Text -> [Text]
nullaryOf w goal = [n | (n, ty) <- exportsOf w, T.strip ty == goal]

-- | The goal types the catalogue can construct (at least one producer).
goalTypes :: SimWorld -> [Text]
goalTypes w =
    nub
        [ goal
        | (_, ty) <- exportsOf w
        , let goal = T.strip (last (T.splitOn "->" ty))
        , not (null (producersOf w goal))
        ]

constructArgs :: Value
constructArgs = object [K.fromText "mode" .= ("construct" :: Text)]

-- Whole-catalogue property (R3.1/R3.2) --------------------------------------

wholeCatalogueSpec :: Spec
wholeCatalogueSpec = describe "producers rank nullary-first for every constructible type" $ do
    it "every type with a producer answers with a producer in the top 3" $ do
        seedNames baseNames
        let w = worldOf baseNames
        forM_ (goalTypes w) $ \goal -> do
            v <- runWorldArgs w goal constructArgs
            stateOf v `shouldBe` "found"
            let top3 = map (hitText "name") (take 3 (hitsOf v))
                good = [n | n <- top3, n `elem` producersOf w goal]
            (goal, good) `shouldSatisfy` (not . null . snd)
    it "a nullary producer or constructor outranks every partial match" $ do
        seedNames baseNames
        let w = worldOf baseNames
        forM_ (goalTypes w) $ \goal ->
            case nullaryOf w goal of
                [] -> pure ()
                nullary -> do
                    v <- runWorldArgs w goal constructArgs
                    let top = map (hitText "name") (take 1 (hitsOf v))
                    (goal, top) `shouldSatisfy` \(_, t) ->
                        not (null t) && all (`elem` nullary) t
    it "a nullary producer outranks the unary ones" $ do
        seedNames baseNames
        let w = worldOf baseNames
        v <- runWorldArgs w "Plot" constructArgs
        let names = map (hitText "name") (hitsOf v)
        names `shouldSatisfy` elem "defaultPlot"
        names `shouldSatisfy` elem "mkPlot"
        let rankOf n = length (takeWhile (/= n) names)
        rankOf "defaultPlot" `shouldSatisfy` (< rankOf "mkPlot")
        rankOf "defaultPlot" `shouldSatisfy` (< rankOf "plotLike")

-- Library-name invariance (R3.1) --------------------------------------------

renameInvarianceSpec :: Spec
renameInvarianceSpec = describe "library-name substitution never changes the decision" $
    it "granite/dataframe/acme-tables spellings rank identically" $ do
        let spellings =
                [ baseNames
                , ("granite", "dataframe", "acme-tables")
                , ("plotly-hs", "frames", "tables-kit")
                ]
        decisions <- mapM decisionOf spellings
        length (nub decisions) `shouldBe` 1
  where
    -- Conserved non-producer rows carry the spelled package names, so the
    -- comparison maps each spelling back through the substitution.
    canonical (a, b, c) n
        | n == a = "PKG-A"
        | n == b = "PKG-B"
        | n == c = "PKG-C"
        | otherwise = n
    decisionOf names = do
        seedNames names
        let w = worldOf names
        vs <-
            mapM (\g -> runWorldArgs w g constructArgs) (goalTypes (worldOf baseNames))
        pure [map (canonical names . hitText "name") (take 3 (hitsOf v)) | v <- vs]

-- Conservation + provenance + bounds (R3.3/R3.5, R7.7) ----------------------

conservationSpec :: Spec
conservationSpec = describe "no producer source's answer is dropped (R3.3/R3.5)" $ do
    it "session hole-fit and hoogle-only producers both surface, with provenance" $ do
        seedNames baseNames
        let w = worldOf baseNames
        v <- runWorldArgs w "Style" constructArgs
        -- mkStyle lives only in the universe-known styling package: hoogle
        -- evidence, uninstalled — its answer must not be dropped.
        let hits = hitsOf v
            styleHit = [h | h <- hits, hitText "name" h == "mkStyle"]
        styleHit `shouldSatisfy` (not . null)
        forM_ styleHit $ \h -> do
            hitText "install" h `shouldBe` "absent-known"
            hitText "cabal" h `shouldSatisfy` T.isInfixOf "build-depends:"
        vPlot <- runWorldArgs w "Plot" constructArgs
        forM_ (hitsOf vPlot) $ \h -> do
            hitText "install" h `shouldSatisfy` (not . T.null)
            hitText "origin" h `shouldSatisfy` (not . T.null)
    it "renders through the one bounded envelope" $ do
        seedNames baseNames
        let w = worldOf baseNames
        forM_ (goalTypes w) $ \goal -> do
            v <- runWorldArgs w goal constructArgs
            envelopeViolations v `shouldBe` []
            envelopeChars v `shouldSatisfy` (<= envelopeCharBudget)

-- The schema carries the facet (R1.7 by construction) -----------------------

schemaSpec :: Spec
schemaSpec = describe "one requestSchema carries the facet" $ do
    it "mode=construct is a validated mode, not a bad_request" $ do
        seedNames baseNames
        v <- runWorldArgs (worldOf baseNames) "Plot" constructArgs
        stateOf v `shouldNotBe` "bad_request"
    it "an unknown mode is still rejected with the mode vocabulary" $ do
        v <-
            runWorldArgs
                (worldOf baseNames)
                "Plot"
                (object [K.fromText "mode" .= ("banana" :: Text)])
        stateOf v `shouldBe` "bad_request"
        textField "reason" v `shouldSatisfy` T.isInfixOf "construct"
    it "a 'value of type T' prose query triggers the facet without the knob" $ do
        seedNames baseNames
        let w = worldOf baseNames
        v <- runWorld w "value of type Plot"
        stateOf v `shouldBe` "found"
        map (hitText "name") (take 3 (hitsOf v))
            `shouldSatisfy` elem "defaultPlot"

-- The barChart smoke (last-mile counterfactual) -----------------------------

smokeSpec :: Spec
smokeSpec = describe "barChart smoke: the 14-field record's last mile"
    $ it
        "holding bars :: [(Text,Double)] -> Plot -> Text, 'value of type Plot' answers top-3"
    $ do
        seedNames baseNames
        let w = worldOf baseNames
        v <- runWorld w "value of type Plot"
        let top3 = take 3 (hitsOf v)
        map (hitText "name") top3 `shouldSatisfy` elem "defaultPlot"
        -- 'default Plot' (the vocabulary the model actually used) also lands.
        v2 <- runWorld w "default Plot"
        map (hitText "name") (take 3 (hitsOf v2))
            `shouldSatisfy` elem "defaultPlot"
