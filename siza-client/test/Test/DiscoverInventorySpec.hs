{-# LANGUAGE OverloadedStrings #-}

module Test.DiscoverInventorySpec (discoverInventorySpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Siza.Agent.Discover.Envelope (
    boundEnvelope,
    envelopeCharBudget,
    envelopeChars,
 )
import Siza.Agent.Discover.Inventory (inventoryEnvelope)
import Siza.Agent.Discover.Request (requestKey)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    Interpreted (..),
    MatchKind (..),
    NotebookEnv (..),
    SourceAnswer (..),
    emptyScope,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (
    SynPkg (..),
    field,
    hitText,
    hitsOf,
    installNamesFileWith,
    runCatArgs,
    runCatArgsIn,
    stateOf,
    synHackageNames,
    synHoogle,
    textField,
 )
import Test.DiscoverGen (genModulePair, genPkgPair)

args :: [(Text, Value)] -> Value
args kvs = object [(K.fromText k, v) | (k, v) <- kvs]

intOf :: Text -> Value -> Int
intOf k v = case field k v of
    Just (Number n) -> round n
    _ -> -1

emptyEnv :: NotebookEnv
emptyEnv = seededBuiltins (NotebookEnv [] [] [] [] [] [])

topicInterp :: Text -> Interpreted
topicInterp q = Interpreted q q Nothing "name" "" []

genStates :: Int -> [(Text, InstallState)]
genStates n =
    [ (T.pack ("pkg" <> show i), cycle3 (i `mod` 3))
    | i <- [0 .. n - 1]
    ]
  where
    cycle3 0 = InstInstalled
    cycle3 1 = InstHidden
    cycle3 _ = InstAbsentKnown

{- | The inventory rows one answer yields, over the modules its hits and its
package map named.
-}
rowsFor :: Text -> [DHit] -> [(Text, [Text])] -> [Value]
rowsFor topic hs mods =
    hitsOf
        ( boundEnvelope
            ( inventoryEnvelope
                emptyEnv
                (topicInterp topic)
                emptyScope
                8
                [(okAnswer "hoogle" hs){saPkgModules = mods}]
                (HackageInfo True [] [] [])
                []
            )
        )

-- | A hit that names its package and no module at all.
moduleless :: Text -> DHit
moduleless p = (genHit (p, InstHidden)){dhModule = ""}

genHit :: (Text, InstallState) -> DHit
genHit (p, st) =
    DHit
        (p <> "Fn")
        ("Int -> " <> p)
        (moduleOf p)
        p
        (if st == InstInstalled then "1.0." <> T.drop 3 p else "")
        st
        MkExact
        (if st == InstInstalled then "session" else "hoogle")
        Nothing
        Nothing
        Nothing
        Nothing
  where
    moduleOf t = "M." <> T.toTitle t

genInventory :: Int -> Int -> Value
genInventory n limit =
    boundEnvelope
        ( inventoryEnvelope
            emptyEnv
            (topicInterp "pkg")
            emptyScope
            limit
            [okAnswer "hoogle" (map genHit states)]
            (HackageInfo True [p | (p, InstAbsentKnown) <- states] [] [])
            []
        )
  where
    states = genStates n

httpCat :: [SynPkg]
httpCat =
    synHoogle
        ++ [ SynPkg
                "http-client"
                "0.7.19"
                True
                [
                    ( "Network.HTTP.Client"
                    , [("httpLbs", "Request -> Manager -> IO (Response ByteString)")]
                    )
                ]
           ]

discoverInventorySpec :: Spec
discoverInventorySpec = describe "inventory mode (R3-T3)" $ do
    describe "R1.7 argument grid: every argument changes the result or rejects" $ do
        it "query changes the answer" $ do
            installNamesFileWith synHackageNames
            a <- runCatArgs "gust" (args [])
            b <- runCatArgs "lull" (args [])
            encode a `shouldNotBe` encode b
        it "module filter changes or discloses" $ do
            installNamesFileWith synHackageNames
            a <- runCatArgs "lull" (args [])
            b <- runCatArgs "lull" (args [("module", String "Stratus.Air")])
            (encode a /= encode b || not (T.null (textField "narrow" b)))
                `shouldBe` True
        it "package filter changes or discloses" $ do
            installNamesFileWith synHackageNames
            a <- runCatArgs "lull" (args [])
            b <- runCatArgs "lull" (args [("package", String "stratus")])
            (encode a /= encode b || not (T.null (textField "narrow" b)))
                `shouldBe` True
        it "limit is exact at 1 and rejected at 0 and oversized" $ do
            installNamesFileWith synHackageNames
            one <- runCatArgs "lull" (args [("limit", Number 1)])
            length (hitsOf one) `shouldBe` 1
            forM_ [Number 0, Number 26] $ \bad -> do
                v <- runCatArgs "lull" (args [("limit", bad)])
                stateOf v `shouldBe` "bad_request"
                textField "reason" v `shouldSatisfy` T.isInfixOf "limit"
        it "mode=inventory changes the answer; a bogus mode is rejected" $ do
            installNamesFileWith synHackageNames
            search <- runCatArgs "zephyr" (args [])
            inv <- runCatArgs "zephyr" (args [("mode", String "inventory")])
            encode search `shouldNotBe` encode inv
            bad <- runCatArgs "zephyr" (args [("mode", String "browse")])
            stateOf bad `shouldBe` "bad_request"
            textField "reason" bad `shouldSatisfy` T.isInfixOf "mode"
        it "mode is observable in the ledger key" $
            requestKey "zephyr" (args [("mode", String "inventory")])
                `shouldNotBe` requestKey "zephyr" (args [])

    describe "inventory boundedness over generated catalogues (R3.4/R3.9)" $
        forM_ [1, 5, 20, 50, 100] $ \n ->
            it ("stays bounded and reconciled at " <> show n <> " packages") $
                forM_ [1, 8, 25] $ \limit -> do
                    let v = genInventory n limit
                    envelopeChars v `shouldSatisfy` (<= envelopeCharBudget)
                    (intOf "shown" v + intOf "omitted" v)
                        `shouldBe` intOf "total" v
                    length (hitsOf v) `shouldSatisfy` (<= limit)

    describe "three-state install provenance, never conflated (R1.3/R3.5)" $ do
        it "every row reports its package's true state, cabal on hidden/absent" $ do
            let n = 30
                v = genInventory n 25
                states = genStates n
            hitsOf v `shouldSatisfy` (not . null)
            forM_ (hitsOf v) $ \h -> do
                let p = hitText "package" h
                    expected = lookup p states
                case expected of
                    Just InstInstalled ->
                        hitText "install" h `shouldBe` "installed"
                    Just InstHidden -> do
                        hitText "install" h `shouldBe` "installed-not-loaded"
                        hitText "cabal" h
                            `shouldSatisfy` T.isInfixOf "build-depends"
                    Just InstAbsentKnown -> do
                        hitText "install" h `shouldBe` "absent-known"
                        hitText "cabal" h
                            `shouldSatisfy` T.isInfixOf "build-depends"
                    _ -> expectationFailure ("unexpected row: " <> show h)
        it "each package appears in exactly one row (no state conflation)" $ do
            let v = genInventory 30 25
                pkgs = map (hitText "package") (hitsOf v)
            length pkgs `shouldBe` length (foldr dedup [] pkgs)

    describe "the M6 counterfactual: hidden http answered in ONE call" $
        it "topic 'http' yields the hidden package with its cabal line" $ do
            installNamesFileWith ("http-client" : synHackageNames)
            v <-
                runCatArgsIn
                    httpCat
                    "http"
                    (args [("mode", String "inventory")])
            stateOf v `shouldBe` "found"
            let rows =
                    [ h
                    | h <- hitsOf v
                    , hitText "package" h == "http-client"
                    ]
            rows `shouldSatisfy` (not . null)
            forM_ (take 1 rows) $ \h -> do
                hitText "install" h `shouldBe` "installed-not-loaded"
                hitText "cabal" h
                    `shouldBe` "-- cabal: build-depends: http-client"

    describe "a package row's module is one its sources named (W5 item 3)" $ do
        prop "a package no source gave a module for gets no row" $
            forAll (fst <$> genPkgPair) $ \p ->
                let rows = rowsFor p [moduleless p] []
                 in counterexample (show rows) (map (hitText "package") rows === [])
        prop "a row takes the module its package answer computed" $
            forAll ((,) . fst <$> genPkgPair <*> (fst <$> genModulePair)) $
                \(p, m) ->
                    let rows = rowsFor p [moduleless p] [(p, [m])]
                     in counterexample (show rows) $
                            [hitText "module" h | h <- rows] === [m]
        prop "every emitted row states a module some source named" $
            forAll ((,) <$> genPkgPair <*> (fst <$> genModulePair)) $
                \((p, q), m) ->
                    let hs = [moduleless p, genHit (q, InstInstalled)]
                        rows = rowsFor p hs [(q, [m])]
                        named = m : [dhModule h | h <- hs, not (T.null (dhModule h))]
                        stated = map (hitText "module") rows
                     in counterexample (show (stated, named)) $
                            conjoin [property (s `elem` named) | s <- stated]

    describe "a package row claims exact only on a whole-name match" $ do
        let pkgRows q pkgs =
                boundEnvelope
                    ( inventoryEnvelope
                        emptyEnv
                        (topicInterp q)
                        emptyScope
                        8
                        [ okAnswer
                            "hoogle"
                            [ (genHit (p, InstAbsentKnown)){dhPackage = p}
                            | p <- pkgs
                            ]
                        ]
                        (HackageInfo True pkgs [] [])
                        []
                    )
            kindsOf v = [hitText "matchKind" h | h <- hitsOf v]
        it "a token merely inside the package name is not exact" $
            kindsOf (pkgRows "line" ["Facebook-Password-Hacker-Online-Latest-Version"])
                `shouldSatisfy` notElem "exact"
        it "a package whose name IS the query stays exact" $
            kindsOf (pkgRows "conduit" ["conduit"])
                `shouldSatisfy` elem "exact"

dedup :: (Eq a) => a -> [a] -> [a]
dedup x acc = if x `elem` acc then acc else x : acc
