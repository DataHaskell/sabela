{-# LANGUAGE OverloadedStrings #-}

{- | R3-T3: @mode:"inventory"@ (search-api.md section 3, M6), the within-
stratum public-API demotion (R3.1-R3.3, R3.7), session-hit version enrichment
(carryover 6), and the R1.7 argument grid: every advertised discover argument
observably changes the result or is rejected.
-}
module Test.DiscoverInventorySpec (discoverInventorySpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.Envelope (
    boundEnvelope,
    envelopeCharBudget,
    envelopeChars,
 )
import Siza.Agent.Discover.Inventory (inventoryEnvelope)
import Siza.Agent.Discover.Rank (rankKey, stratum)
import Siza.Agent.Discover.Request (requestKey)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    Interpreted (..),
    MatchKind (..),
    NotebookEnv (..),
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

-- | A generated catalogue: n packages cycling the three honest states.
genStates :: Int -> [(Text, InstallState)]
genStates n =
    [ (T.pack ("pkg" <> show i), cycle3 (i `mod` 3))
    | i <- [0 .. n - 1]
    ]
  where
    cycle3 0 = InstInstalled
    cycle3 1 = InstHidden
    cycle3 _ = InstAbsentKnown

-- | One catalogue hit per generated package, in that package's true state.
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
            (HackageInfo True [p | (p, InstAbsentKnown) <- states])
            []
        )
  where
    states = genStates n

-- | The synthetic hidden http-client catalogue of the M6 counterfactual.
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

subHit :: Text -> DHit
subHit m =
    DHit
        "gustish"
        "Int -> Int"
        m
        "zephyr"
        "1.0"
        InstInstalled
        MkSubstring
        "session"
        Nothing
        Nothing

discoverInventorySpec :: Spec
discoverInventorySpec = describe "inventory mode + demotion (R3-T3)" $ do
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
                        hitText "install" h `shouldBe` "hidden"
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
                hitText "install" h `shouldBe` "hidden"
                hitText "cabal" h
                    `shouldBe` "-- cabal: build-depends: http-client"

    describe "within-stratum internal demotion (R3.1-R3.3, R3.7)" $ do
        it "a public module outranks an internal one inside its stratum" $ do
            let public = subHit "Zephyr.Core.Extra"
                internal = subHit "Zephyr.Internal"
                env = emptyEnv
                interp = topicInterp "gust"
            stratum env interp public `shouldBe` stratum env interp internal
            rankKey env interp public
                `shouldSatisfy` (< rankKey env interp internal)
        it "an exact hit still outranks any public substring hit" $ do
            let exactNoise =
                    (subHit "Zephyr.Internal")
                        { dhName = "gust"
                        , dhKind = MkExact
                        }
                env = emptyEnv
                interp = topicInterp "gust"
            rankKey env interp exactNoise
                `shouldSatisfy` (< rankKey env interp (subHit "Zephyr.Core"))
        it "demoted internal hits are summarised, never dropped" $ do
            installNamesFileWith synHackageNames
            full <- runCatArgs "gust" (args [])
            v <- runCatArgs "gust" (args [("limit", Number 1)])
            intOf "total" v `shouldBe` intOf "total" full
            textField "narrow" v `shouldSatisfy` T.isInfixOf "internal"
        it "ranking is deterministic (R3.7)" $ do
            installNamesFileWith synHackageNames
            a <- runCatArgs "gust" (args [])
            b <- runCatArgs "gust" (args [])
            encode a `shouldBe` encode b

    describe "session-hit version enrichment (carryover 6)" $ do
        it "a session hit gains the version another source holds" $ do
            installNamesFileWith synHackageNames
            v <- runCatArgs "gust" (args [])
            let top = take 1 (hitsOf v)
            top `shouldSatisfy` (not . null)
            forM_ top $ \h -> hitText "version" h `shouldBe` "1.2.0"
        it "a session hit with no known version never leaks raw provenance" $ do
            installNamesFileWith synHackageNames
            v <- runCatArgs "gust" (args [])
            let mine =
                    [h | h <- hitsOf v, hitText "name" h == "puffLoop"]
            mine `shouldSatisfy` (not . null)
            forM_ (take 1 mine) $ \h ->
                hitText "version" h
                    `shouldSatisfy` (not . T.isInfixOf "package env")

    describe "miss guidance points at inventory (R5.3)" $
        it "a nonsense miss names mode=inventory" $ do
            installNamesFileWith synHackageNames
            v <- runCatArgs "qqzzyx" (args [])
            stateOf v `shouldBe` "not_found"
            textField "next" v `shouldSatisfy` T.isInfixOf "inventory"

dedup :: (Eq a) => a -> [a] -> [a]
dedup x acc = if x `elem` acc then acc else x : acc
