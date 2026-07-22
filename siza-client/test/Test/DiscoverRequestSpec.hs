{-# LANGUAGE OverloadedStrings #-}

{- | The discover request schema (R1.7, R2.7, R2.8): the offered catalogue
entry and the validator derive from ONE schema, every advertised argument
observably changes the result or is rejected (no dead knobs), scope filters
are honoured or disclosed, and bounds are exact at 0, 1, oversized and
malformed. Plus the shipped-knob alignment of the history advice.
-}
module Test.DiscoverRequestSpec (discoverRequestSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.Advice (factsClause)
import Siza.Agent.Discover.Request (
    defaultLimit,
    requestArgNames,
    requestKey,
    requestRequired,
 )
import Siza.Agent.Tools (catalogueWith)
import Test.DiscoverFixtures (
    field,
    hitText,
    hitsOf,
    installNamesFile,
    runCatArgs,
    stateOf,
    textField,
 )

-- | The discover entry's schema in the model-facing catalogue.
discoverSchema :: Maybe Value
discoverSchema =
    case [ f
         | Object o <- catalogueWith True
         , Just (Object f) <- [KM.lookup "function" o]
         , Just (String "discover") <- [KM.lookup "name" f]
         ] of
        (f : _) -> KM.lookup "parameters" f
        [] -> Nothing

intOf :: Text -> Value -> Int
intOf k v = case field k v of
    Just (Number n) -> round n
    _ -> -1

args :: [(Text, Value)] -> Value
args kvs = object [(K.fromText k, v) | (k, v) <- kvs]

discoverRequestSpec :: Spec
discoverRequestSpec = describe "discover request schema (R1.7/R2.7/R2.8)" $ do
    describe "one schema: offered catalogue entry == validated surface" $ do
        it "offers exactly the schema's argument names" $ do
            let names = case discoverSchema of
                    Just (Object s)
                        | Just (Object ps) <- KM.lookup "properties" s ->
                            map K.toText (KM.keys ps)
                    _ -> []
            names `shouldMatchList` requestArgNames
        it "requires exactly the schema's required list" $ do
            let req = case discoverSchema of
                    Just (Object s) -> maybe [] textList (KM.lookup "required" s)
                    _ -> []
            req `shouldBe` requestRequired
        it "ships module, package and limit (the narrow advice is followable)" $
            forM_ (["module", "package", "limit"] :: [Text]) $ \k ->
                requestArgNames `shouldContain` [k]

    describe "R1.7 no dead knobs: every argument observably changes the result" $ do
        it "module scopes an ambiguous name to one module" $ do
            installNamesFile
            unfiltered <- runCatArgs "lull" (args [])
            scoped <- runCatArgs "lull" (args [("module", String "Stratus.Air")])
            map (hitText "module") (hitsOf unfiltered)
                `shouldSatisfy` any (/= "Stratus.Air")
            hitsOf scoped `shouldSatisfy` (not . null)
            map (hitText "module") (hitsOf scoped)
                `shouldSatisfy` all (== "Stratus.Air")
        it "package scopes an ambiguous name to one package" $ do
            installNamesFile
            scoped <- runCatArgs "lull" (args [("package", String "stratus")])
            hitsOf scoped `shouldSatisfy` (not . null)
            map (hitText "package") (hitsOf scoped)
                `shouldSatisfy` all (== "stratus")
        it "limit=1 shows exactly one hit and reconciles the counts" $ do
            installNamesFile
            v <- runCatArgs "lull" (args [("limit", Number 1)])
            length (hitsOf v) `shouldBe` 1
            intOf "shown" v `shouldBe` 1
            (intOf "shown" v + intOf "omitted" v) `shouldBe` intOf "total" v
            dflt <- runCatArgs "lull" (args [])
            intOf "total" dflt `shouldSatisfy` (> 1)

    describe "R2.7 scope filters are honoured or disclosed, never silent" $ do
        it "a filter matching nothing says so and names the unfiltered count" $ do
            installNamesFile
            v <- runCatArgs "gust" (args [("module", String "Nimbus.Sky")])
            hitsOf v `shouldBe` []
            textField "narrow" v `shouldSatisfy` T.isInfixOf "matched none"
            textField "narrow" v `shouldSatisfy` T.isInfixOf "Nimbus.Sky"
        it "a filter changing nothing says so explicitly" $ do
            installNamesFile
            v <- runCatArgs "stratify" (args [("module", String "Stratus.Air")])
            hitsOf v `shouldSatisfy` (not . null)
            textField "narrow" v `shouldSatisfy` T.isInfixOf "changed nothing"
        it "a narrowing filter disclosed kept-of-total" $ do
            installNamesFile
            v <- runCatArgs "lull" (args [("module", String "Stratus.Air")])
            textField "narrow" v `shouldSatisfy` T.isInfixOf "kept"

    describe "R2.8 limit bounds are exhaustive: 0, 1, oversized, malformed" $ do
        it "limit=0 is rejected as a bad_request naming the field" $ do
            v <- runCatArgs "lull" (args [("limit", Number 0)])
            stateOf v `shouldBe` "bad_request"
            textField "reason" v `shouldSatisfy` T.isInfixOf "limit"
        it "a limit larger than the result set shows everything, omitted 0" $ do
            installNamesFile
            v <- runCatArgs "lull" (args [("limit", Number 25)])
            intOf "shown" v `shouldBe` intOf "total" v
            intOf "omitted" v `shouldBe` 0
        it "malformed limits are rejected (string, fractional, negative, over-cap)"
            $ forM_
                [ String "abc"
                , Number 3.5
                , Number (-3)
                , Number 26
                , Bool True
                ]
            $ \bad -> do
                v <- runCatArgs "lull" (args [("limit", bad)])
                (bad, stateOf v) `shouldBe` (bad, "bad_request")
        it "malformed scope filters are rejected with the field named" $ do
            v <- runCatArgs "lull" (args [("module", Number 3)])
            stateOf v `shouldBe` "bad_request"
            textField "reason" v `shouldSatisfy` T.isInfixOf "module"
        it
            "blank module/package/mode mean 'no filter': a literal caller \
            \fills every schema property (run-20260720 barChart)"
            $ do
                unfiltered <- runCatArgs "lull" (args [])
                blanks <-
                    runCatArgs
                        "lull"
                        ( args
                            [ ("module", String "")
                            , ("package", String " ")
                            , ("mode", String "")
                            ]
                        )
                stateOf blanks `shouldBe` stateOf unfiltered
                (hitText "name" <$> hitsOf blanks)
                    `shouldBe` (hitText "name" <$> hitsOf unfiltered)

    describe "R2.6 query-less scoped inventory (the miss-guidance target)" $ do
        it "module scope with a blank query answers the scoped module card" $ do
            installNamesFile
            v <-
                runCatArgs
                    ""
                    ( args
                        [ ("mode", String "inventory")
                        , ("module", String "Zephyr.Core")
                        ]
                    )
            stateOf v `shouldNotBe` "bad_request"
            hitsOf v `shouldSatisfy` (not . null)
            map (hitText "package") (hitsOf v)
                `shouldSatisfy` elem "zephyr"
        it "package scope with the query key ABSENT answers the card" $ do
            installNamesFile
            v <-
                runCatArgs
                    ""
                    ( args
                        [ ("mode", String "inventory")
                        , ("package", String "zephyr")
                        ]
                    )
            stateOf v `shouldNotBe` "bad_request"
            hitsOf v `shouldSatisfy` (not . null)
        it "blank everything is still rejected as malformed" $ do
            v <- runCatArgs "" (args [])
            stateOf v `shouldBe` "bad_request"
        it "blank query in SEARCH mode is still rejected, scope or not" $ do
            v <- runCatArgs "" (args [("module", String "Zephyr.Core")])
            stateOf v `shouldBe` "bad_request"
        it "inventory without a scope still requires a query" $ do
            v <- runCatArgs "" (args [("mode", String "inventory")])
            stateOf v `shouldBe` "bad_request"
        it "the advertised schema matches: query is not unconditionally required" $ do
            requestRequired `shouldBe` []
            let qDesc = case discoverSchema of
                    Just (Object s)
                        | Just (Object ps) <- KM.lookup "properties" s
                        , Just (Object q) <- KM.lookup "query" ps
                        , Just (String d) <- KM.lookup "description" q ->
                            d
                    _ -> ""
            T.toLower qDesc `shouldSatisfy` T.isInfixOf "inventory"

    describe "the ledger key keeps the knobs observable (R3.8 x R1.7)" $ do
        it "a re-scoped repeat is a different key" $ do
            requestKey "lull" (args [])
                `shouldNotBe` requestKey "lull" (args [("module", String "Stratus.Air")])
            requestKey "lull" (args [])
                `shouldNotBe` requestKey "lull" (args [("limit", Number 1)])
        it "an identical call is the same key" $
            requestKey "lull" (args [("limit", Number 1)])
                `shouldBe` requestKey "lull" (args [("limit", Number 1)])
        it "the default limit adds no decoration" $
            requestKey "lull" (args [("limit", Number (fromIntegral defaultLimit))])
                `shouldBe` requestKey "lull" (args [])

    describe "history advice names only shipped knobs (carryover #4, R5.6)" $ do
        it "factsClause points at the now-shipped inventory mode" $ do
            let advice = T.toLower (factsClause [])
            advice `shouldSatisfy` T.isInfixOf "inventory"
        it "every knob factsClause names is in the shipped schema" $ do
            let advice = factsClause []
                named =
                    [ k
                    | k <- ["module=", "package=", "limit=", "mode="]
                    , k `T.isInfixOf` advice
                    ]
            forM_ named $ \k ->
                requestArgNames `shouldContain` [T.dropEnd 1 k]

textList :: Value -> [Text]
textList (Array a) = [t | String t <- foldr (:) [] a]
textList _ = []
