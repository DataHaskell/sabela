{-# LANGUAGE OverloadedStrings #-}

{- | Intention specs for the @discover@ tool plumbing: routing by query shape,
the qualified-name fallback, blank-payload detection, catalogue drift-freedom,
and the honest tool description (R1.7: advertised behaviour = actual). The
merge itself is covered by Test.DiscoverCatalogueSpec / DiscoverInvariantSpec.
-}
module Test.DiscoverToolClientSpec (discoverToolSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.DiscoverTool (
    blankPayload,
    discoverArgs,
    discoverPlan,
    discoverQuery,
    discoverToolDescription,
    queryVariants,
    runDiscoverTool,
 )
import Siza.Agent.Tools (catalogueWith, unknownToolMsg)
import Test.DiscoverFixtures (stateOf, textField)
import Test.Hspec

-- | The function names offered by a model-facing catalogue value.
catalogueNames :: [Value] -> [Text]
catalogueNames cat =
    [ n
    | Object o <- cat
    , Just (Object f) <- [KM.lookup "function" o]
    , Just (String n) <- [KM.lookup "name" f]
    ]

discoverToolSpec :: Spec
discoverToolSpec = describe "discover tool (plumbing)" $ do
    describe "discoverPlan — route by query shape, fall through on miss" $ do
        it "routes a type-shaped query type-first" $
            discoverPlan True "(Char -> Bool) -> Parser String"
                `shouldBe` [FindByType, FindFunction, SearchCapability]
        it "routes a name :: type query type-first" $
            take 1 (discoverPlan True "runConduit :: ConduitT () Void m r -> m r")
                `shouldBe` [FindByType]
        it "routes a bare name name-first" $
            discoverPlan True "divvy" `shouldBe` [FindFunction, SearchCapability]
        it "routes a module name name-first" $
            take 1 (discoverPlan True "Granite.Svg") `shouldBe` [FindFunction]
        it "routes prose capability-first" $
            discoverPlan True "parse digits from a string"
                `shouldBe` [SearchCapability, FindFunction]
        it "the lever never removes the lexical capability channel (section 2)" $
            discoverPlan False "parse digits from a string"
                `shouldBe` [SearchCapability, FindFunction]

    describe "queryVariants — a qualified name retries as the bare name" $ do
        it "an alias-qualified value name adds its bare-name fallback" $
            queryVariants "D.col" `shouldBe` ["D.col", "col"]
        it "a deeper qualifier still falls back to the last component" $
            queryVariants "Data.List.foldl'" `shouldBe` ["Data.List.foldl'", "foldl'"]
        it "a module-shaped query (Upper last component) has no fallback" $
            queryVariants "Data.List.Extra" `shouldBe` ["Data.List.Extra"]
        it "a bare name has no fallback" $
            queryVariants "divvy" `shouldBe` ["divvy"]
        it "prose is never split" $
            queryVariants "parse digits from a string"
                `shouldBe` ["parse digits from a string"]

    describe "runDiscoverTool — one bounded envelope on every outcome" $ do
        it "a total miss is a scoped not_found envelope, never empty" $ do
            let call _ _ =
                    pure (Right (ToolOk (object ["matches" .= ([] :: [Value])])))
            ToolOk v <- runDiscoverTool False call "frobwizzle"
            stateOf v `shouldBe` "not_found"
        it "a blank query is a bad_request, not a miss" $ do
            let call _ _ = pure (Left "must not be called")
            ToolOk v <- runDiscoverTool False call "   "
            stateOf v `shouldBe` "bad_request"
        it "an unreachable backend reads unavailable, not absent" $ do
            let call _ _ = pure (Left "connect failure")
            ToolOk v <- runDiscoverTool False call "zzz"
            stateOf v `shouldBe` "not_found"
            textField "next" v `shouldSatisfy` T.isInfixOf "unavailable"

    describe "discoverArgs — each backend keeps its own arg key" $ do
        it "find_by_type takes goal" $
            discoverArgs FindByType "[Int] -> Int"
                `shouldBe` object ["goal" .= ("[Int] -> Int" :: Text)]
        it "find_function takes query" $
            discoverArgs FindFunction "divvy"
                `shouldBe` object ["query" .= ("divvy" :: Text)]
        it "search_capability takes query" $
            discoverArgs SearchCapability "edit distance"
                `shouldBe` object ["query" .= ("edit distance" :: Text)]

    describe "discoverQuery — accept the weak-model name-baked argument" $ do
        it "reads the query argument" $
            discoverQuery "discover" (object ["query" .= ("divvy" :: Text)])
                `shouldBe` Just "divvy"
        it "recovers a query baked into the tool name" $
            discoverQuery "discover divvy" (object [])
                `shouldBe` Just "divvy"
        it "is Nothing for other tools" $
            discoverQuery "find_function" (object ["query" .= ("x" :: Text)])
                `shouldBe` Nothing

    describe "blankPayload — an echo-only payload is a miss" $ do
        it "empty matches with a query echo is blank" $
            blankPayload
                (object ["query" .= ("x" :: Text), "matches" .= ([] :: [Value])])
                `shouldBe` True
        it "a status card is substance, not a blank" $
            blankPayload
                ( object
                    [ "module" .= ("Data.Conduit" :: Text)
                    , "status" .= ("hidden-package" :: Text)
                    , "package" .= ("conduit" :: Text)
                    ]
                )
                `shouldBe` False
        it "an error card is still blank (fall through)" $
            blankPayload
                ( object
                    [ "module" .= ("Zzz" :: Text)
                    , "status" .= ("error" :: Text)
                    , "message" .= ("parse error" :: Text)
                    ]
                )
                `shouldBe` True
        it "a non-empty hits array is a hit" $
            blankPayload
                ( object
                    [ "query" .= ("q" :: Text)
                    , "hits" .= [object ["name" .= ("takeWhileP" :: Text)]]
                    ]
                )
                `shouldBe` False

    describe "tool description — advertised = actual (R1.7)" $ do
        it "names the four install states the envelope actually returns" $ do
            let d = discoverToolDescription
            mapM_
                (\s -> d `shouldSatisfy` T.isInfixOf s)
                ["installed", "hidden", "absent-known", "absent-unknown"]
        it "no longer promises never-empty results or unconditional cabal lines" $ do
            let d = discoverToolDescription
            d `shouldSatisfy` (not . T.isInfixOf "Never returns nothing")
            d `shouldSatisfy` (not . T.isInfixOf "never empty")

    describe "catalogue — one spec list, structurally drift-free" $ do
        it "offers discover and not the tools it subsumes" $ do
            let ns = catalogueNames (catalogueWith True)
            ns `shouldContain` ["discover"]
            ns `shouldNotContain` ["find_function"]
            ns `shouldNotContain` ["find_by_type"]
            ns `shouldNotContain` ["find_example_cell"]
            ns `shouldNotContain` ["search_capability"]
        it "unknownToolMsg names every offered tool and no phantom" $ do
            let msg = unknownToolMsg "bogus"
            mapM_
                (\n -> msg `shouldSatisfy` T.isInfixOf n)
                (catalogueNames (catalogueWith True))
            msg `shouldSatisfy` (not . T.isInfixOf "find_package")
            msg `shouldSatisfy` (not . T.isInfixOf "find_function")
            msg `shouldSatisfy` (not . T.isInfixOf "search_capability")
