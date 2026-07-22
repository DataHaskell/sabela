{-# LANGUAGE OverloadedStrings #-}

{- | The honest request boundary (R1.7, the M8 laundering class): over ALL
catalogue tool names x wrapper shapes (flat, {input:{...}}, {arguments:{...}},
doubly-nested, junk keys) an offered name NEVER routes to "unknown tool"; a
still-wrong shape yields one shape-correcting bad-args hint naming the
wrapper; a genuinely unknown name still says so (no over-acceptance).
-}
module Test.ToolRouteSpec (toolRouteSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.ToolRoute (
    Route (..),
    normalizeToolCall,
    routeCall,
 )
import Siza.Agent.Tools (offeredNames)

-- | A representative flat argument object accepted by any catalogue tool.
flatArgs :: Value
flatArgs =
    object
        [ "query" .= ("granite" :: Text)
        , "cell_id" .= (1 :: Int)
        , "source" .= ("x = 1" :: Text)
        , "new_source" .= ("x = 2" :: Text)
        , "pattern" .= ("x" :: Text)
        , "expr" .= ("x" :: Text)
        , "code" .= ("x" :: Text)
        ]

wrapperShapes :: [(Text, Value -> Value)]
wrapperShapes =
    [ ("flat", id)
    , ("input-wrapped", \a -> object ["input" .= a])
    , ("arguments-wrapped", \a -> object ["arguments" .= a])
    , ("doubly-nested", \a -> object ["input" .= object ["input" .= a]])
    , ("junk-keys", withJunk)
    ]
  where
    withJunk (Object o) =
        Object (KM.insert "junk_key" (String "noise") o)
    withJunk v = v

isUnknown :: Route -> Bool
isUnknown (RouteUnknown _) = True
isUnknown _ = False

isBadArgs :: Route -> Bool
isBadArgs (RouteBadArgs _) = True
isBadArgs _ = False

toolRouteSpec :: Spec
toolRouteSpec = describe "tool-call routing boundary (R1.7/M8 class)" $ do
    describe "an offered name never yields 'unknown tool'" $
        forM_ wrapperShapes $ \(shapeLabel, wrap) ->
            it (T.unpack ("wrapper shape: " <> shapeLabel)) $
                forM_ offeredNames $ \name -> do
                    let route = routeCall (ToolCall name (wrap flatArgs))
                    (name, isUnknown route) `shouldBe` (name, False)

    describe "the jsonSum false denial: wrapped discover args" $ do
        it "{input:{query}} routes to discover with the query" $ do
            let call =
                    ToolCall
                        "discover"
                        (object ["input" .= object ["query" .= ("granite" :: Text)]])
            routeCall call
                `shouldBe` RouteDiscover "granite" (object ["query" .= ("granite" :: Text)])
        it "{arguments:{query}} routes to discover with the query" $ do
            let call =
                    ToolCall
                        "discover"
                        (object ["arguments" .= object ["query" .= ("granite" :: Text)]])
            routeCall call
                `shouldBe` RouteDiscover "granite" (object ["query" .= ("granite" :: Text)])
        it "a query-less discover call still routes to discover, not unknown" $ do
            let route = routeCall (ToolCall "discover" (object []))
            isUnknown route `shouldBe` False
            route `shouldBe` RouteDiscover "" (object [])

    describe "a still-wrong shape yields ONE hint naming the wrapper" $ do
        it "a non-object 'input' wrapper is a bad-args hint naming input" $ do
            let route =
                    routeCall
                        (ToolCall "discover" (object ["input" .= ("granite" :: Text)]))
            isBadArgs route `shouldBe` True
            case route of
                RouteBadArgs hint -> do
                    hint `shouldSatisfy` T.isInfixOf "input"
                    hint `shouldSatisfy` T.isInfixOf "discover"
                _ -> pure ()
        it "a non-object 'arguments' wrapper names arguments" $ do
            let route =
                    routeCall
                        (ToolCall "list_cells" (object ["arguments" .= (5 :: Int)]))
            case route of
                RouteBadArgs hint -> hint `shouldSatisfy` T.isInfixOf "arguments"
                other -> expectationFailure ("expected RouteBadArgs, got " <> show other)

    describe "no over-acceptance: a genuinely unknown name still says so" $
        forM_ (["frobnicate", "run_sql", ""] :: [Text]) $ \name ->
            it (T.unpack ("unknown name: '" <> name <> "'")) $
                isUnknown (routeCall (ToolCall name flatArgs)) `shouldBe` True

    describe "normalizeToolCall feeds the guards normalised keys" $ do
        it "unwraps a single-key input wrapper in place" $ do
            let inner = object ["query" .= ("granite" :: Text)]
                norm = normalizeToolCall (ToolCall "discover" (object ["input" .= inner]))
            tcArgs norm `shouldBe` inner
            tcName norm `shouldBe` "discover"
        it "leaves flat args untouched" $ do
            let call = ToolCall "list_cells" (object ["full" .= True])
            normalizeToolCall call `shouldBe` call
        it "unwraps a doubly-nested wrapper" $ do
            let inner = object ["cell_id" .= (3 :: Int)]
                norm =
                    normalizeToolCall
                        ( ToolCall
                            "read_cell"
                            (object ["arguments" .= object ["input" .= inner]])
                        )
            tcArgs norm `shouldBe` inner

    describe "name-baked arguments still resolve (weak-model spelling)" $
        it "'discover granite' carries its inline query" $
            routeCall (ToolCall "discover granite" (object []))
                `shouldBe` RouteDiscover "granite" (object [])
