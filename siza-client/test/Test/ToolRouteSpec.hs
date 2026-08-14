{-# LANGUAGE OverloadedStrings #-}

module Test.ToolRouteSpec (toolRouteSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.ToolName (ToolName (ReadSource))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.ToolRoute (
    Route (..),
    normalizeToolCall,
    routeCall,
    routeCallWith,
 )
import Siza.Agent.Tools (offeredArgKeys, offeredNames)

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

    describe "read_source is a plain server tool" $
        it "routes to the ReadSource ToolName" $ do
            let args = object ["module" .= ("Data.Time.Clock" :: Text)]
            routeCall (ToolCall "read_source" args)
                `shouldBe` RouteTool ReadSource args

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

    describe "a correctly-named call still gets its OWN schema checked" $ do
        it
            "insert_cell with invented keys reprompts instead of silently emptying source"
            $ do
                let payload =
                        object
                            [ "filePath" .= ("test.md" :: Text)
                            , "line" .= (3 :: Int)
                            , "content" .= ("main = print 1" :: Text)
                            ]
                case routeCallWith offeredArgKeys (ToolCall "insert_cell" payload) of
                    RouteBadArgs hint -> do
                        hint `shouldSatisfy` T.isInfixOf "insert_cell"
                        hint `shouldSatisfy` T.isInfixOf "source"
                    other -> expectationFailure ("expected RouteBadArgs, got " <> show other)

        it "names what was given but not recognized, not just what's missing" $ do
            let payload = object ["filePath" .= ("test.md" :: Text)]
            case routeCallWith offeredArgKeys (ToolCall "insert_cell" payload) of
                RouteBadArgs hint -> hint `shouldSatisfy` T.isInfixOf "filePath"
                other -> expectationFailure ("expected RouteBadArgs, got " <> show other)

        it "a well-named, well-shaped call is unaffected" $ do
            let payload = object ["source" .= ("main = print 1" :: Text)]
            routeCallWith offeredArgKeys (ToolCall "insert_cell" payload)
                `shouldSatisfy` isTool

        it "an unrecognized extra key alongside every required one still reprompts" $ do
            -- Consistent with the name-recovery path's own argsFit: a
            -- schema match is exact, not "required fields present, plus
            -- whatever else."
            let payload =
                    object
                        [ "source" .= ("main = print 1" :: Text)
                        , "not_a_real_field" .= ("x" :: Text)
                        ]
            case routeCallWith offeredArgKeys (ToolCall "insert_cell" payload) of
                RouteBadArgs hint -> hint `shouldSatisfy` T.isInfixOf "not_a_real_field"
                other -> expectationFailure ("expected RouteBadArgs, got " <> show other)
  where
    isTool (RouteTool _ _) = True
    isTool _ = False
