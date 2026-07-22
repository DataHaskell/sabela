{-# LANGUAGE OverloadedStrings #-}

{- | R9-T5 schema-match recovery at the routing boundary: a garbled name
carrying a payload that uniquely fits one offered schema now DISPATCHES
(stamped by 'recoverTurn'); ambiguity reprompts naming the parse failure;
a word-like foreign name stays honestly unknown. Safety invariant: an
auto-dispatch only ever lands on a schema-consistent tool.
-}
module Test.SchemaRecoverySpec (schemaRecoverySpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.ToolName (
    ToolName (InsertCell, ReplaceCellSource),
    toolWireName,
 )
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..))
import Siza.Agent.ToolRoute (Route (..), recoverTurn, routeCallWith)
import Siza.Agent.Tools (offeredArgKeys, offeredNames)

schemaRecoverySpec :: Spec
schemaRecoverySpec = describe "schema-match recovery boundary (R9-T5)" $ do
    describe
        "garbled-name recovery: corruptions of EVERY offered name either \
        \dispatch a schema-consistent tool or reprompt"
        $ forM_ offeredNames
        $ \name ->
            it (T.unpack ("corruptions of " <> name)) $
                forM_ (corruptionsOf name) $ \bad -> do
                    let args = requiredPayloadFor name
                        route = routeCallWith offeredArgKeys (ToolCall bad args)
                    -- Safety invariant: an auto-dispatch only ever lands on
                    -- a tool whose schema the given argument keys fit.
                    case route of
                        RouteTool tn a -> do
                            (bad, dispatchFits tn a) `shouldBe` (bad, True)
                            a `shouldBe` args
                        RouteDiscover _ _ ->
                            (bad, name) `shouldBe` (bad, "discover")
                        RouteBadArgs hint ->
                            hint
                                `shouldSatisfy` T.isInfixOf "could not parse tool call"
                        RouteUnknown _ -> pure ()

    describe "unique schema recovery dispatches (was suggest-only)" $ do
        it "the run-085948 '?..??' replace_cell_source payload dispatches" $ do
            let payload =
                    object
                        [ "cell_id" .= (0 :: Int)
                        , "new_source" .= ("import Graphics.Plotly\nfig = 1" :: Text)
                        ]
            case routeCallWith offeredArgKeys (ToolCall "?..??" payload) of
                RouteTool tn a -> do
                    tn `shouldBe` ReplaceCellSource
                    a `shouldBe` payload
                other ->
                    expectationFailure ("expected a dispatch, got " <> show other)
        it "the run-085948 '…?' insert_cell payload dispatches" $ do
            let payload = object ["source" .= ("bars = [1,2,3]" :: Text)]
            case routeCallWith offeredArgKeys (ToolCall "\x2026?" payload) of
                RouteTool tn _ -> tn `shouldBe` InsertCell
                other ->
                    expectationFailure ("expected a dispatch, got " <> show other)
        it "the topMonth 'insert_cell?We' replace payload dispatches as replace" $ do
            let payload =
                    object
                        ["cell_id" .= (3 :: Int), "new_source" .= ("x = 2" :: Text)]
            case routeCallWith offeredArgKeys (ToolCall "insert_cell?We" payload) of
                RouteTool tn _ -> tn `shouldBe` ReplaceCellSource
                other ->
                    expectationFailure ("expected a dispatch, got " <> show other)
        it "an ambiguous fingerprint reprompts naming the parse failure" $
            case routeCallWith
                offeredArgKeys
                (ToolCall "??" (object ["cell_id" .= (1 :: Int)])) of
                RouteBadArgs hint -> do
                    hint `shouldSatisfy` T.isInfixOf "could not parse tool call"
                    hint `shouldSatisfy` T.isInfixOf "read_cell"
                other ->
                    expectationFailure ("expected a reprompt, got " <> show other)
        it "empty args cannot fingerprint: reprompt, never a guess" $
            case routeCallWith offeredArgKeys (ToolCall "??" (object [])) of
                RouteBadArgs hint ->
                    hint `shouldSatisfy` T.isInfixOf "could not parse tool call"
                other ->
                    expectationFailure ("expected a reprompt, got " <> show other)
        it "a word-like foreign name never borrows a fingerprint" $
            forM_ (["frobnicate", "run_sql"] :: [Text]) $ \name -> do
                let payload =
                        object
                            [ "cell_id" .= (0 :: Int)
                            , "new_source" .= ("x = 1" :: Text)
                            ]
                case routeCallWith offeredArgKeys (ToolCall name payload) of
                    RouteUnknown n -> n `shouldBe` name
                    other ->
                        expectationFailure ("expected RouteUnknown, got " <> show other)

    describe "recoverTurn stamps recovered calls into turnRaw (R8.4)" $ do
        it "renames a unique-schema garble and stamps the raw message" $ do
            let payload = object ["source" .= ("x = 1" :: Text)]
                raw =
                    object
                        [ "role" .= ("assistant" :: Text)
                        , "content" .= ("" :: Text)
                        , "tool_calls"
                            .= [ object
                                    [ "function"
                                        .= object
                                            [ "name" .= ("\x2026?" :: Text)
                                            , "arguments" .= payload
                                            ]
                                    ]
                               ]
                        ]
                t = Turn raw "" [ToolCall "\x2026?" payload]
                t' = recoverTurn offeredArgKeys t
            map tcName (turnCalls t') `shouldBe` ["insert_cell"]
            rawNames (turnRaw t') `shouldBe` ["insert_cell"]
        it "leaves a resolvable name-baked call untouched (no query loss)" $ do
            let t = Turn (object []) "" [ToolCall "discover granite" (object [])]
            turnCalls (recoverTurn offeredArgKeys t)
                `shouldBe` [ToolCall "discover granite" (object [])]
        it "leaves an ambiguous garble untouched (the router reprompts)" $ do
            let payload = object ["cell_id" .= (1 :: Int)]
                t = Turn (object []) "" [ToolCall "??" payload]
            turnCalls (recoverTurn offeredArgKeys t)
                `shouldBe` [ToolCall "??" payload]

{- | A tool's payload of exactly its required argument keys, values keyed by
name — the minimal call a weak model actually sends.
-}
requiredPayloadFor :: Text -> Value
requiredPayloadFor tool =
    object
        [ K.fromText k .= valFor k
        | (t, (_, rq)) <- offeredArgKeys
        , t == tool
        , k <- rq
        ]
  where
    valFor "cell_id" = Number 1
    valFor k = String ("value for " <> k)

-- | Do the given argument keys fit the dispatched tool's offered schema?
dispatchFits :: ToolName -> Value -> Bool
dispatchFits tn (Object o) =
    case lookup (toolWireName tn) offeredArgKeys of
        Just (ps, rq) ->
            all (`elem` ps) keys && all (`elem` keys) rq
        Nothing -> False
  where
    keys = map K.toText (KM.keys o)
dispatchFits _ _ = False

-- | The function names stamped under @tool_calls@ in a raw message.
rawNames :: Value -> [Text]
rawNames (Object o) = case KM.lookup "tool_calls" o of
    Just (Array a) ->
        [ n
        | Object c <- foldr (:) [] a
        , Just (Object f) <- [KM.lookup "function" c]
        , Just (String n) <- [KM.lookup "name" f]
        ]
    _ -> []
rawNames _ = []

{- | A bounded, systematic set of <=2-edit corruptions: every single
deletion, substitution, and insertion, plus two-edit combinations; anything
that lands on an offered name is excluded (it routes as that tool).
-}
corruptionsOf :: Text -> [Text]
corruptionsOf name =
    [ bad
    | bad <- nub (oneEdit ++ twoEdit)
    , not (T.null bad)
    , bad `notElem` offeredNames
    ]
  where
    n = T.length name
    del i = T.take i name <> T.drop (i + 1) name
    sub i c = T.take i name <> T.singleton c <> T.drop (i + 1) name
    ins i c = T.take i name <> T.singleton c <> T.drop i name
    oneEdit =
        [del i | i <- [0 .. n - 1]]
            ++ [sub i c | i <- [0 .. n - 1], c <- "xq"]
            ++ [ins i 'x' | i <- [0 .. n]]
    twoEdit =
        [T.drop 2 name, T.dropEnd 2 name, "x" <> name <> "x"]
            ++ [T.take i name <> "xx" <> T.drop (i + 2) name | i <- [0, n `div` 2]]
