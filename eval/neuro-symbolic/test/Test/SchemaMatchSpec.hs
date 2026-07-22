{-# LANGUAGE OverloadedStrings #-}

{- | R9-T5 schema-match tool-call recovery, as a corpus property over the
live garble classes (topMonth's thinking-fused suffix, both live_test.md
punctuation shapes, truncation, near-miss) x every offered tool's valid
payload: a UNIQUE schema match recovers exactly one call stamped into
'turnRaw'; ambiguity reprompts naming the parse failure; nothing is
silently swallowed; recovery is invariant to task content.
-}
module Test.SchemaMatchSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Eval.Ollama (ToolCall (..), Turn (..), parseTurn)
import Eval.Tools (offeredArgKeys, offeredNames)
import Siza.Agent.ToolRoute (Route (..), recoverTurn, routeCallWith)

spec :: Spec
spec = describe "schema-match name recovery (R9-T5 corpus property)" $ do
    it
        "corpus: every (garble class x offered tool x valid payload) \
        \uniquely recovers stamped into turnRaw, or reprompts — never \
        \an ambiguous dispatch, never swallowed"
        $ sequence_
            [ checkCorpusMember cls garbled real args
            | (cls, garble) <- garbleClasses
            , real <- offeredNames
            , let garbled = garble real
            , garbled `notElem` offeredNames
            , let args = payloadFor real
            ]
    it
        "the topMonth thinking-fused shape recovers by schema: \
        \insert_cell?We {cell_id,new_source} -> replace_cell_source"
        $ do
            let args =
                    object
                        [ "cell_id" .= (3 :: Int)
                        , "new_source" .= ("topMonth = \"Mar\"" :: Text)
                        ]
                t =
                    recoverTurn offeredArgKeys
                        <$> parseTurn (encode (chatBody [callObjA "insert_cell?We" args]))
            case t of
                Right t' -> do
                    map tcName (turnCalls t') `shouldBe` ["replace_cell_source"]
                    rawCallNames (turnRaw t') `shouldBe` ["replace_cell_source"]
                Left e -> expectationFailure ("unexpected Left: " <> show e)
    it
        "a payload matching two schemas never auto-dispatches and \
        \reprompts naming the parse failure"
        $ do
            let call = ToolCall "\x2026?" (object ["cell_id" .= (1 :: Int)])
                t =
                    recoverTurn offeredArgKeys
                        <$> parseTurn
                            (encode (chatBody [callObjA "\x2026?" (tcArgs call)]))
            case t of
                Right t' -> map tcName (turnCalls t') `shouldBe` ["\x2026?"]
                Left e -> expectationFailure ("unexpected Left: " <> show e)
            case routeCallWith offeredArgKeys call of
                RouteBadArgs hint -> do
                    hint `shouldSatisfy` T.isInfixOf "could not parse tool call"
                    hint `shouldSatisfy` T.isInfixOf "read_cell"
                    hint `shouldSatisfy` T.isInfixOf "delete_cell"
                other ->
                    expectationFailure ("expected a parse-failure reprompt: " <> show other)
    it "an ambiguous truncated prefix (kernel_) reprompts, never guesses" $
        case routeCallWith offeredArgKeys (ToolCall "kernel_" (object [])) of
            RouteBadArgs hint ->
                hint `shouldSatisfy` T.isInfixOf "could not parse tool call"
            other ->
                expectationFailure ("expected a parse-failure reprompt: " <> show other)
    it "a word-like foreign name never borrows a schema (no over-acceptance)" $
        forM_ (["frobnicate", "run_sql"] :: [Text]) $ \bad -> do
            let args =
                    object
                        ["cell_id" .= (1 :: Int), "new_source" .= ("x = 1" :: Text)]
            case routeCallWith offeredArgKeys (ToolCall bad args) of
                RouteUnknown n -> n `shouldBe` bad
                other ->
                    expectationFailure ("expected RouteUnknown, got " <> show other)
    it "recovery is invariant to task content (only name + arg keys decide)" $
        forM_ (["x = 1", "revenueTotal = sum xs", "bars = plot chart"] :: [Text]) $
            \src -> do
                let args = object ["source" .= src]
                    t =
                        recoverTurn offeredArgKeys
                            <$> parseTurn
                                (encode (chatBody [callObjA "insert_cell?We" args]))
                case t of
                    Right t' -> map tcName (turnCalls t') `shouldBe` ["insert_cell"]
                    Left e -> expectationFailure ("unexpected Left: " <> show e)

{- | The R9-T5 garble corpus: the classes measured live (topMonth's
thinking-fused suffix; both live_test.md punctuation shapes) plus the
truncation and near-miss families, applied to every offered name.
-}
garbleClasses :: [(String, Text -> Text)]
garbleClasses =
    [ ("thinking-fused suffix", (<> "?We"))
    , ("pure punctuation", const "\x2026\x2026..????????????")
    , ("unicode-ellipsis garble", const "?\x2026..??...????..??..???]")
    , ("truncated name", \n -> T.take (T.length n - 4) n)
    , ("near-miss name", \n -> T.dropEnd 1 n <> "z")
    ]

-- | A full-props argument payload for one offered tool, values keyed by name.
payloadFor :: Text -> Value
payloadFor tool =
    object
        [ K.fromText k .= valFor k
        | (t, (ps, _)) <- offeredArgKeys
        , t == tool
        , k <- ps
        ]

valFor :: Text -> Value
valFor "cell_id" = Number 1
valFor "full" = Bool True
valFor "limit" = Number 5
valFor k = String ("task content for " <> k)

{- | Test-side mirror of the schema-fingerprint law (testing-plan R5.9 spec
wording): every given key among the tool's properties, every required key
given; empty args fingerprint nothing.
-}
schemaFits :: Value -> [Text]
schemaFits (Object o) =
    [ t
    | not (null keys)
    , (t, (ps, rq)) <- offeredArgKeys
    , all (`elem` ps) keys
    , all (`elem` keys) rq
    ]
  where
    keys = map K.toText (KM.keys o)
schemaFits _ = []

{- | Expected unique recovery: name-evidence classes (suffix, truncation,
near-miss, built from @real@) recover @real@; letterless classes recover
exactly the unique schema match, else nothing (reprompt).
-}
expectedRecovery :: String -> Text -> Value -> Maybe Text
expectedRecovery cls real args
    | cls `elem` ["pure punctuation", "unicode-ellipsis garble"] =
        case schemaFits args of
            [t] -> Just t
            _ -> Nothing
    | otherwise = Just real

-- | One corpus member: recovery is stamped, or the call reprompts unswallowed.
checkCorpusMember :: String -> Text -> Text -> Value -> Expectation
checkCorpusMember cls garbled real args =
    case parseTurn (encode (chatBody [callObjA garbled args])) of
        Left e ->
            expectationFailure (cls <> ": parseTurn failed: " <> show e)
        Right t0 -> do
            let t = recoverTurn offeredArgKeys t0
            length (turnCalls t) `shouldBe` 1
            case expectedRecovery cls real args of
                Just want -> do
                    (cls, real, map tcName (turnCalls t))
                        `shouldBe` (cls, real, [want])
                    (cls, real, rawCallNames (turnRaw t))
                        `shouldBe` (cls, real, [want])
                    map tcArgs (turnCalls t) `shouldBe` [args]
                Nothing -> do
                    (cls, real, map tcName (turnCalls t))
                        `shouldBe` (cls, real, [garbled])
                    case routeCallWith offeredArgKeys (ToolCall garbled args) of
                        RouteTool tn _ ->
                            expectationFailure
                                (cls <> ": ambiguous garble dispatched as " <> show tn)
                        RouteDiscover _ _ ->
                            expectationFailure
                                (cls <> ": ambiguous garble dispatched as discover")
                        _ -> pure ()

-- | A native tool_call entry with an explicit argument object.
callObjA :: Text -> Value -> Value
callObjA name args =
    object ["function" .= object ["name" .= name, "arguments" .= args]]

chatBody :: [Value] -> Value
chatBody calls =
    object
        [ "message"
            .= object
                [ "role" .= ("assistant" :: Text)
                , "content" .= ("" :: Text)
                , "tool_calls" .= calls
                ]
        ]

-- | The function names under @tool_calls@ in a recorded raw message.
rawCallNames :: Value -> [Text]
rawCallNames raw = case fieldOf "tool_calls" raw of
    Just (Array a) ->
        [ n
        | Object c <- foldr (:) [] a
        , Just (Object f) <- [KM.lookup "function" c]
        , Just (String n) <- [KM.lookup "name" f]
        ]
    _ -> []

fieldOf :: Text -> Value -> Maybe Value
fieldOf k (Object o) = KM.lookup (K.fromText k) o
fieldOf _ _ = Nothing
