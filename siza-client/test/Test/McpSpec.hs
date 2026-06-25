{-# LANGUAGE OverloadedStrings #-}

{- | The MCP stdio server's pure surface: JSON-RPC framing, the notification
rule, the @input_schema@→@inputSchema@ projection (drift guard), tool-result
mapping, and the client-side pre-flight gate that keeps a weak model from
landing broken source. These run without network; 'Siza.Mcp.gateForMcp' invokes
the real parser.
-}
module Test.McpSpec (mcpSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft, isRight)
import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Siza.Mcp (
    Rpc (..),
    decodeRpc,
    errorResp,
    gateForMcp,
    initializeResult,
    routeResponse,
    successResp,
    toMcpTool,
    toolResult,
 )
import Siza.Security (advisoryPolicy)
import Test.Hspec

field :: Key -> Value -> Maybe Value
field k (Object o) = KM.lookup k o
field _ _ = Nothing

type Key = K.Key

mcpSpec :: Spec
mcpSpec = describe "Siza.Mcp" $ do
    describe "decodeRpc" $ do
        it "parses a request with id, method, params" $ do
            let r =
                    decodeRpc
                        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\",\"params\":{}}"
            fmap rpcMethod r `shouldBe` Right "tools/list"
            fmap rpcId r `shouldBe` Right (Just (Number 1))
        it "treats a missing id as a notification" $ do
            let r = decodeRpc "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
            fmap rpcId r `shouldBe` Right Nothing
        it "rejects a malformed line" $
            isLeft (decodeRpc "{not json") `shouldBe` True

    describe "routeResponse (the notification rule)" $ do
        it "returns a response only when the request had an id" $ do
            routeResponse (Rpc (Just (Number 1)) "ping" (object [])) (object [])
                `shouldSatisfy` (/= Nothing)
        it "returns nothing for a notification (no id)" $
            routeResponse (Rpc Nothing "notifications/initialized" (object [])) (object [])
                `shouldBe` Nothing

    describe "initializeResult" $ do
        it "echoes the client's protocolVersion" $
            field
                "protocolVersion"
                (initializeResult (object ["protocolVersion" .= ("2025-03-26" :: String)]))
                `shouldBe` Just (String "2025-03-26")
        it "advertises a tools capability and serverInfo name" $ do
            let r = initializeResult (object [])
            (field "capabilities" r >>= field "tools") `shouldSatisfy` (/= Nothing)
            (field "serverInfo" r >>= field "name") `shouldBe` Just (String "siza")

    describe "toMcpTool (drift guard: input_schema -> inputSchema)" $ do
        let td =
                object
                    [ "name" .= ("insert_cell" :: String)
                    , "description" .= ("d" :: String)
                    , "input_schema" .= object ["type" .= ("object" :: String)]
                    , "cache_control" .= object ["type" .= ("ephemeral" :: String)]
                    ]
            mcp = toMcpTool td
        it "renames input_schema to inputSchema, body verbatim" $
            field "inputSchema" mcp
                `shouldBe` Just (object ["type" .= ("object" :: String)])
        it "drops the Anthropic-only fields" $ do
            field "input_schema" mcp `shouldBe` Nothing
            field "cache_control" mcp `shouldBe` Nothing
        it "keeps name and description" $ do
            field "name" mcp `shouldBe` Just (String "insert_cell")
            field "description" mcp `shouldBe` Just (String "d")

    describe "toolResult" $
        it "wraps text in a content block with isError" $ do
            let r = toolResult True "boom"
            field "isError" r `shouldBe` Just (Bool True)
            case field "content" r of
                Just (Array _) -> pure ()
                _ -> expectationFailure "content should be an array"

    describe "successResp / errorResp" $ do
        it "success carries jsonrpc, id, result" $ do
            let r = successResp (Number 7) (object ["ok" .= True])
            field "id" r `shouldBe` Just (Number 7)
            field "jsonrpc" r `shouldBe` Just (String "2.0")
        it "error carries a code and message" $ do
            let e = errorResp Null (-32601) "method not found"
            (field "error" e >>= field "code") `shouldBe` Just (Number (-32601))

    describe "gateForMcp (the client-side safety gate)" $ do
        it "passes a non-mutation tool through untouched" $ do
            v <- gateForMcp advisoryPolicy ListCells (object [])
            v `shouldSatisfy` isRight
        it "passes a mutation with no source field" $ do
            v <-
                gateForMcp advisoryPolicy InsertCell (object ["after_cell_id" .= (1 :: Int)])
            v `shouldSatisfy` isRight
        it "passes a mutation with valid Haskell source" $ do
            v <-
                gateForMcp
                    advisoryPolicy
                    InsertCell
                    (object ["source" .= ("answer = 42" :: String)])
            v `shouldSatisfy` isRight
        it "blocks a mutation whose source fails to parse" $ do
            v <-
                gateForMcp advisoryPolicy InsertCell (object ["source" .= ("f = (" :: String)])
            v `shouldSatisfy` isLeft
