{-# LANGUAGE OverloadedStrings #-}

{- | What the server advertises about itself: tool annotations a host can act
on, a prompt it can offer as a command, and the notebook as a resource.
-}
module Test.McpSurfaceSpec (mcpSurfaceSpec) where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Mcp (
    handleLine,
    initializeResult,
    mcpCatalogue,
    mcpEnvOver,
    mcpSession,
 )
import Test.StackFixtures (Fake)

mcpSurfaceSpec :: Spec
mcpSurfaceSpec = describe "what the MCP server advertises" $ do
    it "declares the prompt and resource capabilities it serves" $ do
        let caps = field "capabilities" (initializeResult (object []))
        caps `shouldSatisfy` maybe False (hasKey "prompts")
        caps `shouldSatisfy` maybe False (hasKey "resources")

    it "marks the read-only tools, so a host can stop prompting for them" $ do
        readOnlyOf "discover" `shouldBe` Just (Bool True)
        readOnlyOf "list_cells" `shouldBe` Just (Bool True)
        readOnlyOf "check_type" `shouldBe` Just (Bool True)
        readOnlyOf "read_source" `shouldBe` Just (Bool True)
        readOnlyOf "insert_cell" `shouldBe` Just (Bool False)

    it "marks read_source as reaching outside the notebook" $
        annotationOf "openWorldHint" "read_source" `shouldBe` Just (Bool True)

    it "marks what destroys work, and only that" $ do
        destructiveOf "delete_cell" `shouldBe` Just (Bool True)
        destructiveOf "kernel_restart" `shouldBe` Just (Bool True)
        destructiveOf "insert_cell" `shouldBe` Just (Bool False)

    it "titles every tool in words a person reads" $
        [n | (n, t) <- titles, T.null t || n == t] `shouldBe` []

    it "offers a pairing prompt a host can surface as a command" $ do
        r <- rpc "prompts/list" (object [])
        promptNames r `shouldSatisfy` elem "pair"

    it "answers prompts/get with the orientation, as a user message" $ do
        r <- rpc "prompts/get" (object ["name" .= ("pair" :: Text)])
        renderJson r `shouldSatisfy` T.isInfixOf "reactive Haskell notebook"
        renderJson r `shouldSatisfy` T.isInfixOf "\"role\":\"user\""

    it "exposes the notebook as a resource" $ do
        r <- rpc "resources/list" (object [])
        renderJson r `shouldSatisfy` T.isInfixOf "siza://notebook"

    it "reads that resource off the live notebook" $ do
        r <- rpc "resources/read" (object ["uri" .= ("siza://notebook" :: Text)])
        renderJson r `shouldSatisfy` T.isInfixOf "the only cell"

    it "still refuses a method it does not serve" $ do
        r <- rpc "completion/complete" (object [])
        renderJson r `shouldSatisfy` T.isInfixOf "method not found"

rpc :: Text -> Value -> IO Value
rpc method params = do
    ss <- mcpSession
    let env = mcpEnvOver ss notebookFake mcpCatalogue
        line =
            LBS.toStrict
                ( encode
                    ( object
                        [ "jsonrpc" .= ("2.0" :: Text)
                        , "id" .= (1 :: Int)
                        , "method" .= method
                        , "params" .= params
                        ]
                    )
                )
    fromMaybe (object []) <$> handleLine env line

notebookFake :: Fake
notebookFake (ToolCall name _)
    | name == "list_cells" =
        pure
            ( Right
                ( ToolOk
                    ( object
                        [ "cells"
                            .= [ object
                                    [ "id" .= (1 :: Int)
                                    , "source" .= ("the only cell" :: Text)
                                    ]
                               ]
                        ]
                    )
                )
            )
notebookFake _ = pure (Right (ToolOk (object [])))

readOnlyOf :: Text -> Maybe Value
readOnlyOf = annotationOf "readOnlyHint"

destructiveOf :: Text -> Maybe Value
destructiveOf = annotationOf "destructiveHint"

annotationOf :: Text -> Text -> Maybe Value
annotationOf key name = do
    tool <- lookupTool name
    anns <- field "annotations" tool
    field key anns

titles :: [(Text, Text)]
titles =
    [ (n, t)
    | Object o <- mcpCatalogue
    , let t = maybe "" asText (field "annotations" (Object o) >>= field "title")
    , Just (String n) <- [KM.lookup "name" o]
    ]

asText :: Value -> Text
asText (String s) = s
asText _ = ""

lookupTool :: Text -> Maybe Value
lookupTool name =
    case [Object o | Object o <- mcpCatalogue, KM.lookup "name" o == Just (String name)] of
        (t : _) -> Just t
        [] -> Nothing

promptNames :: Value -> [Text]
promptNames v = case field "result" v >>= field "prompts" of
    Just (Array a) -> [n | Object o <- foldr (:) [] a, Just (String n) <- [KM.lookup "name" o]]
    _ -> []

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

hasKey :: Text -> Value -> Bool
hasKey k (Object o) = KM.member (K.fromText k) o
hasKey _ _ = False

renderJson :: Value -> Text
renderJson = TE.decodeUtf8 . LBS.toStrict . encode
