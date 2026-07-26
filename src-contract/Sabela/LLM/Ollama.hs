{-# LANGUAGE OverloadedStrings #-}

module Sabela.LLM.Ollama (
    ollamaProvider,
    ollamaParseRetries,
    turnToCompletion,
    renderMessage,
    renderTool,
) where

import Data.Aeson (Value (..), encode, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Network.HTTP.Client (Manager)

import Sabela.AI.Capabilities.ToolName (toolWireName)
import Sabela.AI.Types (ToolOutcome, toolOutcomeIsError, toolOutcomeValue)
import Sabela.Ids (ToolCallId (..))
import Sabela.LLM.Completion (Completion (..), StopCondition (..))
import Sabela.LLM.Message (
    ContentPart (..),
    Message (..),
    Role (..),
    ToolCall (..),
    ToolResult (..),
 )
import qualified Sabela.LLM.Ollama.Client as C
import Sabela.LLM.Provider (
    CompletionRequest (..),
    ModelProvider (..),
    ProviderCaps (..),
 )
import Sabela.LLM.Tool (ToolSpec (..))

ollamaProvider :: Manager -> Text -> Int -> ModelProvider
ollamaProvider mgr model numCtx =
    ModelProvider
        { mpName = "ollama:" <> model
        , mpCaps =
            ProviderCaps
                { capStreaming = False
                , capToolCallIds = False
                , capPromptCache = False
                }
        , mpComplete = complete
        }
  where
    complete req _cancel _sink = do
        let msgs =
                renderSystem (crSystem req)
                    ++ concatMap renderMessage (crMessages req)
            tools = map renderTool (crTools req)
        result <- completeParsed ollamaParseRetries msgs tools
        pure (either (Left . C.pfReprompt) (Right . turnToCompletion) result)
    completeParsed retries msgs tools = do
        result <- C.chatWithParsed numCtx mgr model msgs tools
        case result of
            Left failure
                | retries > 0
                , C.pfClass failure /= C.TransportFailure ->
                    completeParsed
                        (retries - 1)
                        (msgs ++ [parseFailureMessage failure])
                        tools
            _ -> pure result

ollamaParseRetries :: Int
ollamaParseRetries = 2

parseFailureMessage :: C.ParseFailure -> Value
parseFailureMessage failure =
    object
        [ "role" .= ("user" :: Text)
        , "content" .= C.pfReprompt failure
        ]

renderSystem :: [Text] -> [Value]
renderSystem blocks
    | T.null joined = []
    | otherwise = [object ["role" .= ("system" :: Text), "content" .= joined]]
  where
    joined = T.intercalate "\n\n" (filter (not . T.null) blocks)

renderMessage :: Message -> [Value]
renderMessage (Message role parts) = case role of
    System -> [roleMsg "system" (textOf parts)]
    User ->
        [roleMsg "user" (textOf parts) | not (T.null (textOf parts))]
            ++ [toolResultMsg tr | ToolResultPart tr <- parts]
    Assistant ->
        [ object $
            [ "role" .= ("assistant" :: Text)
            , "content" .= textOf parts
            ]
                ++ ["tool_calls" .= map callJson toolCalls | not (null toolCalls)]
        ]
  where
    toolCalls = [tc | ToolCallPart tc <- parts]
    roleMsg r c = object ["role" .= (r :: Text), "content" .= c]
    toolResultMsg tr =
        object
            [ "role" .= ("tool" :: Text)
            , "tool_name" .= trName tr
            , "content" .= outcomeText (trOutcome tr)
            ]
    callJson tc =
        object
            [ "function"
                .= object ["name" .= tcName tc, "arguments" .= tcInput tc]
            ]

textOf :: [ContentPart] -> Text
textOf parts = T.intercalate "\n" [t | TextPart t <- parts]

renderTool :: ToolSpec -> Value
renderTool spec =
    object
        [ "type" .= ("function" :: Text)
        , "function"
            .= object
                [ "name" .= toolWireName (toolName spec)
                , "description" .= toolDescription spec
                , "parameters" .= toolSchema spec
                ]
        ]

turnToCompletion :: C.Turn -> Completion
turnToCompletion t =
    Completion
        { compParts = textParts ++ callParts
        , compStop = if null (C.turnCalls t) then Done else WantsTools
        , compUsage = mempty
        }
  where
    textParts = [TextPart (C.turnContent t) | not (T.null (C.turnContent t))]
    callParts =
        [ ToolCallPart
            ToolCall
                { tcId = ToolCallId ("ollama-" <> tShow i)
                , tcName = C.tcName c
                , tcInput = C.tcArgs c
                }
        | (i, c) <- zip [0 :: Int ..] (C.turnCalls t)
        ]

outcomeText :: ToolOutcome -> Text
outcomeText o = prefix <> T.take outcomeCap (valueText (toolOutcomeValue o))
  where
    prefix = if toolOutcomeIsError o then "TOOL ERROR: " else ""

outcomeCap :: Int
outcomeCap = 6000

valueText :: Value -> Text
valueText (String s) = s
valueText v = TL.toStrict (TLE.decodeUtf8 (encode v))

tShow :: (Show a) => a -> Text
tShow = T.pack . show
