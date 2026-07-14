{-# LANGUAGE OverloadedStrings #-}

{- | The Ollama 'ModelProvider' adapter: the anti-corruption layer between the
neutral kernel and Ollama's native @/api/chat@ wire. Outbound it renders the
neutral request (system prose, 'Message's, 'ToolSpec's) into Ollama's JSON;
inbound it maps a recovered 'Turn' into a 'Completion'. Ollama is non-streaming
(@stream:false@) and emits no tool-call ids or stop reason, so the sink is never
called, ids are synthesised, and the stop condition is derived.
-}
module Sabela.LLM.Ollama (
    ollamaProvider,
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

{- | An Ollama-backed provider. @model@ is the Ollama model tag (e.g.
@gpt-oss:20b@); @numCtx@ is the context window sent as @num_ctx@.
-}
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
        result <- C.chatWith numCtx mgr model msgs tools
        pure (turnToCompletion <$> result)

-- | System prose becomes one @role:system@ message (empty prose → none).
renderSystem :: [Text] -> [Value]
renderSystem blocks
    | T.null joined = []
    | otherwise = [object ["role" .= ("system" :: Text), "content" .= joined]]
  where
    joined = T.intercalate "\n\n" (filter (not . T.null) blocks)

{- | One neutral 'Message' → the Ollama message(s) it expands to. Ollama needs a
separate @role:tool@ message per tool result, so a message is fanned out: its
text (if any) plus one @role:tool@ message per 'ToolResultPart'. Assistant tool
calls ride in the @tool_calls@ field of the single assistant message.
-}
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

-- | A neutral 'ToolSpec' → Ollama's @{type:function, function:{…}}@ envelope.
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

{- | An Ollama 'Turn' → a neutral 'Completion': content → a text part, tool calls
→ 'ToolCallPart's with positionally-synthesised ids, stop condition derived from
whether any tool was called, and no token usage (the client reports none).
-}
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

{- | Render a tool outcome as Ollama message content. Ollama's @role:tool@
message has no @is_error@ field, so the failure is signalled by a @TOOL ERROR:@
prefix (as the eval harness did); the body is truncated so a large result cannot
blow @num_ctx@.
-}
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
