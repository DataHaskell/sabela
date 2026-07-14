{-# LANGUAGE OverloadedStrings #-}

{- | The Anthropic 'ModelProvider' adapter: the anti-corruption layer between the
neutral kernel and Anthropic's Messages API. Outbound it renders the neutral
request into a 'A.MessagesRequest', applying the prompt-cache policy that used to
live in the notebook loop (stable system prefix on the 1-hour TTL, the volatile
block on the default, the last tool cached). Inbound it streams text deltas to
the sink and maps the final 'A.MessageResponse' to a 'Completion'. @cache_control@
and @tool_use_id@ never escape this module.
-}
module Sabela.LLM.Anthropic (
    anthropicProvider,
    buildRequest,
    responseToCompletion,
) where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Network.HTTP.Client (Manager)

import Sabela.AI.Capabilities.ToolName (toolWireName)
import Sabela.AI.Types (toolOutcomeIsError, toolOutcomeValue)
import qualified Sabela.Anthropic.Types as A
import Sabela.Ids (ToolCallId (..))
import Sabela.LLM.Anthropic.Client (streamMessages)
import Sabela.LLM.Completion (Completion (..), StopCondition (..))
import Sabela.LLM.Message (
    ContentPart (..),
    Message (..),
    Role (..),
    ToolCall (..),
    ToolResult (..),
 )
import Sabela.LLM.Provider (
    ChunkSink (..),
    CompletionRequest (..),
    ModelProvider (..),
    ProviderCaps (..),
 )
import Sabela.LLM.Tool (ToolSpec (..))
import Sabela.LLM.Usage (TokenUsage (..))

{- | An Anthropic-backed provider. The config supplies the API key, base URL,
and the model ('A.acModel') this provider speaks; the request carries no model.
-}
anthropicProvider :: Manager -> A.AnthropicConfig -> ModelProvider
anthropicProvider mgr cfg =
    ModelProvider
        { mpName = "anthropic"
        , mpCaps =
            ProviderCaps
                { capStreaming = True
                , capToolCallIds = True
                , capPromptCache = True
                }
        , mpComplete = complete
        }
  where
    complete req cancel sink = do
        eresp <-
            streamMessages mgr cfg (buildRequest (A.acModel cfg) req) cancel (onEvent sink)
        pure (responseToCompletion <$> eresp)
    onEvent sink ev = case ev of
        A.SEContentBlockDelta _ (A.TextDelta t) -> onTextDelta sink t
        _ -> pure ()

{- | Render the neutral request into Anthropic's wire body, applying the
prompt-cache policy: the first system block takes the 1-hour TTL (stable prompt),
later blocks the default TTL (volatile), and the last tool is cached so the whole
schema block joins the cached prefix.
-}
buildRequest :: Text -> CompletionRequest -> A.MessagesRequest
buildRequest model req =
    A.MessagesRequest
        { A.mrModel = model
        , A.mrMaxTokens = crMaxTokens req
        , A.mrSystem = systemBlocks (crSystem req)
        , A.mrMessages = map toAMessage (crMessages req)
        , A.mrTools = withLastCached (map toToolDef (crTools req))
        , A.mrStream = True
        }

systemBlocks :: [Text] -> [A.SystemBlock]
systemBlocks [] = []
systemBlocks (b0 : rest) =
    A.SystemBlock b0 (Just A.EphemeralHour)
        : [A.SystemBlock b (Just A.Ephemeral) | b <- rest]

withLastCached :: [A.ToolDef] -> [A.ToolDef]
withLastCached xs = case reverse xs of
    [] -> []
    (t : rest) -> reverse (t{A.tdCacheControl = Just A.EphemeralHour} : rest)

toToolDef :: ToolSpec -> A.ToolDef
toToolDef spec =
    A.ToolDef
        { A.tdName = toolWireName (toolName spec)
        , A.tdDescription = toolDescription spec
        , A.tdInputSchema = toolSchema spec
        , A.tdCacheControl = Nothing
        }

toAMessage :: Message -> A.Message
toAMessage (Message role parts) =
    A.Message
        { A.msgRole = toARole role
        , A.msgContent = map toBlock parts
        }

toARole :: Role -> A.Role
toARole Assistant = A.RoleAssistant
toARole _ = A.RoleUser

toBlock :: ContentPart -> A.ContentBlock
toBlock (TextPart t) = A.TextBlock t
toBlock (ToolCallPart tc) =
    A.ToolUseBlock
        { A.tubId = idText (tcId tc)
        , A.tubName = tcName tc
        , A.tubInput = tcInput tc
        }
toBlock (ToolResultPart tr) =
    A.ToolResultBlock
        { A.trbToolUseId = idText (trFor tr)
        , A.trbIsError = toolOutcomeIsError (trOutcome tr)
        , A.trbContent = [A.TextBlock (valueText (toolOutcomeValue (trOutcome tr)))]
        }

responseToCompletion :: A.MessageResponse -> Completion
responseToCompletion resp =
    Completion
        { compParts = concatMap fromBlock (A.mrsContent resp)
        , compStop = maybe Done fromStopReason (A.mrsStopReason resp)
        , compUsage = maybe mempty fromUsage (A.mrsUsage resp)
        }

{- | Response blocks are only text or tool_use; a stray tool_result is dropped.
An empty text block is dropped too — Anthropic streams a leading empty one, and
re-sending an empty @text@ block on the next turn is rejected.
-}
fromBlock :: A.ContentBlock -> [ContentPart]
fromBlock (A.TextBlock t) = [TextPart t | not (T.null t)]
fromBlock (A.ToolUseBlock tid name input) =
    [ToolCallPart (ToolCall (ToolCallId tid) name (decodeToolInput input))]
fromBlock A.ToolResultBlock{} = []

{- | A streamed @tool_use@ block's @input@ arrives as an accumulated JSON
*string* (the @input_json_delta@ path), not an object; decode it. A decode
failure is almost always a @max_tokens@-clipped payload, so surface an actionable
@_parseError@ (as the old @finalizeContent@ did) rather than forwarding a broken
string that would then be re-serialised as an invalid @tool_use.input@.
-}
decodeToolInput :: Value -> Value
decodeToolInput (String s) =
    case decode (TLE.encodeUtf8 (TL.fromStrict s)) of
        Just v -> v
        Nothing -> object ["_parseError" .= parseErrorHint, "raw" .= s]
decodeToolInput v = v

parseErrorHint :: Text
parseErrorHint =
    "Tool input JSON failed to parse — almost certainly truncated mid-generation. \
    \Split the payload across multiple tool calls (e.g. smaller propose_edit, or \
    \insert an empty cell then patch it in a follow-up)."

fromStopReason :: A.StopReason -> StopCondition
fromStopReason A.SREndTurn = Done
fromStopReason A.SRToolUse = WantsTools
fromStopReason A.SRMaxTokens = Truncated

fromUsage :: A.Usage -> TokenUsage
fromUsage u =
    TokenUsage
        { tuInput = A.uInputTokens u
        , tuOutput = A.uOutputTokens u
        , tuCacheWrite = A.uCacheCreationInputTokens u
        , tuCacheRead = A.uCacheReadInputTokens u
        }

idText :: ToolCallId -> Text
idText (ToolCallId t) = t

valueText :: Value -> Text
valueText (String s) = s
valueText v = TL.toStrict (TLE.decodeUtf8 (encode v))
