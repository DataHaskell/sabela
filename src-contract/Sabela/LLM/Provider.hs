{- | The model-provider port: a record-of-functions (like
'Sabela.SessionTypes.SessionBackend') that the orchestration layer drives
without knowing which provider is behind it. Adapters (Anthropic, Ollama) are
the only code that speaks a provider's wire dialect.

Streaming is one method, not a capability split: 'mpComplete' always takes a
'ChunkSink'. A streaming adapter drives it incrementally; a non-streaming one
(Ollama @stream:false@) simply never calls it and returns the whole
'Completion'. Callers never fork; 'capStreaming' only tells the UI whether to
promise token-by-token deltas.
-}
module Sabela.LLM.Provider (
    ProviderCaps (..),
    ChunkSink (..),
    CompletionRequest (..),
    ModelProvider (..),
) where

import Data.Text (Text)

import Sabela.LLM.Cancel (CancelToken)
import Sabela.LLM.Completion (Completion)
import Sabela.LLM.Message (Message)
import Sabela.LLM.Tool (ToolSpec)

data ProviderCaps = ProviderCaps
    { capStreaming :: Bool
    , capToolCallIds :: Bool
    , capPromptCache :: Bool
    }
    deriving (Show, Eq)

-- | Sink for streamed assistant text. Extended later (tool-arg deltas, etc.).
newtype ChunkSink = ChunkSink {onTextDelta :: Text -> IO ()}

{- | One completion request, in neutral terms. It carries NO model — the
'ModelProvider' owns its model, so a caller cannot route a provider-specific
model id to the wrong provider. @crSystem@ is plain prose (the adapter decides
any prompt-cache marking); @crMaxTokens@ is a safety cap.
-}
data CompletionRequest = CompletionRequest
    { crSystem :: [Text]
    , crMessages :: [Message]
    , crTools :: [ToolSpec]
    , crMaxTokens :: Int
    }

data ModelProvider = ModelProvider
    { mpName :: Text
    , mpCaps :: ProviderCaps
    , mpComplete ::
        CompletionRequest ->
        CancelToken ->
        ChunkSink ->
        IO (Either Text Completion)
    }
