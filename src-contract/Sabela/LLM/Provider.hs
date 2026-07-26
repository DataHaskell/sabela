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
    deriving (Eq, Show)

newtype ChunkSink = ChunkSink {onTextDelta :: Text -> IO ()}

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
