{- | Provider-neutral token accounting. The two cache counters are optional so a
provider that does not report prompt caching (Ollama) leaves them 'Nothing'
while one that does (Anthropic) fills them. The 'Monoid' folds per-turn usage
into a session total, replacing the ad-hoc merge that lived in the loop.
-}
module Sabela.LLM.Usage (TokenUsage (..)) where

import Data.Maybe (fromMaybe)

data TokenUsage = TokenUsage
    { tuInput :: !Int
    , tuOutput :: !Int
    , tuCacheWrite :: !(Maybe Int)
    , tuCacheRead :: !(Maybe Int)
    }
    deriving (Eq, Show)

instance Semigroup TokenUsage where
    a <> b =
        TokenUsage
            { tuInput = tuInput a + tuInput b
            , tuOutput = tuOutput a + tuOutput b
            , tuCacheWrite = addMaybe (tuCacheWrite a) (tuCacheWrite b)
            , tuCacheRead = addMaybe (tuCacheRead a) (tuCacheRead b)
            }
      where
        addMaybe Nothing Nothing = Nothing
        addMaybe x y = Just (fromMaybe 0 x + fromMaybe 0 y)

instance Monoid TokenUsage where
    mempty = TokenUsage 0 0 Nothing Nothing
