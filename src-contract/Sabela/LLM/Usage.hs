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
