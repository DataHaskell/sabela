module Sabela.Ids (
    TurnId (..),
    EditId (..),
    ToolCallId (..),
) where

import Data.Aeson (ToJSON (..))
import Data.Text (Text)

newtype TurnId = TurnId Int
    deriving (Eq, Ord, Show)

instance ToJSON TurnId where
    toJSON (TurnId n) = toJSON n

newtype EditId = EditId Int
    deriving (Eq, Ord, Show)

instance ToJSON EditId where
    toJSON (EditId n) = toJSON n

newtype ToolCallId = ToolCallId Text
    deriving (Eq, Show)

instance ToJSON ToolCallId where
    toJSON (ToolCallId t) = toJSON t
