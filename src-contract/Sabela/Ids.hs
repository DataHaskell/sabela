{- | Typed identifiers shared by the AI subsystem and the 'NotebookEvent'
SSE wire types. Lives in its own leaf module because 'Sabela.Model' can't
depend on 'Sabela.AI.Types' (the dependency runs the other way), and the
'EvChat*' constructors need these newtypes to stop masquerading as @Int@.

The 'ToJSON' instances render bare integers / strings so the SSE wire
format is unchanged from the pre-B1 @Int@/'Text' world.
-}
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
