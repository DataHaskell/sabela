{-# LANGUAGE DeriveGeneric #-}

{- | Wire types for a table's expression box (@POST /api/frame/query@): an
expression over @df@, applied in the kernel to the frame a table came from.
-}
module Sabela.Api.FrameQuery (
    FrameQueryRequest (..),
    FrameQueryResult (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Sabela.Model (OutputItem)

{- | @fqSlot@ is the @cell#n@ key the table's markup carries, and @fqQuery@
the typed query function it names; absent, the untyped one is used. With
@fqCols@ (and their @fqTypes@) the expression is a row predicate over those
columns by name; without, it is a whole-frame expression over @df@.
-}
data FrameQueryRequest = FrameQueryRequest
    { fqSlot :: Text
    , fqExpr :: Text
    , fqQuery :: Maybe Text
    , fqCols :: Maybe [Text]
    , fqTypes :: Maybe [Text]
    }
    deriving (Eq, Generic, Show)

instance ToJSON FrameQueryRequest
instance FromJSON FrameQueryRequest

data FrameQueryResult = FrameQueryResult
    { fqOutputs :: [OutputItem]
    , fqError :: Maybe Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON FrameQueryResult
instance FromJSON FrameQueryResult
