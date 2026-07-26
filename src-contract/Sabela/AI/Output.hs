{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Sabela.AI.Output (
    HandleId (..),
    HandleRef (..),
    Output (..),
    inlineJson,
    stashedJson,
) where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Text (Text)
import Sabela.Model (MimeType, mimeIndicator)

newtype HandleId = HandleId Text
    deriving (Eq, Ord, Show)

data HandleRef = HandleRef
    { hrId :: HandleId
    , hrSummary :: Text
    , hrTotalLines :: Int
    , hrTotalBytes :: Int
    }
    deriving (Eq, Show)

data Output
    = Inline MimeType Text
    | Stashed HandleRef
    deriving (Eq, Show)

instance ToJSON Output where
    toJSON (Inline mime text) = inlineJson mime text
    toJSON (Stashed ref) = stashedJson ref

inlineJson :: MimeType -> Text -> Value
inlineJson mime text =
    object
        [ "mime" .= mimeIndicator mime
        , "output" .= text
        ]

stashedJson :: HandleRef -> Value
stashedJson (HandleRef (HandleId hid) summary nLines nBytes) =
    object
        [ "handleId" .= hid
        , "summary" .= summary
        , "totalLines" .= nLines
        , "totalBytes" .= nBytes
        , "hint"
            .= ( "Call explore_result with handleId to read head/tail/slice/grep of this payload." ::
                    Text
               )
        ]
