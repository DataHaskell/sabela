{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- | The typed tool-output carving (R2-5 of @docs/siza-redesign.md@ §1.3).

@storeLargeResult@'s old @Either Text (HandleId, ...)@ return is this sum in
disguise: 'Inline' is the small cleaned text, 'Stashed' is the handle plus the
existing 'summarizeForLLM' payload. The 'ToJSON' here is the single source of
the inline @{mime,output}@ and stashed handle wire shapes, so the two
compaction paths in "Sabela.AI.Capabilities.Util" stop diverging.
-}
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

{- | The stashed-result reference. Carries EXACTLY today's @storeLargeResult@
payload: the handle id, the preview summary, and the line/byte counts.
-}
data HandleRef = HandleRef
    { hrId :: HandleId
    , hrSummary :: Text
    , hrTotalLines :: Int
    , hrTotalBytes :: Int
    }
    deriving (Eq, Show)

{- | The outcome of compacting one raw output: small text inlined with its
MIME, or a 'HandleRef' to text stashed under a handle for drill-down.
-}
data Output
    = Inline MimeType Text
    | Stashed HandleRef
    deriving (Eq, Show)

{- | 'Inline' serialises to the legacy @{mime,output}@ object; 'Stashed' to
the legacy 'stashedJson' handle keys. Byte-identical to today's shapes.
-}
instance ToJSON Output where
    toJSON (Inline mime text) = inlineJson mime text
    toJSON (Stashed ref) = stashedJson ref

{- | The inline @{mime,output}@ object both compaction paths share. Kept
byte-identical to the legacy @compactOutputs@ inline shape so the run-1
WireSpec and @explore_result@ keep resolving.
-}
inlineJson :: MimeType -> Text -> Value
inlineJson mime text =
    object
        [ "mime" .= mimeIndicator mime
        , "output" .= text
        ]

{- | JSON payload describing a stored large result. Byte-identical to the
legacy @summarizeForLLM@ keys (handleId/summary/totalLines/totalBytes/hint).
-}
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
