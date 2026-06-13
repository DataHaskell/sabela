{-# LANGUAGE OverloadedStrings #-}

{- | The single chokepoint for the hub's line-based @key=value@ metadata
files. Every value is sanitized on write — a value can never inject a forged
line (e.g. @owner=@ / @isAdmin=@) into a store, regardless of caller.
-}
module Hub.Meta (
    sanitizeLine,
    writeMetaLine,
    parseMeta,
) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Collapse CR/LF to spaces so a value always stays on its own line.
sanitizeLine :: Text -> Text
sanitizeLine = T.map (\c -> if c == '\n' || c == '\r' then ' ' else c)

-- | The only way a store emits a @key=value@ line; sanitizes unconditionally.
writeMetaLine :: Text -> Text -> Text
writeMetaLine k v = k <> "=" <> sanitizeLine v

{- | Parse @key=value@ lines (the inverse of 'writeMetaLine'); values may
contain @=@, lines without one are skipped.
-}
parseMeta :: Text -> [(Text, Text)]
parseMeta txt =
    [ (k, T.drop 1 v)
    | line <- T.lines txt
    , let (k, v) = T.breakOn "=" line
    , not (T.null v)
    ]
