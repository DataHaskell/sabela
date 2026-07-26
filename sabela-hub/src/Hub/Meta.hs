{-# LANGUAGE OverloadedStrings #-}

module Hub.Meta (
    sanitizeLine,
    writeMetaLine,
    parseMeta,
) where

import Data.Text (Text)
import qualified Data.Text as T

sanitizeLine :: Text -> Text
sanitizeLine = T.map (\c -> if c == '\n' || c == '\r' then ' ' else c)

writeMetaLine :: Text -> Text -> Text
writeMetaLine k v = k <> "=" <> sanitizeLine v

parseMeta :: Text -> [(Text, Text)]
parseMeta txt =
    [ (k, T.drop 1 v)
    | line <- T.lines txt
    , let (k, v) = T.breakOn "=" line
    , not (T.null v)
    ]
