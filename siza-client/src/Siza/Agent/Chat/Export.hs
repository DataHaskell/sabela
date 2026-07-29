{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Chat.Export (
    exportCommand,
    exportFileName,
    exportText,
) where

import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Siza.Agent.Transcript (renderTranscript)

exportCommand :: Text -> Maybe (Maybe FilePath)
exportCommand line
    | stripped == "/export" = Just Nothing
    | Just rest <- T.stripPrefix "/export " stripped =
        Just (Just (T.unpack (T.strip rest)))
    | otherwise = Nothing
  where
    stripped = T.strip line

exportFileName :: UTCTime -> FilePath
exportFileName now =
    "siza-chat-" <> formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now <> ".md"

exportText :: Text -> [Value] -> Text
exportText model = renderTranscript ("siza chat \183 " <> model)
