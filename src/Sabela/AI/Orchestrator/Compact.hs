{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Orchestrator.Compact (
    compactToolResult,
    resultToText,
) where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Sabela.AI.Handles (
    HandleRef (..),
    Output (..),
    storeLargeResult,
    summarizeForLLM,
 )
import Sabela.AI.Store

compactToolResultThreshold :: Int
compactToolResultThreshold = 8000

compactToolResult :: AIStore -> Value -> IO Value
compactToolResult store v =
    if smallEnough v
        then pure v
        else do
            let text = resultToText v
            r <- storeLargeResult (aiHandles store) text
            case r of
                Inline _ cleaned -> pure (String cleaned)
                Stashed (HandleRef hid summary nLines nBytes) ->
                    pure $
                        object
                            [ "_compacted" .= True
                            , "_note"
                                .= ( "Tool result exceeded inline limit; stashed. Drill in via explore_result." ::
                                        Text
                                   )
                            , "_large" .= summarizeForLLM hid summary nLines nBytes
                            ]

resultToText :: Value -> Text
resultToText (String s) = s
resultToText v = TL.toStrict (TLE.decodeUtf8 (encode v))

smallEnough :: Value -> Bool
smallEnough (String s) = T.length s <= compactToolResultThreshold
smallEnough v = LBS.length (encode v) <= fromIntegral compactToolResultThreshold
