{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Loop.Support (
    nudgeK,
    nudgeFloor,
    maxChatRetries,
    maxStuckVerifies,
    stuckFinal,
    callActs,
    sampleK,
    writeSource,
    replaceCall,
    groundingMsgs,
    qualifiedBaseNames,
    nubShort,
    factsBlock,
    streakHints,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAlphaNum, isLower)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.ToolName (actsOnNotebook, parseToolName)
import Sabela.AI.CellResult (CellId)
import Sabela.AI.Types (ToolOutcome)
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Owned (OwnedCell (..))
import Siza.Agent.Streak (bumpStreak, streakContrast)
import Siza.Agent.Tools (renderOutcome)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

nudgeK :: Int
nudgeK = 2

nudgeFloor :: Int
nudgeFloor = 3

maxChatRetries :: Int
maxChatRetries = 2

maxStuckVerifies :: Int
maxStuckVerifies = 3

stuckFinal :: Text
stuckFinal =
    "Gave up: the deliverable's check kept failing and the last few turns changed \
    \nothing. The check may be testing the wrong value, or an effect that a pure \
    \expression cannot observe (such as an IO action's result)."

callActs :: ToolCall -> Bool
callActs = maybe False actsOnNotebook . parseToolName . tcName

sampleK :: IO Int
sampleK = maybe 1 (max 1) . (>>= readMaybe) <$> lookupEnv "SIZA_SAMPLE_K"

writeSource :: ToolCall -> Maybe Text
writeSource tc = case tcArgs tc of
    Object o -> case KM.lookup "source" o of
        Just (String s) | not (T.null (T.strip s)) -> Just s
        _ -> case KM.lookup "new_source" o of
            Just (String s) | not (T.null (T.strip s)) -> Just s
            _ -> Nothing
    _ -> Nothing

replaceCall :: Int -> Text -> ToolCall
replaceCall cid src =
    ToolCall "replace_cell_source" (object ["cell_id" .= cid, "new_source" .= src])

groundingMsgs ::
    (ToolCall -> IO (Either Text ToolOutcome)) -> Text -> IO [Value]
groundingMsgs disp src = do
    let names = take 5 (nubShort (qualifiedBaseNames src))
    parts <-
        mapM
            ( \n ->
                (,) n . renderOutcome
                    <$> disp (ToolCall "find_function" (object ["query" .= n]))
            )
            names
    let body =
            T.intercalate
                "\n"
                ["`" <> n <> "`:\n" <> r | (n, r) <- parts, not (T.null (T.strip r))]
    pure
        [ object
            [ "role" .= ("user" :: Text)
            , "content"
                .= ( "Real API from the live index for the functions you used. Use these EXACT \
                     \names, types, and modules; do not guess types or wrap pure functions in `<-`:\n"
                        <> body
                   )
            ]
        | not (T.null (T.strip body))
        ]

qualifiedBaseNames :: Text -> [Text]
qualifiedBaseNames src =
    [ base
    | tok <-
        T.split (\c -> not (isAlphaNum c || c == '.' || c == '_' || c == '\'')) src
    , T.any (== '.') tok
    , let base = T.takeWhileEnd (/= '.') tok
    , not (T.null base)
    , maybe False (isLower . fst) (T.uncons base)
    ]

nubShort :: [Text] -> [Text]
nubShort = go []
  where
    go _ [] = []
    go seen (x : xs)
        | x `elem` seen = go seen xs
        | otherwise = x : go (x : seen) xs

factsBlock :: [Text] -> Text
factsBlock facts
    | null facts = ""
    | otherwise =
        "\n\nFacts already established:\n"
            <> T.unlines (map ("- " <>) facts)

streakHints ::
    IORef (Map CellId (Text, Int)) -> Map CellId OwnedCell -> IO [Text]
streakHints ref owned = do
    m0 <- readIORef ref
    let reds =
            [ (c, ocDiagnostic oc)
            | (c, oc) <- Map.toList owned
            , not (ocHealthy oc)
            ]
        (m', hints) = foldl step (m0, []) reds
        step (m, hs) (c, d) =
            let (m2, n) = bumpStreak m c d
             in (m2, hs ++ maybeToList (streakContrast n d))
    writeIORef ref m'
    pure hints
