{-# LANGUAGE OverloadedStrings #-}

{- | Reads a rendered episode back into its sections.
Both surfaces render with 'Siza.Agent.Transcript.renderTranscript', so one
parser serves a @siza chat@ export and an MCP round alike.
-}
module Siza.Retro.Episode (
    EpisodeCall (..),
    Section (..),
    parseEpisode,
) where

import Data.Char (isDigit)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

data EpisodeCall = EpisodeCall
    { ecTool :: Text
    , ecArgs :: Text
    }
    deriving (Eq, Show)

data Section = Section
    { secIndex :: Int
    , secRole :: Text
    , secTool :: Maybe Text
    , secThinking :: Text
    , secContent :: Text
    , secCalls :: [EpisodeCall]
    }
    deriving (Eq, Show)

parseEpisode :: Text -> [Section]
parseEpisode = mapMaybe sectionOf . chunksOf . T.lines

-- | Cuts the document at each numbered heading; the preamble is dropped.
chunksOf :: [Text] -> [[Text]]
chunksOf = go . dropWhile (isNothing . heading)
  where
    go [] = []
    go (l : ls) =
        let (body, rest) = break (isJust . heading) ls
         in (l : body) : go rest

heading :: Text -> Maybe (Int, Text)
heading l = do
    rest <- T.stripPrefix "## " l
    let (digits, after) = T.span isDigit rest
    n <- readInt digits
    label <- T.stripPrefix ". " after
    pure (n, T.strip label)

readInt :: Text -> Maybe Int
readInt t = case reads (T.unpack t) of
    [(n, "")] -> Just n
    _ -> Nothing

sectionOf :: [Text] -> Maybe Section
sectionOf [] = Nothing
sectionOf (h : body) = do
    (i, label) <- heading h
    let (role, tool) = roleAndTool label
        (blockLines, calls) = splitCalls body
        (thinking, content) = blocksOf blockLines
    pure (Section i role tool thinking content calls)

roleAndTool :: Text -> (Text, Maybe Text)
roleAndTool label
    | Just inner <- T.stripPrefix " (" rest
    , Just tool <- T.stripSuffix ")" inner =
        (role, Just tool)
    | otherwise = (label, Nothing)
  where
    (role, rest) = T.breakOn " (" label

thinkingMarker :: Text
thinkingMarker = "*thinking:*"

callsMarker :: Text
callsMarker = "**tool calls:**"

fence :: Text
fence = "```"

{- | Splits a section at its tool-call list, which the renderer always emits
last. The marker is only honoured when call lines follow it, so a block that
quotes the marker does not truncate the section.
-}
splitCalls :: [Text] -> ([Text], [EpisodeCall])
splitCalls body = case candidates of
    [] -> (body, [])
    (i : _) -> (take i body, mapMaybe callLine (drop (i + 1) body))
  where
    candidates =
        [ i
        | (i, l) <- reverse (zip [0 ..] body)
        , l == callsMarker
        , any (isJust . callLine) (drop (i + 1) body)
        ]

callLine :: Text -> Maybe EpisodeCall
callLine l = do
    rest <- T.stripPrefix "- `" l
    let (name, after) = T.breakOn "`" rest
    args <- T.stripPrefix "`" after
    if T.null name then Nothing else Just (EpisodeCall name (T.strip args))

{- | Reads the fenced blocks of a section: an optional thinking block and an
optional content block, in that order.
-}
blocksOf :: [Text] -> (Text, Text)
blocksOf = go False "" ""
  where
    go _ th ct [] = (th, ct)
    go inThink th ct (l : ls)
        | T.strip l == thinkingMarker = go True th ct ls
        | l == fence =
            let (blk, rest) = takeBlock ls
             in if inThink
                    then go False (join th blk) ct rest
                    else go False th (join ct blk) rest
        | otherwise = go inThink th ct ls
    join "" b = b
    join a b = a <> "\n" <> b

{- | Consumes a fenced block. A fence closes it only when what follows is
structural or no fence follows at all, so a fence inside the model's own
prose stays inside the block.
-}
takeBlock :: [Text] -> (Text, [Text])
takeBlock = go []
  where
    go acc [] = (T.intercalate "\n" (reverse acc), [])
    go acc (l : ls)
        | l == fence && closes ls = (T.intercalate "\n" (reverse acc), ls)
        | otherwise = go (l : acc) ls
    closes ls = structuralNext ls || notElem fence ls
    structuralNext ls = case dropWhile T.null ls of
        [] -> True
        (n : _) -> n == fence || T.strip n == thinkingMarker || n == callsMarker
