{-# LANGUAGE OverloadedStrings #-}

{- | What a compiler-reported position is a position in. GHCi numbers
@\<interactive\>@ lines across the whole session, so such a position is not a
line of the submitted cell; only a position under a cell's LINE-pragma tag is.
-}
module Sabela.Errors.Locate (
    Origin (..),
    originLine,
    originCol,
    attributeHeader,
    startsWithPosition,
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

import ScriptHs.Compiled (parseLinePragmaTag)

-- | A position, and the code it is a position in.
data Origin
    = CellPosition Int (Maybe Int)
    | SessionPosition Int (Maybe Int)
    deriving (Eq, Show)

-- | The cell line a position names, if it names one at all.
originLine :: Origin -> Maybe Int
originLine (CellPosition l _) = Just l
originLine SessionPosition{} = Nothing

originCol :: Origin -> Maybe Int
originCol (CellPosition _ c) = c
originCol SessionPosition{} = Nothing

{- | Reads a diagnostic header and rewrites it to name the code its position
belongs to. A session position keeps its numbers and loses the claim to be a
line of the cell.
-}
attributeHeader :: Text -> Maybe (Origin, Text)
attributeHeader hdr = do
    (origin, rest) <- readHeader hdr
    pure (origin, render origin <> rest)

render :: Origin -> Text
render (CellPosition l c) =
    "line " <> tshow l <> maybe "" (\x -> ", column " <> tshow x) c
render (SessionPosition l c) =
    "(ghci session position "
        <> tshow l
        <> maybe "" ((":" <>) . tshow) c
        <> ", not a line of this cell)"

{- | Whether a line opens a diagnostic, i.e. begins with a position this
module can read. Splitting a stderr blob into diagnostics uses it.
-}
startsWithPosition :: Text -> Bool
startsWithPosition hdr = case readHeader hdr of
    Just _ -> True
    Nothing -> False

{- | Splits a header into its position and everything after it. A file part
carrying a LINE-pragma tag is already cell-relative; @\<interactive\>@ is not,
and nothing here can map it back.
-}
readHeader :: Text -> Maybe (Origin, Text)
readHeader hdr = do
    (file, afterFile) <- splitOnColon hdr
    (ln, afterLine) <- decimalPrefix afterFile
    let (col, rest) = optionalColumn afterLine
    if isTagged file
        then Just (CellPosition ln col, rest)
        else do
            _ <- interactiveFile file
            Just (SessionPosition ln col, rest)

isTagged :: Text -> Bool
isTagged file = case parseLinePragmaTag file of
    Just _ -> True
    Nothing -> False

interactiveFile :: Text -> Maybe Text
interactiveFile file
    | file == "<interactive>" || file == "<cell>" = Just file
    | otherwise = Nothing

splitOnColon :: Text -> Maybe (Text, Text)
splitOnColon t = case T.breakOn ":" t of
    (before, rest) | not (T.null rest) -> Just (before, T.drop 1 rest)
    _ -> Nothing

decimalPrefix :: Text -> Maybe (Int, Text)
decimalPrefix t = either (const Nothing) Just (TR.decimal t)

optionalColumn :: Text -> (Maybe Int, Text)
optionalColumn t = case T.stripPrefix ":" t >>= decimalPrefix of
    Just (c, rest) -> (Just c, rest)
    Nothing -> (Nothing, t)

tshow :: Int -> Text
tshow = T.pack . show
