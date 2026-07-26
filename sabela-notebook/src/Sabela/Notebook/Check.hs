module Sabela.Notebook.Check (
    chartAgrees,
    tableAgrees,
    svgMarkCount,
    tableShape,
) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)

chartAgrees :: String -> [a] -> Bool
chartAgrees svg xs =
    parsesAsSvg svg
        && not (null xs)
        && svgMarkCount svg >= length xs

svgMarkCount :: String -> Int
svgMarkCount svg = sum (map (`occurrences` svg) markTags)

markTags :: [String]
markTags = ["<path", "<rect", "<circle", "<line", "<polyline", "<polygon"]

parsesAsSvg :: String -> Bool
parsesAsSvg svg = "<svg" `occursIn` svg && "</svg>" `occursIn` svg

tableAgrees :: String -> Int -> Int -> Bool
tableAgrees rendered cols rows =
    cols > 0 && rows > 0 && tableShape rendered == (cols, rows)

tableShape :: String -> (Int, Int)
tableShape rendered = case dataLines of
    [] -> (0, 0)
    (h : rest) -> (length h, length (filter ((== length h) . length) rest))
  where
    dataLines = map words (filter (not . isRule) (lines rendered))
    isRule l = all (\c -> isSpace c || c `elem` ("-+|=" :: String)) l

occurrences :: String -> String -> Int
occurrences needle = go
  where
    go [] = 0
    go s@(_ : rest)
        | needle `isPrefixOf` s = 1 + go rest
        | otherwise = go rest

occursIn :: String -> String -> Bool
occursIn needle hay = occurrences needle hay > 0
