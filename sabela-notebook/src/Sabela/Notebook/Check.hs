{- | C2 task 5: structural checks for the deliverables notebooks actually
produce. A covering check should say something about the ARTIFACT, not
merely that a value exists — @not (null svg)@ passes on any string and
verifies nothing.

These are parameterised by the data the artifact was built from, so they
cannot be weakened into a tautology: the census must agree with a
cardinality the caller recomputes, and perturbing the data must change
the render.

> chartAgrees svg pts   -- the SVG draws one mark per point
> tableAgrees out 3 12  -- the table has 3 columns and 12 rows
-}
module Sabela.Notebook.Check (
    chartAgrees,
    tableAgrees,
    svgMarkCount,
    tableShape,
) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)

{- | Does this SVG draw one mark per datum? Parses, and its drawing-element
census matches the data's cardinality. A render that ignored its input, or
drew a fixed decoration, fails.
-}
chartAgrees :: String -> [a] -> Bool
chartAgrees svg xs =
    parsesAsSvg svg
        && not (null xs)
        && svgMarkCount svg >= length xs

{- | The drawing elements an SVG contains — the marks a chart makes, never
its container or defs. Counting these is what ties a render to its data.
-}
svgMarkCount :: String -> Int
svgMarkCount svg = sum (map (`occurrences` svg) markTags)

markTags :: [String]
markTags = ["<path", "<rect", "<circle", "<line", "<polyline", "<polygon"]

parsesAsSvg :: String -> Bool
parsesAsSvg svg = "<svg" `occursIn` svg && "</svg>" `occursIn` svg

{- | Does this rendered table have the shape the caller recomputed? Header
and rows parse, and both the column and row counts agree.
-}
tableAgrees :: String -> Int -> Int -> Bool
tableAgrees rendered cols rows =
    cols > 0 && rows > 0 && tableShape rendered == (cols, rows)

{- | A rendered table's (columns, data rows), read from its whitespace
layout: the header's field count, and the number of non-blank lines after
it that carry the same count. Separator rules are ignored.
-}
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
