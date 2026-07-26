{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.GoalText (
    holeQueryFor,
    sanitizeGoal,
    splitArrows,
) where

import Data.Char (isUpper)
import Data.Text (Text)
import qualified Data.Text as T

holeQueryFor :: Text -> Text
holeQueryFor goal = "_ :: " <> sanitizeGoal goal

sanitizeGoal :: Text -> Text
sanitizeGoal = T.unwords . map fixWord . T.words
  where
    fixWord w =
        let (pre, rest) = T.span (`elem` ("([" :: String)) w
            (core, suf) = T.break (`elem` (")]," :: String)) rest
         in pre <> fixCore core <> suf
    fixCore c
        | T.any (== ':') c = tyvar c
        | T.any (== '.') c, startsUpper c = tyvar c
        | otherwise = c
    startsUpper t = case T.uncons t of
        Just (ch, _) -> isUpper ch
        _ -> False
    tyvar c = T.toLower (lastPart "." (lastPart ":" c))
    lastPart sep t = case reverse (T.splitOn sep t) of
        (x : _) -> x
        [] -> ""

splitArrows :: Text -> [Text]
splitArrows t =
    map T.strip (splitOnTop "->" (0 :: Int) "" afterContext)
  where
    afterContext = case reverse (splitOnTop "=>" (0 :: Int) "" t) of
        (lastSeg : _) -> lastSeg
        [] -> t
    splitOnTop sep d acc s = case T.uncons s of
        Nothing -> [T.pack (reverse acc)]
        Just (c, rest)
            | d == 0 && sep `T.isPrefixOf` s ->
                T.pack (reverse acc) : splitOnTop sep d "" (T.drop 2 s)
            | c `elem` ("([" :: String) -> splitOnTop sep (d + 1) (c : acc) rest
            | c `elem` (")]" :: String) -> splitOnTop sep (d - 1) (c : acc) rest
            | otherwise -> splitOnTop sep d (c : acc) rest
