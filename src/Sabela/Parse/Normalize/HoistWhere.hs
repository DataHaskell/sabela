{-# LANGUAGE OverloadedStrings #-}

module Sabela.Parse.Normalize.HoistWhere (hoistMainWhere) where

import Control.Monad (guard)
import Data.Char (isAlphaNum, isLower)
import Data.List (findIndex)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

hoistMainWhere :: [Text] -> [Text]
hoistMainWhere ls = fromMaybe ls $ do
    (before, mainLine, body, after) <- splitAtMain ls
    body' <- hoistBody body
    pure (before ++ mainLine : body' ++ after)

splitAtMain :: [Text] -> Maybe ([Text], Text, [Text], [Text])
splitAtMain ls = case break isMainEq ls of
    (before, m : rest) ->
        let (body, after) = break isTopLevel rest
         in Just (before, m, body, after)
    _ -> Nothing
  where
    isMainEq l =
        isTopLevel l
            && firstWord l == Just "main"
            && "=" `T.isInfixOf` l

hoistBody :: [Text] -> Maybe [Text]
hoistBody body = do
    let (stmtLines, rest) = break ((== "where") . T.strip) body
    whereLine : ws <- pure rest
    let wInd = indentOf whereLine
        (wLines, trailing) = span (\l -> isBlank l || indentOf l > wInd) ws
    guard (all isBlank trailing)
    (base, binds) <- bindingGroups wLines
    (sInd, stmts) <- statementGroups stmtLines
    pure (concat (interleave sInd base binds stmts))

bindingGroups :: [Text] -> Maybe (Int, [(Text, [Text])])
bindingGroups wLines = do
    base <- indentOf <$> firstNonBlank wLines
    guard (all (\l -> isBlank l || indentOf l >= base) wLines)
    raw <- chunkAt base wLines
    named <- traverse nameOf raw
    pure (base, mergeSameName named)
  where
    nameOf g = do
        l <- listToMaybe g
        w <- firstWord l
        guard (isIdent w)
        pure (w, g)

statementGroups :: [Text] -> Maybe (Int, [[Text]])
statementGroups stmtLines = do
    sInd <- indentOf <$> firstNonBlank stmtLines
    guard (all (\l -> isBlank l || indentOf l >= sInd) stmtLines)
    groups <- chunkAt sInd stmtLines
    pure (sInd, groups)

chunkAt :: Int -> [Text] -> Maybe [[Text]]
chunkAt base = go . dropWhile isBlank
  where
    go [] = Just []
    go (l : rest)
        | startsAt l =
            let (cont, more) = break startsAt rest
             in ((l : cont) :) <$> go more
        | otherwise = Nothing
    startsAt l = not (isBlank l) && indentOf l == base

mergeSameName :: [(Text, [Text])] -> [(Text, [Text])]
mergeSameName ((a, xs) : (b, ys) : rest)
    | a == b = mergeSameName ((a, xs ++ ys) : rest)
mergeSameName (g : rest) = g : mergeSameName rest
mergeSameName [] = []

-- | Emit each statement group prefixed by the let blocks placed above it.
interleave :: Int -> Int -> [(Text, [Text])] -> [[Text]] -> [[Text]]
interleave sInd base binds stmts =
    [ concat [letBlock sInd base g | (at, g) <- placed, at == i] ++ stmt
    | (i, stmt) <- zip [0 :: Int ..] stmts
    ]
  where
    placed = placements binds stmts

placements :: [(Text, [Text])] -> [[Text]] -> [(Int, [Text])]
placements binds stmts
    | crossRefs = [(groupAt, concatMap snd binds) | not (null binds)]
    | otherwise = [(fromMaybe 0 (firstUse n), g) | (n, g) <- binds]
  where
    names = map fst binds
    crossRefs =
        or
            [ any (usesName m) g
            | (n, g) <- binds
            , m <- names
            , m /= n
            ]
    firstUse n = findIndex (any (usesName n)) stmts
    groupAt = case mapMaybe firstUse names of
        [] -> 0
        idxs -> minimum idxs

letBlock :: Int -> Int -> [Text] -> [Text]
letBlock sInd base gls = case gls of
    (l : rest) -> (indent sInd <> "let " <> T.drop base l) : map cont rest
    [] -> []
  where
    cont l
        | isBlank l = ""
        | otherwise = indent (sInd + 4) <> T.drop base l
    indent n = T.replicate n " "

usesName :: Text -> Text -> Bool
usesName n l = n `elem` identTokens l

identTokens :: Text -> [Text]
identTokens = filter (not . T.null) . T.split (not . isIdentChar)

isIdent :: Text -> Bool
isIdent w = case T.uncons w of
    Just (c, _) -> (isLower c || c == '_') && T.all isIdentChar w
    Nothing -> False

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

firstWord :: Text -> Maybe Text
firstWord = listToMaybe . T.words

firstNonBlank :: [Text] -> Maybe Text
firstNonBlank = listToMaybe . dropWhile isBlank

isTopLevel :: Text -> Bool
isTopLevel l = case T.uncons l of
    Just (c, _) -> c /= ' ' && c /= '\t'
    Nothing -> False

indentOf :: Text -> Int
indentOf = T.length . T.takeWhile (== ' ')

isBlank :: Text -> Bool
isBlank = T.null . T.strip
