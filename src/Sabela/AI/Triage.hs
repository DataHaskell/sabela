{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Triage (
    triageErrorText,
    triageResult,
) where

import Data.Char (isDigit)
import Data.Maybe (mapMaybe, maybeToList)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoleRepair (goalFromError)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (notInScopeName)
import Sabela.Parse (cellNames)

triageResult ::
    Text -> Either Text ExecutionResult -> Either Text ExecutionResult
triageResult _ res@(Left _) = res
triageResult src (Right er) =
    Right er{erError = triageErrorText src <$> erError er}

triageErrorText :: Text -> Text -> Text
triageErrorText src blob =
    T.intercalate "\n" (map renderChunk shown ++ moreNote ++ knockNote)
  where
    defs = fst (cellNames src)
    (roots, knock) =
        partitionChunks (knockOn defs) (map hiddenRewrite (chunksOf blob))
    deduped = dedupChunks roots
    (shown, extra) = splitAt chunkCap deduped
    knockNote =
        [ "(+"
            <> tshow (length knock)
            <> " knock-on errors for names this cell defines — they resolve when the errors above are fixed)"
        | not (null knock)
        ]
    moreNote =
        ["… and " <> tshow (length extra) <> " more errors" | not (null extra)]

chunkCap :: Int
chunkCap = 6

chunksOf :: Text -> [Text]
chunksOf blob = go (T.lines blob)
  where
    go [] = []
    go (l : ls) =
        let (cont, rest) = break isStart ls
         in T.intercalate "\n" (l : cont) : go rest
    isStart l = "cell " `T.isPrefixOf` l

knockOn :: S.Set Text -> Text -> Bool
knockOn defs chunk =
    not (null names) && all (`S.member` defs) names
  where
    names =
        mapMaybe notInScopeName (T.lines chunk)
            ++ map fst (maybeToList (goalFromError chunk))

hiddenRewrite :: Text -> Text
hiddenRewrite chunk
    | "hidden package" `T.isInfixOf` chunk =
        T.intercalate
            "\n"
            ( take 1 (T.lines chunk)
                ++ [ "Add as the FIRST line of the cell: -- cabal: build-depends: "
                        <> pkg
                   | pkg <- take 1 (pkgNames chunk)
                   ]
            )
    | otherwise = chunk
  where
    pkgNames t =
        [ stripVersion (T.takeWhile (/= '\'') rest)
        | (_, r) <- [T.breakOn "hidden package `" t]
        , not (T.null r)
        , let rest = T.drop (T.length "hidden package `") r
        ]
    stripVersion =
        T.dropWhileEnd (== '-') . T.dropWhileEnd (\c -> isDigit c || c == '.')

dedupChunks :: [Text] -> [(Text, Int)]
dedupChunks = go []
  where
    go _ [] = []
    go seen (c : cs)
        | key c `elem` seen = go seen cs
        | otherwise =
            (c, length [() | d <- cs, key d == key c]) : go (key c : seen) cs
    key = T.intercalate "\n" . mapFirst stripLoc . T.lines
    mapFirst f (l : ls) = f l : ls
    mapFirst _ [] = []
    stripLoc l
        | "cell " `T.isPrefixOf` l = T.strip (T.drop 2 (snd (T.breakOn ": " l)))
        | otherwise = l

renderChunk :: (Text, Int) -> Text
renderChunk (c, 0) = c
renderChunk (c, n) =
    c <> "\n  (same error at " <> tshow n <> " more site" <> plural n <> ")"
  where
    plural 1 = ""
    plural _ = "s"

partitionChunks :: (Text -> Bool) -> [Text] -> ([Text], [Text])
partitionChunks p xs = (filter (not . p) xs, filter p xs)

tshow :: Int -> Text
tshow = T.pack . show
