{-# LANGUAGE OverloadedStrings #-}

{- | What a repeat says beside the hits it hands back. A replay whose prose
re-reads the fields those hits already carry spends the caller's budget saying
twice what the payload says once.
-}
module Siza.Agent.Discover.Beside (
    besideHits,
    elision,
    restateFloor,
    summaryCap,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

{- | Cut each clause of the summary at the first hit field it restates, mark
the cut, and clamp. With no hit riding, the prose is the only carrier and
travels whole.
-}
besideHits :: [Value] -> Text -> Text
besideHits [] summary = summary
besideHits held summary = clampTo summaryCap (T.intercalate "; " kept)
  where
    kept = case filter (not . T.null) (map trim (clausesOf summary)) of
        [] -> [tShow (length held) <> " carried below"]
        cs -> cs
    trim c = case [i | f <- fields, Just i <- [indexOf f c]] of
        [] -> c
        is -> marked (T.dropWhileEnd (`elem` trailing) (T.take (minimum is) c))
    fields = concatMap restatedFields held
    trailing = " :-(,>=" :: String

{- | What a cut clause ends with, so a reader can tell prose that ended from
prose that was taken away.
-}
elision :: Text
elision = "…"

marked :: Text -> Text
marked t
    | T.null t = t
    | otherwise = t <> elision

summaryCap :: Int
summaryCap = 200

clampTo :: Int -> Text -> Text
clampTo n t
    | T.length t > n = T.take n t <> elision
    | otherwise = t

{- | The hit fields prose must not repeat. The name is not among them: it is
the question, and a summary that cannot say it says nothing. Nor is a one- or
two-character field, indistinguishable from prose it occurs inside.
-}
restatedFields :: Value -> [Text]
restatedFields h =
    [ t
    | k <- ["module", "type", "cabal", "use"]
    , let t = fieldOf k h
    , T.length t >= restateFloor
    ]

restateFloor :: Int
restateFloor = 3

clausesOf :: Text -> [Text]
clausesOf =
    filter (not . T.null)
        . map T.strip
        . concatMap (T.splitOn " — ")
        . T.splitOn "; "

{- | Where a clause restates a field: the first occurrence that stands as its
own token. A field name buried inside a longer identifier is a coincidence of
spelling, and cutting there deletes prose that repeats nothing.
-}
indexOf :: Text -> Text -> Maybe Int
indexOf needle = go 0
  where
    go off rest = case T.breakOn needle rest of
        (_, r) | T.null r -> Nothing
        (pre, r)
            | standsAlone (T.length pre) -> Just (off + T.length pre)
            | otherwise ->
                go
                    (off + T.length pre + 1)
                    (T.drop (T.length pre + 1) rest)
      where
        standsAlone i =
            leftClear (T.take i rest) && rightClear (T.drop (i + T.length needle) rest)
    leftClear before = case (T.unsnoc before, T.uncons needle) of
        (Just (_, c), Just (h, _)) -> not (tokenChar c && tokenChar h)
        _ -> True
    rightClear after = case (T.uncons after, T.unsnoc needle) of
        (Just (c, _), Just (_, l)) -> not (tokenChar c && tokenChar l)
        _ -> True

tokenChar :: Char -> Bool
tokenChar c = isAlphaNum c || c `elem` ("._'" :: String)

fieldOf :: Text -> Value -> Text
fieldOf k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
fieldOf _ _ = ""

tShow :: Int -> Text
tShow = T.pack . show
