{-# LANGUAGE OverloadedStrings #-}

module Sabela.Url (
    rewriteGitHubUrl,
) where

import Data.Text (Text)
import qualified Data.Text as T

rewriteGitHubUrl :: Text -> Text
rewriteGitHubUrl url = case parseHostPath trimmed of
    Just ("github.com", a : b : "blob" : ref : p1 : rest) ->
        "https://raw.githubusercontent.com/"
            <> T.intercalate "/" (a : b : ref : p1 : rest)
    Just ("gist.github.com", segs)
        | length segs == 2 ->
            "https://gist.github.com/" <> T.intercalate "/" segs <> "/raw"
    _ -> trimmed
  where
    trimmed = T.strip url

parseHostPath :: Text -> Maybe (Text, [Text])
parseHostPath u =
    let noScheme = dropScheme u
        (host, rest) = T.breakOn "/" noScheme
        rawPath = T.drop 1 rest
        path = T.takeWhile (\c -> c /= '?' && c /= '#') rawPath
        segs = filter (not . T.null) (T.splitOn "/" path)
     in if T.null host then Nothing else Just (host, segs)

dropScheme :: Text -> Text
dropScheme u
    | "https://" `T.isPrefixOf` u = T.drop 8 u
    | "http://" `T.isPrefixOf` u = T.drop 7 u
    | otherwise = u
