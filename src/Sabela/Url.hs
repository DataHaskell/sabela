{-# LANGUAGE OverloadedStrings #-}

module Sabela.Url (
    rewriteGitHubUrl,
    rewriteDataUrl,
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

{- | Turn a link people paste for a data file into one that downloads it:
@s3://bucket/key@ to the bucket's public HTTPS endpoint, a Google Drive
share link to its download link, a Google Sheet to its CSV export.
-}
rewriteDataUrl :: Text -> Text
rewriteDataUrl url
    | Just rest <- T.stripPrefix "s3://" trimmed =
        let (bucket, key) = T.breakOn "/" rest
         in "https://" <> bucket <> ".s3.amazonaws.com" <> key
    | otherwise = case parseHostPath trimmed of
        Just ("drive.google.com", "file" : "d" : fileId : _) -> driveDownload fileId
        Just ("drive.google.com", ["open"]) | Just i <- queryParam "id" -> driveDownload i
        Just ("docs.google.com", "spreadsheets" : "d" : sheetId : _) ->
            "https://docs.google.com/spreadsheets/d/"
                <> sheetId
                <> "/export?format=csv"
                <> maybe "" ("&gid=" <>) (queryParam "gid")
        _ -> rewriteGitHubUrl trimmed
  where
    trimmed = T.strip url
    driveDownload i = "https://drive.google.com/uc?export=download&id=" <> i
    query = T.takeWhile (/= '#') (T.drop 1 (T.dropWhile (/= '?') trimmed))
    fragment = T.drop 1 (T.dropWhile (/= '#') trimmed)
    params = map (T.breakOn "=") (T.splitOn "&" query ++ T.splitOn "&" fragment)
    queryParam k = case [T.drop 1 v | (k', v) <- params, k' == k, not (T.null v)] of
        (v : _) -> Just v
        [] -> Nothing

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
