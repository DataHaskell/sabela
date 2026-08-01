{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.PathRepair (
    notFoundPath,
    pathNearMissFix,
    pathNotFoundGuidance,
) where

import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.PathResolve (
    PathLookup (..),
    isUrl,
    lookupPath,
    workDirFiles,
 )
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (Guidance (..))

notFoundPath :: Text -> Maybe FilePath
notFoundPath err = listToMaybe (mapMaybe fromLine (T.lines err))
  where
    fromLine raw = case T.splitOn ": " (stripExceptionPrefix (T.strip raw)) of
        (p : loc : reason : _)
            | "does not exist" `T.isPrefixOf` reason
            , not (T.null p)
            , pathLike p
            , ioLocationLike loc ->
                Just (T.unpack p)
        _ -> Nothing
    stripExceptionPrefix t =
        fromMaybe t (T.stripPrefix "*** Exception: " t)

pathLike :: Text -> Bool
pathLike = T.all (`notElem` ("(){}\"[]," :: String))

ioLocationLike :: Text -> Bool
ioLocationLike loc = not (T.null loc) && not (T.any (== ' ') loc)

pathNearMissFix ::
    FilePath -> Either Text ExecutionResult -> Text -> IO (Maybe Text)
pathNearMissFix workDir res src = case notFoundPath =<< runtimeErrorOf res of
    Nothing -> pure Nothing
    Just wrong -> do
        files <- workDirFiles workDir
        pure $ case lookupPath wrong files of
            Unique right ->
                let src' = T.replace (T.pack wrong) ("./" <> T.pack right) src
                 in if src' == src then Nothing else Just src'
            _ -> Nothing

pathNotFoundGuidance ::
    FilePath -> Either Text ExecutionResult -> IO (Maybe Guidance)
pathNotFoundGuidance workDir res = case notFoundPath =<< runtimeErrorOf res of
    Nothing -> pure Nothing
    Just wrong
        | isUrl wrong -> pure (Just (Guidance "url-as-path" (urlMessage wrong)))
        | otherwise -> do
            files <- workDirFiles workDir
            pure $ case lookupPath wrong files of
                Unique _ -> Nothing
                Nearby cs -> Just (Guidance "file-not-found" (candidateMessage wrong cs))
                NoneNearby -> Just (Guidance "file-not-found" (noneMessage wrong))

urlMessage :: FilePath -> Text
urlMessage wrong =
    "`"
        <> T.pack wrong
        <> "` is a URL, not a filesystem path. The file-opening functions \
           \(readFile and its relatives) resolve local paths only; nothing \
           \was fetched."

candidateMessage :: FilePath -> [FilePath] -> Text
candidateMessage wrong cs =
    "The path `"
        <> T.pack wrong
        <> "` does not exist. Nearest files under the work dir: "
        <> T.intercalate ", " (map (tick . T.pack) cs)
        <> ". Ask the user which one they meant, or use the exact path."
  where
    tick t = "`" <> t <> "`"

noneMessage :: FilePath -> Text
noneMessage wrong =
    "The path `"
        <> T.pack wrong
        <> "` does not exist and no similar file was found under the work \
           \dir. Ask the user for the correct path."

runtimeErrorOf :: Either Text ExecutionResult -> Maybe Text
runtimeErrorOf (Left e) = Just e
runtimeErrorOf (Right er) = erError er
