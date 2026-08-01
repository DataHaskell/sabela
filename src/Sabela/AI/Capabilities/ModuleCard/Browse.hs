{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Running @:browse@ against the store package db, and deciding whether what
came back is a listing at all. GHC exits zero on a bad module and prints the
diagnostic to stderr, so the exit code is not evidence; the content is.
-}
module Sabela.AI.Capabilities.ModuleCard.Browse (
    browseFailed,
    diagnosticIn,
    browseOutcome,
    browseHidden,
) where

import Control.Exception (SomeException, try)
import Data.Char (isDigit)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

{- | Why this @:browse@ output is not a listing, if it is not one. Silence
counts: a probe that said nothing enumerated nothing, and the caller must not
read that as a module without exports.
-}
browseFailed :: Text -> Maybe Text
browseFailed raw
    | T.null (T.strip raw) = Just "no output from :browse"
    | otherwise = diagnosticIn raw

{- | The diagnostic a stream carries, head line and the prose under it. A GHC
header line is a code and a location, and says nothing on its own.
-}
diagnosticIn :: Text -> Maybe Text
diagnosticIn raw = case break diagnosticLine (map T.strip (T.lines raw)) of
    (_, ls@(_ : _)) -> Just (clamp (T.unwords (take 2 (filter (not . T.null) ls))))
    _ -> Nothing
  where
    clamp t
        | T.length t <= reasonChars = t
        | otherwise = T.take reasonChars t <> "…"

reasonChars :: Int
reasonChars = 160

diagnosticLine :: Text -> Bool
diagnosticLine l =
    "error:" `T.isInfixOf` low
        || "not in scope" `T.isInfixOf` low
        || hasErrorCode l
  where
    low = T.toLower l

{- | GHC stamps every diagnostic with a bracketed code, @[GHC-45102]@. The shape
survives rewording and translation, where the word @error@ does not.
-}
hasErrorCode :: Text -> Bool
hasErrorCode l = case T.breakOn marker l of
    (_, r)
        | T.null r -> False
        | otherwise ->
            let digits = T.takeWhile isDigit (T.drop (T.length marker) r)
             in not (T.null digits)
                    && "]"
                        `T.isPrefixOf` T.drop (T.length marker + T.length digits) r
  where
    marker = "[GHC-"

{- | What the two output streams of one probe amount to. A listing on stdout is
the answer; otherwise the reason is whichever stream carries a diagnostic.
-}
browseOutcome :: Text -> Text -> Either Text Text
browseOutcome out err = case browseFailed out of
    Nothing -> Right out
    Just silence -> Left (fromMaybe silence (diagnosticIn err))

browseHidden :: FilePath -> Text -> Text -> IO (Either Text Text)
browseHidden db pkg modName = do
    r <-
        try
            ( readProcessWithExitCode
                "ghci"
                [ "-v0"
                , "-package-db=" ++ db
                , "-package"
                , T.unpack pkg
                ]
                (":browse " <> T.unpack modName <> "\n")
            )
    pure $ case r of
        Left (e :: SomeException) -> Left (firstLine (T.pack (show e)))
        Right (ExitSuccess, out, err) -> browseOutcome (T.pack out) (T.pack err)
        Right (_, out, err) ->
            Left
                ( fromMaybe
                    "ghci exited non-zero"
                    (diagnosticIn (T.pack err <> "\n" <> T.pack out))
                )
  where
    firstLine =
        fromMaybe "ghci could not be run"
            . find (not . T.null)
            . map T.strip
            . T.lines
