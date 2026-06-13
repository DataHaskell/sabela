{-# LANGUAGE OverloadedStrings #-}

{- | The signup allowlist. A line-based file (exact emails or @domain
entries, # comments) re-read on every login so grants never need a redeploy;
when the file is configured but unreadable the decision fails closed.
-}
module Hub.Allowlist (
    Allowlist (..),
    parseAllowlist,
    isAllowed,
    checkAllowed,
) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hPutStrLn, stderr)

import Hub.Types (normalizeEmail)

-- | Parsed entries: exact (normalized) emails and bare domains (no @).
data Allowlist = Allowlist
    { alEmails :: [Text]
    , alDomains :: [Text]
    }
    deriving (Eq, Show)

{- | One entry per line: @user\@example.com@ or @\@example.com@; blank lines
and @#@ comments ignored. Entries are stored normalized.
-}
parseAllowlist :: Text -> Allowlist
parseAllowlist t = Allowlist emails domains
  where
    entries =
        [ e
        | line <- T.lines t
        , let e = normalizeEmail (T.takeWhile (/= '#') line)
        , not (T.null e)
        ]
    domains = [d | e <- entries, "@" `T.isPrefixOf` e, let d = T.drop 1 e, not (T.null d)]
    emails = [e | e <- entries, not ("@" `T.isPrefixOf` e)]

{- | Exact email match, or exact match of the part after @\@@ against a
domain entry — never a suffix match (mallory\@evilexample.com must not pass
an \@example.com entry). An @\@@-less input matches nothing. Empty allowlist
denies everyone.
-}
isAllowed :: Allowlist -> Text -> Bool
isAllowed al email =
    norm `elem` alEmails al || (T.isInfixOf "@" norm && domain `elem` alDomains al)
  where
    norm = normalizeEmail email
    domain = T.drop 1 (T.dropWhile (/= '@') norm)

{- | The IO decision used by the callback: no file configured = open (dev
mode; a startup warning covers prod misconfiguration), configured = re-read
per login, fail closed (and log) on a read error so a typo'd path can't pass.
-}
checkAllowed :: Maybe FilePath -> Text -> IO Bool
checkAllowed Nothing _ = pure True
checkAllowed (Just path) email = do
    res <- try (TIO.readFile path) :: IO (Either SomeException Text)
    case res of
        Left e -> do
            hPutStrLn stderr $
                "[hub] allowlist unreadable (" <> show e <> "); denying login"
            pure False
        Right contents -> pure $ isAllowed (parseAllowlist contents) email
