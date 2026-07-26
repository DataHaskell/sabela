{-# LANGUAGE OverloadedStrings #-}

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

data Allowlist = Allowlist
    { alEmails :: [Text]
    , alDomains :: [Text]
    }
    deriving (Eq, Show)

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

isAllowed :: Allowlist -> Text -> Bool
isAllowed al email =
    norm `elem` alEmails al || (T.isInfixOf "@" norm && domain `elem` alDomains al)
  where
    norm = normalizeEmail email
    domain = T.drop 1 (T.dropWhile (/= '@') norm)

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
