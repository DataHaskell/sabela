{-# LANGUAGE OverloadedStrings #-}

{- | The @read_source@ argument grammar, shared by the server executor and
the client's advice-legality property, so a call the tool names is a call
the tool accepts.
-}
module Sabela.AI.ReadSourceArgs (
    ReadSourceReq (..),
    parseReadSourceArgs,
    readSourceCallText,
    validModuleName,
    validPackageName,
    validVersionText,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAlphaNum, isDigit, isUpper)
import Data.Text (Text)
import qualified Data.Text as T

data ReadSourceReq = ReadSourceReq
    { rsModule :: Text
    , rsName :: Maybe Text
    , rsPackage :: Maybe Text
    , rsVersion :: Maybe Text
    }
    deriving (Eq, Show)

parseReadSourceArgs :: Value -> Either Text ReadSourceReq
parseReadSourceArgs v = do
    o <- case v of
        Object o -> Right o
        _ -> Left "read_source takes an object of arguments"
    m <- required o "module"
    if not (validModuleName m)
        then Left badModule
        else do
            pkg <- checked o "package" validPackageName badPackage
            ver <- checked o "version" validVersionText badVersion
            pure (ReadSourceReq m (optional o "name") pkg ver)
  where
    required o k = case optional o k of
        Just t -> Right t
        Nothing -> Left ("`" <> k <> "` required")
    checked o k valid err = case optional o k of
        Nothing -> Right Nothing
        Just t
            | valid t -> Right (Just t)
            | otherwise -> Left err
    badModule =
        "`module` must be a dotted module name like \"Data.Time.Clock\""
    badPackage =
        "`package` must be a Hackage package name (letters, digits, hyphens)"
    badVersion = "`version` must be dotted digits like \"0.2.2.1\""

{- | The paste-ready call text every hint mints, beside the parser so a
minted call and the accepted grammar cannot drift apart.
-}
readSourceCallText :: [(Text, Text)] -> Text
readSourceCallText args =
    "read_source {"
        <> T.intercalate ", " [k <> ": \"" <> v <> "\"" | (k, v) <- args]
        <> "}"

-- | A stated string argument; empty is unstated, not illegal.
optional :: KM.KeyMap Value -> Text -> Maybe Text
optional o k = case KM.lookup (Key.fromText k) o of
    Just (String t) | not (T.null (T.strip t)) -> Just (T.strip t)
    _ -> Nothing

-- | Dotted conid segments, each opening uppercase.
validModuleName :: Text -> Bool
validModuleName t =
    not (T.null t) && all segment (T.splitOn "." t)
  where
    segment s = case T.uncons s of
        Just (c, _) -> isUpper c && T.all conChar s
        Nothing -> False
    conChar c = isAlphaNum c || c == '_' || c == '\''

-- | The Hackage name shape: hyphenated alphanumeric segments, nothing else.
validPackageName :: Text -> Bool
validPackageName t =
    not (T.null t) && all segment (T.splitOn "-" t)
  where
    segment s = not (T.null s) && T.all isAlphaNum s

validVersionText :: Text -> Bool
validVersionText t =
    not (T.null t) && all segment (T.splitOn "." t)
  where
    segment s = not (T.null s) && T.all isDigit s
