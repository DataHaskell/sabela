{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The local @hoogle@ CLI client: the hit type, the JSON blob parser, and
the database fan-out — every query consults the configured DB AND the
whole-installed-DB index @update-search-cache.sh --local@ writes, in union,
so a hidden locally-installed package's symbols are never invisible (R3.1).
Split from "Sabela.AI.HoogleResolve" for the module-size cap.
-}
module Sabela.AI.HoogleClient (
    HoogleHit (..),
    hoogleDbArgSets,
    parseHoogleBlob,
    queryAllDbs,
    runHoogle,
) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

{- | One hoogle result: the named item plus the package and module it lives in,
and (for capability search) its type signature and a stripped docs blurb. The
type is the @item@ text after @::@; @hhDocs@ is the @docs@ field with HTML tags
removed. Name-resolution callers ignore the last two fields.
-}
data HoogleHit = HoogleHit
    { hhName :: Text
    , hhPackage :: Text
    , hhModule :: Text
    , hhType :: Text
    , hhDocs :: Text
    }
    deriving (Eq, Show)

{- | Parse the local @hoogle search --json@ blob into hits. Accepts both a single
JSON array and JSONL (one object per line); a hoogle error line or malformed JSON
yields no hits. The leading identifier of each @item@ signature is the name.
-}
parseHoogleBlob :: Text -> [HoogleHit]
parseHoogleBlob blob =
    case A.decode (BL.pack (T.unpack blob)) of
        Just vs -> mapMaybe hitFromValue vs
        Nothing -> mapMaybe (decodeLine . T.unpack) (T.lines blob)
  where
    decodeLine l = A.decode (BL.pack l) >>= hitFromValue

hitFromValue :: A.Value -> Maybe HoogleHit
hitFromValue = A.parseMaybe $ \v -> A.withObject "hit" parse v
  where
    parse o = do
        item <- o A..: "item"
        mModName <- nameIn o "module"
        mPkgName <- nameIn o "package"
        docs <- o A..:? "docs" A..!= ""
        let d = stripHtml docs
            pkg = fromMaybe "" mPkgName
        -- Package and module rows carry null module/package objects; they
        -- are answers, not parse failures — a discarded tier is a false
        -- absence for a query that names the package (R1.1).
        pure $ case T.words (T.strip item) of
            ["package", p] -> HoogleHit p p "" "" d
            ["module", m] -> HoogleHit m pkg m "" d
            _ ->
                HoogleHit
                    (itemName item)
                    pkg
                    (fromMaybe "" mModName)
                    (itemType item)
                    d
    nameIn o k = do
        mObj <- o A..:? k
        case mObj of
            Just (A.Object oo) -> oo A..:? "name"
            _ -> pure Nothing
    itemName = T.takeWhile isItemChar . T.strip
    isItemChar c = c /= ' ' && c /= ':'

{- | The type signature in an @item@: the text after the first @::@, whitespace
collapsed. Empty when the item has no signature (a type/class/module hit).
-}
itemType :: Text -> Text
itemType item = case T.breakOn "::" item of
    (_, rest)
        | T.null rest -> ""
        | otherwise -> T.unwords (T.words (T.drop 2 rest))

{- | Strip the @\<a\>@/@\<i\>@/@\<tt\>@ markup hoogle wraps docs in, and collapse
whitespace, leaving a plain-text blurb.
-}
stripHtml :: Text -> Text
stripHtml = T.unwords . T.words . go
  where
    go t = case T.breakOn "<" t of
        (before, rest)
            | T.null rest -> before
            | otherwise -> before <> go (T.drop 1 (T.dropWhile (/= '>') rest))

{- | The database argument sets one query consults IN UNION: the configured
DB (@SABELA_HOOGLE_DB@ or hoogle's default), plus the whole-installed-DB
index @update-search-cache.sh --local@ writes (@SABELA_HOOGLE_LOCAL_DB@) —
so a hidden locally-installed package's symbols are never invisible (R3.1).
-}
hoogleDbArgSets :: IO [[String]]
hoogleDbArgSets = do
    db <- lookupEnv "SABELA_HOOGLE_DB"
    localDb <- lookupEnv "SABELA_HOOGLE_LOCAL_DB"
    localOk <- maybe (pure False) doesFileExist localDb
    pure $
        maybe [] (\p -> ["--database=" ++ p]) db
            : [["--database=" ++ p] | localOk, Just p <- [localDb]]

-- | Run one hoogle query against every DB arg set and union the parses.
queryAllDbs :: [String] -> IO [HoogleHit]
queryAllDbs args = do
    bin <- fromMaybe "hoogle" <$> lookupEnv "SABELA_HOOGLE_BIN"
    dbSets <- hoogleDbArgSets
    outs <- mapM (runHoogle bin . insertDb) dbSets
    pure (concatMap (maybe [] parseHoogleBlob) outs)
  where
    insertDb dbArg = init args ++ dbArg ++ [last args]

-- | Shell out to the hoogle binary; Nothing on any failure, never a throw.
runHoogle :: FilePath -> [String] -> IO (Maybe Text)
runHoogle bin args = do
    r <- try (readProcessWithExitCode bin args "")
    pure $ case r of
        Left (_ :: SomeException) -> Nothing
        Right (ExitSuccess, out, _)
            | not (null out) -> Just (T.pack out)
        Right _ -> Nothing
