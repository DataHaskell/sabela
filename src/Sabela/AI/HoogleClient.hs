{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

data HoogleHit = HoogleHit
    { hhName :: Text
    , hhPackage :: Text
    , hhModule :: Text
    , hhType :: Text
    , hhDocs :: Text
    }
    deriving (Eq, Show)

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
    itemName item = case T.words (T.strip item) of
        (kw : rest : _) | kw `elem` declKeywords -> T.takeWhile isItemChar rest
        (w : _) -> T.takeWhile isItemChar w
        [] -> ""
    declKeywords = ["data", "type", "newtype", "class"]
    isItemChar c = c /= ' ' && c /= ':'

itemType :: Text -> Text
itemType item = case T.breakOn "::" item of
    (_, rest)
        | T.null rest -> ""
        | otherwise -> T.unwords (T.words (T.drop 2 rest))

stripHtml :: Text -> Text
stripHtml = T.unwords . T.words . go
  where
    go t = case T.breakOn "<" t of
        (before, rest)
            | T.null rest -> before
            | otherwise -> before <> go (T.drop 1 (T.dropWhile (/= '>') rest))

hoogleDbArgSets :: IO [[String]]
hoogleDbArgSets = do
    db <- lookupEnv "SABELA_HOOGLE_DB"
    localDb <- lookupEnv "SABELA_HOOGLE_LOCAL_DB"
    localOk <- maybe (pure False) doesFileExist localDb
    pure $
        maybe [] (\p -> ["--database=" ++ p]) db
            : [["--database=" ++ p] | localOk, Just p <- [localDb]]

queryAllDbs :: [String] -> IO [HoogleHit]
queryAllDbs args = do
    bin <- fromMaybe "hoogle" <$> lookupEnv "SABELA_HOOGLE_BIN"
    dbSets <- hoogleDbArgSets
    outs <- mapM (runHoogle bin . insertDb) dbSets
    pure (concatMap (maybe [] parseHoogleBlob) outs)
  where
    insertDb dbArg = init args ++ dbArg ++ [last args]

runHoogle :: FilePath -> [String] -> IO (Maybe Text)
runHoogle bin args = do
    r <- try (readProcessWithExitCode bin args "")
    pure $ case r of
        Left (_ :: SomeException) -> Nothing
        Right (ExitSuccess, out, _)
            | not (null out) -> Just (T.pack out)
        Right _ -> Nothing
