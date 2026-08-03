{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.AI.HoogleClient (
    HoogleHit (..),
    breakTopLevel,
    declKeywords,
    hoogleDbArgSets,
    itemType,
    parseHoogleBlob,
    queryAllDbs,
    runHoogle,
    statesDeclaration,
) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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

{- | Hoogle emits UTF-8; encoding it as Latin-1 corrupts any result set
containing a single non-ASCII character and silently yields no hits at all.
-}
parseHoogleBlob :: Text -> [HoogleHit]
parseHoogleBlob blob =
    case A.decode (utf8 blob) of
        Just vs -> mapMaybe hitFromValue vs
        Nothing -> mapMaybe decodeLine (T.lines blob)
  where
    decodeLine l = A.decode (utf8 l) >>= hitFromValue
    utf8 = BL.fromStrict . TE.encodeUtf8

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
        (kw : rest) | kw `elem` declKeywords -> subjectOf (T.unwords rest)
        (w : _) -> T.takeWhile isItemChar w
        [] -> ""
    isItemChar c = c /= ' ' && c /= ':'
    subjectOf rest = case T.words (afterContext (declaredHead rest)) of
        (w : _) -> T.takeWhile isItemChar w
        [] -> ""
    declaredHead r = maybe r fst (breakDefiniens r)
    afterContext r = maybe r snd (breakTopLevel "=>" r)

{- | The keywords whose declaration names its subject after the keyword. The
listing a card claim is checked against reads its own lines by the same list,
so a declaration means one entity to the writer and to the witness.
-}
declKeywords :: [Text]
declKeywords = ["data", "type", "newtype", "class", "pattern"]

{- | Whether text is a whole declaration rather than a type. A reader that
prints @::@ before it announces a signature no source gave.
-}
statesDeclaration :: Text -> Bool
statesDeclaration t = any (\k -> (k <> " ") `T.isPrefixOf` t) declKeywords

{- | The type an index item states: the expansion its source gave, or the
signature its own head introduces, or nothing at all.
-}
itemType :: Text -> Text
itemType item = case expansion decl of
    Just d -> d
    Nothing -> fromMaybe "" (headSignature decl)
  where
    decl = T.unwords (T.words item)

{- | An item's whole declaration when the index expanded it. A declaration the
index left unexpanded has no right-hand side, so it states its own head and
never a bare name dressed as an expansion.
-}
expansion :: Text -> Maybe Text
expansion decl
    | statesDeclaration decl
    , Just (_, rhs) <- breakDefiniens decl
    , not (T.null (T.strip rhs)) =
        Just decl
    | otherwise = Nothing

{- | Split a declaration at the @=@ that defines it. The @=@ of a @=>@ opens a
context rather than a right-hand side, so a head constrained by one has no
definiens and states nothing.
-}
breakDefiniens :: Text -> Maybe (Text, Text)
breakDefiniens = go ""
  where
    go acc t = case breakTopLevel "=" t of
        Just (before, rest)
            | ">" `T.isPrefixOf` rest ->
                go (acc <> before <> "=>") (T.drop 1 rest)
            | otherwise -> Just (acc <> before, rest)
        Nothing -> Nothing

{- | The signature an item states: the text after a @::@ its head introduces. A
@::@ reached inside the head ascribes a kind to one of the head's binders, so
reading it would state a signature the index never gave.
-}
headSignature :: Text -> Maybe Text
headSignature decl = do
    (before, rhs) <- breakTopLevel "::" decl
    case dropKeyword (T.words before) of
        [_] -> Just (T.unwords (T.words rhs))
        _ -> Nothing
  where
    dropKeyword ws = case ws of
        (w : rest) | w `elem` declKeywords -> rest
        _ -> ws

-- | Split on the first occurrence of @sep@ that no bracket encloses.
breakTopLevel :: Text -> Text -> Maybe (Text, Text)
breakTopLevel sep = go (0 :: Int) ""
  where
    go d acc t
        | T.null t = Nothing
        | d == 0 && sep `T.isPrefixOf` t =
            Just (acc, T.drop (T.length sep) t)
        | otherwise = case T.uncons t of
            Just (c, rest) -> go (d + delta c) (T.snoc acc c) rest
            Nothing -> Nothing
    delta c
        | c `elem` ("([{" :: String) = 1
        | c `elem` (")]}" :: String) = -1
        | otherwise = 0

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
