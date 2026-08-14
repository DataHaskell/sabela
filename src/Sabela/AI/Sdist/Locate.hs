{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{- | Finds a module's source file inside an sdist tarball, in memory, by
anchored path-suffix match: @Data.X.Y@ is @…/Data/X/Y.hs@ under whatever
source directory the package chose; no @.cabal@ parsing is needed or trusted.
-}
module Sabela.AI.Sdist.Locate (
    LocateMiss (..),
    locateModuleFile,
    moduleRelPaths,
    presentModules,
    decompressCapped,
    unlit,
) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as TarE
import qualified Codec.Compression.Zlib.Internal as Z
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isUpper)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)

data LocateMiss
    = BadArchive Text
    | NoSuchModule [Text]
    deriving (Eq, Show)

-- | Decompressed size past this reads as a bomb, not a package.
maxDecompressedBytes :: Int
maxDecompressedBytes = 64 * 1024 * 1024

-- | The relative paths a module name may live at.
moduleRelPaths :: Text -> [Text]
moduleRelPaths m =
    [T.replace "." "/" m <> ext | ext <- [".hs", ".lhs", ".hsc"]]

locateModuleFile ::
    BL.ByteString -> Text -> Either LocateMiss (FilePath, Text)
locateModuleFile bytes m = do
    files <- listFiles bytes
    let rels = moduleRelPaths m
        hits = [f | f <- files, any (anchoredSuffix (fst f)) rels]
    case sortOn (rank . fst) hits of
        ((path, body) : _) ->
            Right
                ( T.unpack path
                , if ".lhs" `T.isSuffixOf` path then unlit body else body
                )
        [] -> Left (NoSuchModule (presentModules (map fst files)))
  where
    rank p = (fromEnum (offMainPath p), T.length p)

-- | @…/Data/X.hs@ matches @Data/X.hs@ at a path boundary, never inside one.
anchoredSuffix :: Text -> Text -> Bool
anchoredSuffix path rel =
    path == rel || ("/" <> rel) `T.isSuffixOf` path

-- | A copy under a test or bench tree loses to the library's.
offMainPath :: Text -> Bool
offMainPath p =
    any (`elem` T.splitOn "/" p) ["test", "tests", "bench", "benchmarks"]

{- | The module names an sdist's source paths state, for a miss that should
name what is there instead. The name is the trailing run of capitalised path
segments, which is what the compiler requires of a module's file path.
-}
presentModules :: [Text] -> [Text]
presentModules paths =
    dedup
        [ name
        | p <- paths
        , ext <- [".hs", ".lhs", ".hsc"]
        , ext `T.isSuffixOf` p
        , let name = trailingModule (T.dropEnd (T.length ext) p)
        , not (T.null name)
        ]
  where
    trailingModule p =
        T.intercalate "."
            . reverse
            . takeWhile startsUpper
            . reverse
            $ T.splitOn "/" p
    startsUpper s = case T.uncons s of
        Just (c, _) -> isUpper c
        Nothing -> False
    dedup = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- | Every regular file in the archive, decoded leniently.
listFiles :: BL.ByteString -> Either LocateMiss [(Text, Text)]
listFiles bytes = do
    raw <- decompressCapped maxDecompressedBytes bytes
    walk (Tar.read raw)
  where
    walk = Tar.foldEntries keep (Right []) (Left . BadArchive . T.pack . show)
    keep e acc = case TarE.entryContent e of
        TarE.NormalFile body _ ->
            ((T.pack (TarE.entryPath e), decodeBody body) :)
                <$> acc
        _ -> acc
    decodeBody = TE.decodeUtf8With lenientDecode . BL.toStrict

{- | Pure gzip decompression that reports corruption as a value and stops
demanding output once the cap is passed.
-}
decompressCapped :: Int -> BL.ByteString -> Either LocateMiss BL.ByteString
decompressCapped cap input = fmap BL.fromChunks (go 0)
  where
    go =
        Z.foldDecompressStreamWithInput
            chunk
            end
            err
            (Z.decompressST Z.gzipFormat Z.defaultDecompressParams)
            input
    chunk c k n
        | n + BS.length c > cap = Left (BadArchive overCap)
        | otherwise = (c :) <$> k (n + BS.length c)
    end _ _ = Right []
    err e _ = Left (BadArchive (T.pack (show e)))
    overCap =
        "decompressed size exceeds the "
            <> T.pack (show cap)
            <> "-byte guard"

{- | Bird-track unlit: a @>@ line is code, anything else blanks, line count
preserved so spans stay truthful.
-}
unlit :: Text -> Text
unlit = T.unlines . map codeLine . T.lines
  where
    codeLine l
        | Just rest <- T.stripPrefix "> " l = rest
        | l == ">" = ""
        | Just rest <- T.stripPrefix ">" l = rest
        | otherwise = ""
