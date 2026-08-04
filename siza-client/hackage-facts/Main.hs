{-# LANGUAGE OverloadedStrings #-}

{- | Build the local Hackage facts cache: one row per package, carrying what
its latest @.cabal@ states. Run by tools/update-search-cache.sh, never at
query time — discover reads the file this writes.
-}
module Main (main) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as TarE
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BL
import Data.List (foldl', isSuffixOf, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)

import Siza.Agent.Discover.CabalFacts (
    PkgFacts (..),
    parseCabalFacts,
    renderFactsRow,
 )

data Args = Args
    { aTar :: Maybe FilePath
    , aOut :: FilePath
    }

defaultOut :: FilePath
defaultOut = "data" </> "hackage-facts.tsv"

parseArgs :: [String] -> Args
parseArgs = go (Args Nothing defaultOut)
  where
    go acc [] = acc
    go acc ("--tar" : p : r) = go acc{aTar = Just p} r
    go acc ("--out" : p : r) = go acc{aOut = p} r
    go acc (_ : r) = go acc r

main :: IO ()
main = do
    args <- parseArgs <$> getArgs
    tarPath <- maybe defaultTar pure (aTar args)
    ok <- doesFileExist tarPath
    unless ok $ do
        logErr ("no Hackage index at " <> tarPath <> " — run cabal update")
        exitFailure
    logErr ("==> reading package facts from " <> tarPath)
    rows <- latestFacts tarPath
    createDirectoryIfMissing True (takeDirectory (aOut args))
    TIO.writeFile (aOut args) (T.unlines [renderFactsRow n f | (n, f) <- rows])
    logErr
        ( "   wrote "
            <> show (length rows)
            <> " packages, "
            <> show (sum [length (pfModules f) | (_, f) <- rows])
            <> " modules -> "
            <> aOut args
        )

defaultTar :: IO FilePath
defaultTar = do
    fromEnv <- lookupEnv "CABAL_INDEX_TAR"
    case fromEnv of
        Just p -> pure p
        Nothing -> do
            home <- getHomeDirectory
            pure (home </> ".cabal/packages/hackage.haskell.org/01-index.tar")

{- | Every package's latest version, with the facts that version states. A
later entry for the same version is a revision of it, so it wins.
-}
latestFacts :: FilePath -> IO [(Text, PkgFacts)]
latestFacts tarPath = do
    entries <- readEntries tarPath
    let m = foldl' step M.empty entries
    pure [(name, facts) | (name, (_, facts)) <- sortOn fst (M.toList m)]
  where
    step acc e = case cabalParts e of
        Nothing -> acc
        Just (pkg, ver) ->
            let v = parseVer ver
             in case M.lookup pkg acc of
                    Just (cur, _) | v < cur -> acc
                    _ -> M.insert pkg (v, parseCabalFacts (entryText e)) acc

-- | Read all entries; a trailing format error truncates the list.
readEntries :: FilePath -> IO [Tar.Entry]
readEntries p = Tar.foldEntries (:) [] (const []) . Tar.read <$> BL.readFile p

-- | @pkg/ver/pkg.cabal@ -> @(pkg, ver)@; anything else is not a package file.
cabalParts :: Tar.Entry -> Maybe (Text, Text)
cabalParts e = case splitSlash (TarE.entryPath e) of
    [a, b, c] | ".cabal" `isSuffixOf` c -> Just (T.pack a, T.pack b)
    _ -> Nothing

entryText :: Tar.Entry -> Text
entryText e = case TarE.entryContent e of
    TarE.NormalFile bs _ -> TE.decodeUtf8With TEE.lenientDecode (BL.toStrict bs)
    _ -> ""

parseVer :: Text -> [Int]
parseVer = map (fromMaybe 0 . readMaybeInt) . T.splitOn "."
  where
    readMaybeInt t = case reads (T.unpack t) of
        [(n, "")] -> Just n
        _ -> Nothing

splitSlash :: FilePath -> [String]
splitSlash = foldr f [[]]
  where
    f '/' acc = [] : acc
    f c (cur : rest) = (c : cur) : rest
    f _ [] = [[]]

logErr :: String -> IO ()
logErr = hPutStrLn stderr
