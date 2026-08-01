{-# LANGUAGE OverloadedStrings #-}

{- | Independent witnesses for what discover claims about the world: GHC's own
module listing and the package databases' module map, pointed at the databases
the harness resolves against. A witness that cannot see is not a witness.
-}
module Probe.Oracle (
    Oracle (..),
    browseExports,
    defaultPackageDbs,
    loadOracle,
    ownsModule,
) where

import Control.Monad (filterM)
import Data.Char (isSpace)
import Data.List (isPrefixOf, nub, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, getHomeDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

data Oracle = Oracle
    { orExposed :: M.Map Text [Text]
    , orDbs :: [FilePath]
    , orTrouble :: Maybe Text
    }

{- | The package databases a compiler on this machine would resolve against:
the ones it always reads, plus the store this GHC builds into. Discovered here
rather than asked of the harness, so the two can disagree.
-}
defaultPackageDbs :: IO [FilePath]
defaultPackageDbs = do
    override <- lookupEnv "SABELA_STORE_PACKAGE_DB"
    case override of
        Just p -> pure [p]
        Nothing -> do
            ver <- compilerVersion
            home <- getHomeDirectory
            let root = home </> ".cabal" </> "store"
            entries <- listing root
            filterM
                doesDirectoryExist
                [root </> e </> "package.db" | e <- entries, forVersion ver e]

forVersion :: String -> FilePath -> Bool
forVersion ver e = e == stem || (stem ++ "-") `isPrefixOf` e
  where
    stem = "ghc-" ++ ver

compilerVersion :: IO String
compilerVersion = do
    (_, out, _) <- readProcessWithExitCode "ghc" ["--numeric-version"] ""
    pure (takeWhile (not . isSpace) out)

listing :: FilePath -> IO [FilePath]
listing dir = do
    ok <- doesDirectoryExist dir
    if ok then sort <$> listDirectory dir else pure []

{- | The module map of every database given plus the ones @ghc-pkg@ always
reads; a re-export's @from …@ tail is stripped. A dump that fails or comes
back empty is recorded as trouble, never as "this package exposes nothing".
-}
loadOracle :: [FilePath] -> IO Oracle
loadOracle dbs = do
    dumps <- mapM dumpOf ([] : map pure dbs)
    let units = concatMap snd dumps
        broke = [d | (d, []) <- dumps]
    pure
        Oracle
            { orExposed = M.fromListWith (++) units
            , orDbs = dbs
            , orTrouble = trouble units broke
            }
  where
    trouble units broke
        | null units = Just "no package database listed a single package"
        | not (null broke) =
            Just ("a package database listed nothing: " <> T.pack (show broke))
        | otherwise = Nothing

{- | One @ghc-pkg dump@ per database. The flag replaces the stack rather than
adding to it, so the compiler's own databases are asked for separately or the
oracle goes blind to @base@.
-}
dumpOf :: [FilePath] -> IO ([FilePath], [(Text, [Text])])
dumpOf dbs = do
    (code, out, _) <- readProcessWithExitCode "ghc-pkg" args ""
    pure (dbs, if code == ExitSuccess then dumpUnits (T.pack out) else [])
  where
    args = map ("--package-db=" ++) dbs ++ ["dump"]

firstLine :: Text -> Text
firstLine t = case filter (not . T.null) (T.lines t) of
    (l : _) -> T.strip l
    [] -> "(no output)"

dumpUnits :: Text -> [(Text, [Text])]
dumpUnits dump =
    [ (nm, mods)
    | block <- T.splitOn "\n---\n" dump
    , let nm = fieldOf "name" block
    , not (T.null nm)
    , let mods = map stripFrom (T.words (fieldOf "exposed-modules" block))
    ]

stripFrom :: Text -> Text
stripFrom = T.takeWhile (/= ',')

{- | A field of a @ghc-pkg dump@ block with its indented continuation lines.
The dump pads values out to a column, so the words are taken rather than the
rest of the line: a name carrying its padding matches nothing.
-}
fieldOf :: Text -> Text -> Text
fieldOf k block = case break (T.isPrefixOf (k <> ":")) (T.lines block) of
    (_, l : rest) ->
        T.unwords
            (concatMap T.words (T.drop (T.length k + 1) l : takeWhile indented rest))
    _ -> ""
  where
    indented l = not (T.null l) && isSpace (T.head l)

{- | Whether the package database says this package exposes this module. A
package it has never heard of returns Nothing: an oracle that cannot see a
thing must not testify about it, and the caller must not read silence as yes.
-}
ownsModule :: Oracle -> Text -> Text -> Maybe Bool
ownsModule o pkg m = (m `elem`) <$> M.lookup pkg (orExposed o)

{- | What GHC says each module exports: one session per owning package, with
that package exposed and the same databases on the stack. A session that fails
or a listing that comes back empty is returned as the reason it failed.
-}
browseExports ::
    Oracle -> [(Text, Text)] -> IO (M.Map Text (Either Text [Text]))
browseExports oracle asked = do
    groups <- mapM (uncurry (browseGroup oracle)) (byPackage owners)
    pure (M.unions groups)
  where
    owners = [(m, owner m p) | (m, p) <- asked]
    owner m p
        | M.member p (orExposed oracle) = p
        | otherwise = fromMaybe p (packageExposing oracle m)

{- | Which package the database says exposes a module, for a card that named
none or named one the database has never heard of.
-}
packageExposing :: Oracle -> Text -> Maybe Text
packageExposing o m =
    headMay [p | (p, ms) <- M.toList (orExposed o), m `elem` ms]

byPackage :: [(Text, Text)] -> [(Text, [Text])]
byPackage asked =
    [ (p, nub [m | (m, p') <- asked, p' == p])
    | p <- nub (map snd asked)
    ]

browseGroup ::
    Oracle -> Text -> [Text] -> IO (M.Map Text (Either Text [Text]))
browseGroup _ _ [] = pure M.empty
browseGroup oracle pkg mods = do
    (code, out, err) <- readProcessWithExitCode "ghci" flags script
    pure $ case code of
        ExitSuccess -> M.fromList (map (listed (T.pack out)) mods)
        _ -> M.fromList [(m, Left (sessionFailed err)) | m <- mods]
  where
    flags =
        ["-v0", "-ignore-dot-ghci"]
            ++ concat [["-package-db", d] | d <- orDbs oracle]
            ++ concat [["-package", T.unpack pkg] | exposable]
    exposable = not (T.null pkg) && M.member pkg (orExposed oracle)
    script =
        T.unpack . T.unlines $
            concat [[":browse " <> m, "putStrLn " <> quoted (marker m)] | m <- mods]
    quoted t = "\"" <> t <> "\""
    sessionFailed e =
        "no session for " <> pkg <> ": " <> firstLine (T.pack e)
    listed out m = (m, verdict m (fromMaybe "" (M.lookup m (sections out))))
    verdict m chunk = case names chunk of
        [] -> Left ("browsed to an empty listing: " <> m)
        ns -> Right ns
    sections out = M.fromList (cut (T.lines out) mods)
    cut _ [] = []
    cut ls (m : ms) = case break (== marker m) ls of
        (before, _ : after) -> (m, T.unlines before) : cut after ms
        (_, []) -> [(m, "")]

marker :: Text -> Text
marker m = "<<<BROWSED " <> m <> ">>>"

{- | The names a @:browse@ listing declares: the head of each unindented
declaration, plus every binder a @::@ attaches to anywhere — a record selector
shows only inside its constructor's braces, on a continuation line.
-}
names :: Text -> [Text]
names chunk = filter (not . T.null) (heads ++ binders)
  where
    ls = T.lines chunk
    heads = [declared l | l <- ls, not (T.null l), not (isSpace (T.head l))]
    binders =
        [ unqualify (T.filter (`notElem` ("{},()" :: String)) w)
        | l <- ls
        , (w, "::") <- zip (T.words l) (drop 1 (T.words l))
        ]

declared :: Text -> Text
declared l = case T.words (T.replace "::" " :: " l) of
    (w : _)
        | w `elem` ["type", "data", "class", "newtype", "pattern"] ->
            fromMaybe "" (headMay (drop 1 (T.words l)))
        | otherwise -> unqualify (T.takeWhile (/= ',') w)
    [] -> ""

unqualify :: Text -> Text
unqualify n
    | T.null base = n
    | otherwise = base
  where
    base = T.takeWhileEnd (/= '.') n

headMay :: [a] -> Maybe a
headMay (x : _) = Just x
headMay [] = Nothing
