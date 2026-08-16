{-# LANGUAGE OverloadedStrings #-}

{- | The canonical environment identity: one spelling per environment, so
whitespace or ordering variants of the same metadata share one cache bucket,
while anything GHC treats as different (option order, versions) splits.
-}
module Sabela.Session.EnvKey (
    canonicalDep,
    canonicalKeyText,
    envBucketName,
    resolveLocalPackages,
) where

import Data.Char (isAlphaNum)
import Data.Hashable (hashWithSalt)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)
import System.FilePath (isAbsolute, (</>))

import ScriptHs.Parser (CabalMeta (..), SourceRepoPin (..))

{- | One spelling per dependency entry: the name, then the constraint split
into operator and atom tokens joined by single spaces. Tokenising (not
whitespace deletion) keeps @>=@ distinct from @> =@ while making @==0.6.7@
and @== 0.6.7@ one spelling. Total and idempotent.
-}
canonicalDep :: Text -> Text
canonicalDep entry = T.unwords (name : constraintTokens rest)
  where
    trimmed = T.strip entry
    (name, rest) = T.span (\c -> isAlphaNum c || c == '-') trimmed

constraintTokens :: Text -> [Text]
constraintTokens rest =
    concatMap (concatMap splitRun . T.groupBy sameClass) (T.words rest)
  where
    sameClass a b = charClass a == charClass b
    charClass :: Char -> Int
    charClass c
        | c `elem` ("=<>&|^~!" :: String) = 0
        | isAlphaNum c || c `elem` (".-*" :: String) = 1
        | otherwise = 2
    splitRun run
        | maybe False ((== 0) . charClass . fst) (T.uncons run) = splitOps run
        | otherwise = [run]

-- | Greedy longest-known-operator split, so @&&<@ reads as @&& <@.
splitOps :: Text -> [Text]
splitOps t
    | T.null t = []
    | otherwise = case filter (`T.isPrefixOf` t) knownOps of
        (op : _) -> op : splitOps (T.drop (T.length op) t)
        [] -> T.take 1 t : splitOps (T.drop 1 t)
  where
    knownOps =
        ["^>=", ">=", "<=", "==", "&&", "||", ">", "<", "=", "^", "~", "!", "|", "&"]

{- | Netstring framing: each entry escaped and length-prefixed, so no entry
content (commas, @\@@, newlines) can alias a different entry list.
-}
frameEntries :: [Text] -> Text
frameEntries = T.intercalate "," . map frame
  where
    frame e =
        let esc = T.replace "\n" "\\n" (T.replace "\\" "\\\\" e)
         in T.pack (show (T.length esc)) <> "#" <> esc

{- | The full canonical key text. Unordered fields (deps, extensions, local
packages, repos) sort and dedupe; ordered fields (ghc-options, search dirs)
keep order and repetition, which GHC honours.
-}
canonicalKeyText :: [FilePath] -> CabalMeta -> Text -> Text
canonicalKeyText localPkgs meta ghcVersion =
    T.intercalate
        "\n"
        [ unordered "deps" (map canonicalDep (metaDeps meta))
        , unordered "exts" (metaExts meta)
        , ordered "opts" (metaGhcOptions meta)
        , ordered "libdirs" (metaExtraLibDirs meta)
        , ordered "incdirs" (metaExtraIncludeDirs meta)
        , unordered "pkgs" (map T.pack localPkgs)
        , unordered "rawpkgs" (metaPackages meta)
        , unordered "repos" (map repoText (metaSourceRepos meta))
        , "ghc:" <> ghcVersion
        , "schema:2"
        ]
  where
    unordered label xs =
        label <> ":" <> frameEntries (S.toAscList (S.fromList (clean xs)))
    ordered label xs = label <> ":" <> frameEntries (clean xs)
    clean = filter (not . T.null) . map T.strip
    repoText r =
        frameEntries ([srpLocation r, srpRef r] <> maybe [] pure (srpSubdir r))

{- | The bucket a canonical key lives in: a 128-bit double hash (no
cryptographic hash exists in the dependency closure). A collision is not a
correctness hazard — the bucket's key.txt is re-validated on every acquire —
only a serialization nuisance between the colliding keys.
-}
envBucketName :: Text -> String
envBucketName keyText = "env-" <> hex 1 <> hex 2
  where
    hex salt =
        showHex (fromIntegral (hashWithSalt salt (T.unpack keyText)) :: Word) ""

{- | The full local-package overlay a generated project sees: the server's
configured packages plus the metadata's own, relative paths resolved against
the work dir. This list is part of the environment's identity.
-}
resolveLocalPackages :: FilePath -> [FilePath] -> CabalMeta -> [FilePath]
resolveLocalPackages workDir envLocals meta =
    stableNub (envLocals <> map resolve (metaPackages meta))
  where
    resolve raw =
        let path = T.unpack raw
         in if isAbsolute path then path else workDir </> path

stableNub :: (Ord a) => [a] -> [a]
stableNub = go S.empty
  where
    go _ [] = []
    go seen (x : xs)
        | x `S.member` seen = go seen xs
        | otherwise = x : go (S.insert x seen) xs
