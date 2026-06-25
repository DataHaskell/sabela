{-# LANGUAGE OverloadedStrings #-}

{- | The mechanical package knowledge the diagnostic layer uses to turn "could
not find module M" into "add package P", and to repair an invented package
token (@df-core@) into a real one (@dataframe-core@). Held in code, not in a
prompt, so it covers any model and any client.

Two resolvers with different keys:

* 'packageForModule' / 'table' — keyed on module prefixes. Ordered
  longest-prefix-first so @DataFrame.Display@ resolves to the plotting umbrella
  before @DataFrame@ falls through to the lighter core.
* 'resolvePackageToken' / 'packageNameIndex' — keyed on package names. Exact
  membership first, then a fuzzy (trigram) match, so a bogus token the model
  invented snaps to the nearest real package.

The fuzzy keys must agree with the curated table, so every package the table
maps to is a member of 'packageNameIndex'.
-}
module Sabela.Diagnose.Packages (
    packageForModule,
    table,
    packageNameIndex,
    resolvePackageToken,
    findModulePackage,
) where

import Data.List (find, maximumBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

-- | (module prefix, package). Checked in order, so list specifics first.
table :: [(Text, Text)]
table =
    [ ("DataFrame.Display", "dataframe")
    , ("DataFrame", "dataframe-core")
    , ("Granite", "granite")
    , ("Data.Text", "text")
    , ("Data.Vector", "vector")
    , ("Data.Map", "containers")
    , ("Data.Set", "containers")
    , ("Data.Aeson", "aeson")
    ]

{- | The package providing a module, by exact name or dotted-prefix match.
'Nothing' for base modules and anything not in the table.
-}
packageForModule :: Text -> Maybe Text
packageForModule m = snd <$> find (matches . fst) table
  where
    matches p = p == m || (p <> ".") `T.isPrefixOf` m

{- | Known real package names. The 'table' targets are always present so the
two resolvers agree; the rest are common notebook packages a weak model is
likely to mangle. Outside this set a token goes verbatim into @-- cabal:@ and
cabal itself becomes the verifier.
-}
packageNameIndex :: [Text]
packageNameIndex =
    nubOrd $
        map snd table
            ++ [ "bytestring"
               , "directory"
               , "filepath"
               , "process"
               , "time"
               , "random"
               , "unordered-containers"
               , "hashable"
               , "scientific"
               , "split"
               , "mtl"
               , "transformers"
               ]

nubOrd :: (Ord a) => [a] -> [a]
nubOrd = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
        | x `Set.member` seen = go seen xs
        | otherwise = x : go (Set.insert x seen) xs

{- | Resolve a (possibly bogus) package token to a real package name. Exact
membership wins; otherwise the closest 'packageNameIndex' entry by trigram
similarity, when that similarity clears 'fuzzyThreshold'. 'Nothing' when the
token resembles nothing known, so a genuinely-new package falls through to be
declared verbatim.
-}
resolvePackageToken :: Text -> Maybe Text
resolvePackageToken tok
    | tok `elem` packageNameIndex = Just tok
    | null scored = Nothing
    | bestScore >= fuzzyThreshold = Just best
    | otherwise = Nothing
  where
    scored = [(p, trigramSimilarity tok p) | p <- packageNameIndex]
    (best, bestScore) = maximumBy (comparing snd) scored

-- | Minimum trigram Jaccard similarity for a fuzzy package-token match.
fuzzyThreshold :: Double
fuzzyThreshold = 0.2

-- | Jaccard similarity of the two tokens' character-trigram sets.
trigramSimilarity :: Text -> Text -> Double
trigramSimilarity a b
    | Set.null union = 0
    | otherwise =
        fromIntegral (Set.size inter) / fromIntegral (Set.size union)
  where
    inter = Set.intersection ta tb
    union = Set.union ta tb
    ta = trigrams a
    tb = trigrams b

trigrams :: Text -> Set.Set Text
trigrams t
    | T.length t < 3 = Set.singleton t
    | otherwise = Set.fromList [T.take 3 (T.drop i t) | i <- [0 .. T.length t - 3]]

{- | Authoritative module-to-package lookup via @ghc-pkg find-module@ (local,
no network). The fall-through 'resolveMessage' uses when 'packageForModule'
misses. 'Nothing' when ghc-pkg names no package (or is unavailable).
-}
findModulePackage :: Text -> IO (Maybe Text)
findModulePackage m = (>>= firstPackage) <$> tryFind
  where
    tryFind = do
        r <-
            readProcessWithExitCode
                "ghc-pkg"
                ["--simple-output", "find-module", T.unpack m]
                ""
        case r of
            (ExitSuccess, out, _) -> pure (Just (T.pack out))
            _ -> pure Nothing
    firstPackage out = case T.words out of
        (w : _) -> Just (stripVersion w)
        [] -> Nothing

-- | "granite-0.7.3.0" -> "granite"; drops a trailing dotted-numeric component.
stripVersion :: Text -> Text
stripVersion = T.intercalate "-" . takeWhile (not . isVersion) . T.splitOn "-"
  where
    isVersion p =
        not (T.null p) && T.all (\c -> c `elem` ("0123456789." :: String)) p
