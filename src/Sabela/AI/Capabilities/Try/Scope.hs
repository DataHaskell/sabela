{- | Candidates for a not-found module, drawn from the packages the candidate
source already declares.

A global pool needs an absolute similarity floor to stay sane, and that floor
is what rejects the answer: `Control.Algebra.State` scores 0.154 against
`Bluefin.State`, rank 1 of bluefin's 35 modules but rank 257 of 6,964 installed
ones, with 102 wrong candidates above the 0.2 floor. Scope the pool by the
package the cell itself names and the ranking needs no floor at all — take the
nearest few and let the compiler reject them.
-}
module Sabela.AI.Capabilities.Try.Scope (
    scopedModuleCandidates,
) where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)

import Sabela.AI.PackageIndex (
    PackageEntry (..),
    installedPackages,
    storePackageDb,
 )
import Sabela.AI.Similarity (trigramSimilarity)
import Sabela.Diagnose.Parse (declaredPackages)

{- | The @k@ modules nearest to @wrong@ among the declared packages' modules.
No similarity floor: the pool is already scoped, so the trial is the filter.
-}
scopedModuleCandidates :: Int -> Text -> Text -> IO [(Text, PackageEntry)]
scopedModuleCandidates k src wrong
    | null declared = pure []
    | otherwise = do
        mDb <- storePackageDb
        case mDb of
            Nothing -> pure []
            Just db -> do
                pkgs <- installedPackages db
                pure (take k (sortOn (Down . nearness) (pool pkgs)))
  where
    declared = declaredPackages src
    pool pkgs =
        [ (m, p)
        | p <- pkgs
        , peName p `elem` declared
        , m <- peModules p
        , m /= wrong
        ]
    nearness = trigramSimilarity wrong . fst
