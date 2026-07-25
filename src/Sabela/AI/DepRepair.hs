{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.DepRepair (
    addBuildDepend,
    depFromResult,
    newDependencies,
) where

import Control.Applicative ((<|>))
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (hiddenPackage, packageNeedsFlag)
import Sabela.Model (CellError (..))

{- | Merge @pkg@ into a cell's @-- cabal: build-depends:@ line: append it to an
existing line (a no-op when already declared), or prepend a fresh @-- cabal:@
first line when the cell declares none.
-}
addBuildDepend :: Text -> Text -> Text
addBuildDepend pkg src =
    case break (T.isInfixOf "build-depends:") ls of
        (before, depLine : after)
            | pkg `elem` declaredDeps depLine -> src
            | otherwise -> T.unlines (before ++ [depLine <> ", " <> pkg] ++ after)
        (_, []) -> T.unlines (("-- cabal: build-depends: " <> pkg) : ls)
  where
    ls = T.lines src

-- | The package names already listed on a @build-depends:@ line.
declaredDeps :: Text -> [Text]
declaredDeps line =
    map T.strip (T.splitOn "," (T.drop (T.length "build-depends:") afterField))
  where
    afterField = snd (T.breakOn "build-depends:" line)

{- | Every package a source's @-- cabal: build-depends:@ line declares, @[]@
when it has none.
-}
sourceDeps :: Text -> [Text]
sourceDeps src = case break (T.isInfixOf "build-depends:") (T.lines src) of
    (_, depLine : _) -> filter (not . T.null) (declaredDeps depLine)
    (_, []) -> []

{- | The packages @candidate@ declares that @priorSrc@ did not — G2's test for
whether a rewrite is a dependency ADD (never applied automatically, however
green it gate-verifies; see "Sabela.AI.Capabilities.Edit.RepairGate").
-}
newDependencies :: Text -> Text -> [Text]
newDependencies priorSrc candidate =
    filter (`notElem` sourceDeps priorSrc) (sourceDeps candidate)

{- | The package a failed run says is missing — the one GHC named in its "hidden
package" wall — ready for 'addBuildDepend'. Nothing when the run did not fail on
a missing package (so the repair only fires on the deterministic case).
-}
depFromResult :: Either Text ExecutionResult -> Maybe Text
depFromResult (Left _) = Nothing
depFromResult (Right er) =
    listToMaybe (mapMaybe pkgFrom errorTexts)
  where
    errorTexts = maybeToList (erError er) ++ map ceMessage (erErrors er)
    pkgFrom t = hiddenPackage t <|> packageNeedsFlag t
