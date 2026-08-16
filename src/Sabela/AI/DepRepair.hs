{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.DepRepair (
    addBuildDepend,
    depFromResult,
    depName,
    newDependencies,
    pinnedDep,
) where

import Control.Applicative ((<|>))
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (depName, hiddenPackage, packageNeedsFlag, pinnedDep)
import Sabela.Model (CellError (..))

{- | Adds a dependency entry (possibly version-pinned). A package the line
already names, under any constraint spelling, is never added again — the
cell's own pin always wins.
-}
addBuildDepend :: Text -> Text -> Text
addBuildDepend entry src =
    case break (T.isInfixOf "build-depends:") ls of
        (before, depLine : after)
            | depName entry `elem` map depName (declaredDeps depLine) -> src
            | otherwise -> T.unlines (before ++ [depLine <> ", " <> entry] ++ after)
        (_, []) -> T.unlines (("-- cabal: build-depends: " <> entry) : ls)
  where
    ls = T.lines src

declaredDeps :: Text -> [Text]
declaredDeps line =
    map T.strip (T.splitOn "," (T.drop (T.length "build-depends:") afterField))
  where
    afterField = snd (T.breakOn "build-depends:" line)

sourceDeps :: Text -> [Text]
sourceDeps src = case break (T.isInfixOf "build-depends:") (T.lines src) of
    (_, depLine : _) -> filter (not . T.null) (declaredDeps depLine)
    (_, []) -> []

-- | The entries whose package NAME the prior source did not already declare.
newDependencies :: Text -> Text -> [Text]
newDependencies priorSrc candidate =
    filter
        ((`notElem` map depName (sourceDeps priorSrc)) . depName)
        (sourceDeps candidate)

depFromResult :: Either Text ExecutionResult -> Maybe Text
depFromResult (Left _) = Nothing
depFromResult (Right er) =
    listToMaybe (mapMaybe pkgFrom errorTexts)
  where
    errorTexts = maybeToList (erError er) ++ map ceMessage (erErrors er)
    pkgFrom t = hiddenPackage t <|> packageNeedsFlag t
