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

addBuildDepend :: Text -> Text -> Text
addBuildDepend pkg src =
    case break (T.isInfixOf "build-depends:") ls of
        (before, depLine : after)
            | pkg `elem` declaredDeps depLine -> src
            | otherwise -> T.unlines (before ++ [depLine <> ", " <> pkg] ++ after)
        (_, []) -> T.unlines (("-- cabal: build-depends: " <> pkg) : ls)
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

newDependencies :: Text -> Text -> [Text]
newDependencies priorSrc candidate =
    filter (`notElem` sourceDeps priorSrc) (sourceDeps candidate)

depFromResult :: Either Text ExecutionResult -> Maybe Text
depFromResult (Left _) = Nothing
depFromResult (Right er) =
    listToMaybe (mapMaybe pkgFrom errorTexts)
  where
    errorTexts = maybeToList (erError er) ++ map ceMessage (erErrors er)
    pkgFrom t = hiddenPackage t <|> packageNeedsFlag t
