{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.ExtRepair (
    addExtension,
    declaredExts,
    extFromResult,
) where

import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (neededExtension)
import Sabela.Model (CellError (..))

addExtension :: Text -> Text -> Text
addExtension ext src
    | ext `elem` declaredExts src = src
    | otherwise = case break isPragma ls of
        (before, pragma : after) -> T.unlines (before ++ [mergeExt ext pragma] ++ after)
        (_, []) -> T.unlines (insertPragma ext ls)
  where
    ls = T.lines src

declaredExts :: Text -> [Text]
declaredExts src =
    [ e
    | l <- T.lines src
    , isPragma l
    , e <- map T.strip (T.splitOn "," (pragmaBody l))
    , not (T.null e)
    ]

isPragma :: Text -> Bool
isPragma l = "{-# LANGUAGE" `T.isPrefixOf` T.stripStart l

pragmaBody :: Text -> Text
pragmaBody l = fst (T.breakOn "#-}" (T.drop (T.length "LANGUAGE") after))
  where
    after = snd (T.breakOn "LANGUAGE" l)

mergeExt :: Text -> Text -> Text
mergeExt ext pragma =
    "{-# LANGUAGE " <> T.intercalate ", " (exts ++ [ext]) <> " #-}"
  where
    exts = filter (not . T.null) (map T.strip (T.splitOn "," (pragmaBody pragma)))

insertPragma :: Text -> [Text] -> [Text]
insertPragma ext ls = case ls of
    (l : rest) | "-- cabal:" `T.isPrefixOf` T.stripStart l -> l : pragma : rest
    _ -> pragma : ls
  where
    pragma = "{-# LANGUAGE " <> ext <> " #-}"

extFromResult :: Either Text ExecutionResult -> Maybe Text
extFromResult (Left _) = Nothing
extFromResult (Right er) = listToMaybe (mapMaybe neededExtension errorTexts)
  where
    errorTexts = maybeToList (erError er) ++ map ceMessage (erErrors er)
