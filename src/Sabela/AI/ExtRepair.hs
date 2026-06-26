{-# LANGUAGE OverloadedStrings #-}

{- | Auto-repair for a missing LANGUAGE extension, the deterministic sibling of
"Sabela.AI.DepRepair": GHC names the extension it needs, so enabling it is a fact,
not a guess. Paired with 'Sabela.Diagnose.neededExtension' (the validated detector)
and driven by the same chokepoint repair loop.
-}
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

{- | Enable a LANGUAGE extension in a cell: merge it into the first
@{\-# LANGUAGE ... #-\}@ pragma (a no-op when already listed anywhere), or insert a
fresh pragma line after a leading @-- cabal:@ line (else at the very top, before
the imports).
-}
addExtension :: Text -> Text -> Text
addExtension ext src
    | ext `elem` declaredExts src = src
    | otherwise = case break isPragma ls of
        (before, pragma : after) -> T.unlines (before ++ [mergeExt ext pragma] ++ after)
        (_, []) -> T.unlines (insertPragma ext ls)
  where
    ls = T.lines src

-- | Every extension already listed across the cell's LANGUAGE pragmas.
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

-- | The extension list inside a @{\-# LANGUAGE ... #-\}@ line.
pragmaBody :: Text -> Text
pragmaBody l = fst (T.breakOn "#-}" (T.drop (T.length "LANGUAGE") after))
  where
    after = snd (T.breakOn "LANGUAGE" l)

-- | Rebuild a pragma line with @ext@ appended to its existing extensions.
mergeExt :: Text -> Text -> Text
mergeExt ext pragma =
    "{-# LANGUAGE " <> T.intercalate ", " (exts ++ [ext]) <> " #-}"
  where
    exts = filter (not . T.null) (map T.strip (T.splitOn "," (pragmaBody pragma)))

-- | Insert a fresh pragma line after a leading @-- cabal:@ line, else at the top.
insertPragma :: Text -> [Text] -> [Text]
insertPragma ext ls = case ls of
    (l : rest) | "-- cabal:" `T.isPrefixOf` T.stripStart l -> l : pragma : rest
    _ -> pragma : ls
  where
    pragma = "{-# LANGUAGE " <> ext <> " #-}"

{- | The extension a failed run says is missing — GHC's "intended to use X" hint,
ready for 'addExtension'. Nothing when the run did not fail on a missing extension,
so the repair only fires on the deterministic case.
-}
extFromResult :: Either Text ExecutionResult -> Maybe Text
extFromResult (Left _) = Nothing
extFromResult (Right er) = listToMaybe (mapMaybe neededExtension errorTexts)
  where
    errorTexts = maybeToList (erError er) ++ map ceMessage (erErrors er)
