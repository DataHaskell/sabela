{-# LANGUAGE OverloadedStrings #-}

{- | The facts cache's row codec, shared by its generator, the discover
client, and the server's own index reader, so the wire shape has exactly one
definition.
-}
module Sabela.AI.FactsRow (
    PkgFacts (..),
    emptyFacts,
    hasFacts,
    renderFactsRow,
    parseFactsRow,
) where

import Data.Text (Text)
import qualified Data.Text as T

{- | Where to read about a package, what it is for, what a dependent may
import from it, and the release the index documented ('pfVersion' is empty
in rows written before the column existed).
-}
data PkgFacts = PkgFacts
    { pfHomepage :: !Text
    , pfSynopsis :: !Text
    , pfModules :: ![Text]
    , pfVersion :: !Text
    }
    deriving (Eq, Show)

emptyFacts :: PkgFacts
emptyFacts = PkgFacts "" "" [] ""

-- | Whether a row carries anything beyond the package's own name.
hasFacts :: PkgFacts -> Bool
hasFacts f = f /= emptyFacts

{- | One package per line: name, homepage, synopsis, space-separated modules,
version. Every field is collapsed to single-spaced one-line text at render,
so a tab or newline in a value never corrupts the row.
-}
renderFactsRow :: Text -> PkgFacts -> Text
renderFactsRow name f =
    T.intercalate "\t" (map oneLine fields)
  where
    fields =
        [ name
        , pfHomepage f
        , pfSynopsis f
        , T.unwords (pfModules f)
        , pfVersion f
        ]
    oneLine = T.unwords . T.words

parseFactsRow :: Text -> Maybe (Text, PkgFacts)
parseFactsRow row = case T.splitOn "\t" row of
    (n : rest)
        | not (T.null (T.strip n)) ->
            Just
                ( T.strip n
                , PkgFacts
                    (col 0 rest)
                    (col 1 rest)
                    (T.words (col 2 rest))
                    (col 3 rest)
                )
    _ -> Nothing
  where
    col i cols = case drop i cols of
        (c : _) -> c
        [] -> ""
