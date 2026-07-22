{-# LANGUAGE OverloadedStrings #-}

{- | Literal-constructibility of a type (R3.10): the atoms and composites a cell
can write directly, and the canonical literal for each. Split from
"Siza.Agent.Discover.Goal" for the module-size cap; the goal ranker uses
'literalConstructible' to exclude these from genuine gaps, the candidate
synthesiser uses 'literalFill' to fill them instead of holing them.
-}
module Siza.Agent.Discover.Literal (
    literalConstructible,
    literalFill,
) where

import Data.Text (Text)
import qualified Data.Text as T

-- | The scalar atoms an argument can be filled with directly, never a goal.
literalConstructible :: [Text]
literalConstructible = map fst scalarLiterals

{- | A canonical paste-valid literal for each scalar an argument can be filled
with directly — the atoms that never need a producer hunt (R3.10). @Text@ uses
a string literal (OverloadedStrings is a Sabela default extension).
-}
scalarLiterals :: [(Text, Text)]
scalarLiterals =
    [ ("Int", "0")
    , ("Integer", "0")
    , ("Word", "0")
    , ("Double", "0.0")
    , ("Float", "0.0")
    , ("Char", "' '")
    , ("Bool", "True")
    , ("String", "\"\"")
    , ("Text", "\"\"")
    , ("Ordering", "EQ")
    ]

{- | A canonical literal for a type built ONLY from literal-constructible atoms,
lists, and tuples ([(Text, Double)] → [("", 0.0)]); 'Nothing' for any type
carrying a genuine gap, so that gap stays a typed hole (R3.10). Purely
type-syntactic: it fills what a cell can write without a producer, holes the
rest — decided by the type, never a library.
-}
literalFill :: Text -> Maybe Text
literalFill ty0 = case T.strip ty0 of
    t
        | Just inner <- wrappedIn '[' ']' t ->
            (\e -> "[" <> e <> "]") <$> literalFill inner
        | Just inner <- wrappedIn '(' ')' t ->
            case splitTopCommas inner of
                [one] -> literalFill one
                parts -> do
                    lits <- traverse literalFill parts
                    pure ("(" <> T.intercalate ", " lits <> ")")
        | otherwise -> lookup (normType t) scalarLiterals
  where
    normType = T.unwords . T.words

{- | The text a matching pair of outer @open@\/@close@ brackets wraps, when the
opener at index 0 is closed by the character at the very end (depth-aware over
that one bracket kind, so @[(a, b)]@ unwraps to @(a, b)@).
-}
wrappedIn :: Char -> Char -> Text -> Maybe Text
wrappedIn open close t
    | matches = Just (T.dropEnd 1 (T.drop 1 t))
    | otherwise = Nothing
  where
    n = T.length t
    matches = case T.uncons t of
        Just (h, _) | h == open -> go (0 :: Int) (zip [0 :: Int ..] (T.unpack t))
        _ -> False
    go _ [] = False
    go d ((i, c) : rest)
        | c == open = go (d + 1) rest
        | c == close = if d - 1 == 0 then i == n - 1 else go (d - 1) rest
        | otherwise = go d rest

-- | Split on top-level commas only (paren\/bracket depth aware).
splitTopCommas :: Text -> [Text]
splitTopCommas = map (T.strip . T.pack) . go (0 :: Int) "" . T.unpack
  where
    go _ acc [] = [reverse acc]
    go 0 acc (',' : rest) = reverse acc : go 0 "" rest
    go d acc (c : rest) = go (d + delta c) (c : acc) rest
    delta c
        | c `elem` ("([" :: String) = 1
        | c `elem` (")]" :: String) = -1
        | otherwise = 0
