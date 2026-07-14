{-# LANGUAGE OverloadedStrings #-}

{- | Pure helpers for typed-hole repair: read the goal type a not-in-scope error
implies ('goalFromError'), turn a hole-fit blob into candidate names
('holeFitNames', over the richer 'parseHoleFits'), and substitute a wrong name
for a fit in a cell's source ('substituteName'). Shared by the in-notebook repair
path and the eval harness.
-}
module Sabela.AI.HoleRepair (
    goalFromError,
    holeFitNames,
    suggestedNames,
    orderBySimilarity,
    substituteName,
) where

import Data.Char (isAlphaNum)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoleFits (HoleFit (..), parseHoleFits)

{- | The (wrong-name, goal-type) a @not in scope: name :: type@ error implies, so
we can ask GHC for hole fits of that goal. Handles both the one-line form and
GHC's multi-line form where the name + type sit on the line AFTER the marker:

> Variable not in scope:
>   foldrr :: (a -> a -> a) -> t -> [Int] -> b

'Nothing' when the error is not that shape (e.g. a bare "not in scope" with no
inferred type, or a type-constructor error, which has no goal type to fit).
-}
goalFromError :: Text -> Maybe (Text, Text)
goalFromError err = do
    rest <- afterInfix "not in scope:" err
    let (lhs, rhs) = T.breakOn "::" rest
    if T.null rhs
        then Nothing
        else do
            name <- lastWord (T.strip lhs)
            let ty = T.strip (T.takeWhile (/= '\n') (T.drop 2 rhs))
            if T.null ty then Nothing else Just (name, ty)
  where
    lastWord t = case reverse (T.words t) of
        (w : _) -> Just w
        [] -> Nothing

-- | Plain (non-refinement) hole-fit names to try, over the shared 'parseHoleFits'.
holeFitNames :: Text -> [Text]
holeFitNames blob = [hfWrite f | f <- parseHoleFits blob, not (hfRefined f)]

{- | Replace every ident-class token equal to @wrong@ with @fit@ in @src@. A
whole-token replace: 'Nothing' clever about scope, strings, or comments, so the
caller must verify the result compiles before keeping it.
-}
substituteName :: Text -> Text -> Text -> Text
substituteName wrong fit src
    | wrong == fit || T.null wrong = src
    | otherwise = T.concat (map sub (T.groupBy sameClass src))
  where
    sub tok = if tok == wrong then fit else tok
    sameClass a b = isIdent a == isIdent b
    isIdent c = isAlphaNum c || c == '_' || c == '.' || c == '\''

{- | Names GHC's "Perhaps use" did-you-mean offers for a not-in-scope error —
the backtick-quoted names after the phrase. These are spelling-corrections, so
they are the right first candidates for a typo (e.g. @lengthh@ -> @length@).
-}
suggestedNames :: Text -> [Text]
suggestedNames err = maybe [] backtickNames (afterInfix "Perhaps use" err)
  where
    backtickNames s = case T.breakOn "`" s of
        (_, r)
            | T.null r -> []
            | otherwise ->
                let (nm, r2) = T.breakOn "'" (T.drop 1 r)
                 in if T.null r2 then [] else nm : backtickNames (T.drop 1 r2)

{- | Order candidate names by closeness to the wrong name (Levenshtein), so a
misspelling heals to the spelling-nearest valid name — @lengthh@ picks @length@,
not an arbitrary same-typed fit like @product@.
-}
orderBySimilarity :: Text -> [Text] -> [Text]
orderBySimilarity wrong = sortOn (editDistance wrong)

-- | Levenshtein edit distance between two names.
editDistance :: Text -> Text -> Int
editDistance a b = go (T.unpack a) (T.unpack b)
  where
    go xs ys = last (foldl transform [0 .. length xs] ys)
      where
        transform ns@(n : ns') y = scanl calc (n + 1) (zip3 xs ns ns')
          where
            calc z (x, diag, nxt) =
                minimum [z + 1, nxt + 1, diag + if x == y then 0 else 1]
        transform [] _ = []

afterInfix :: Text -> Text -> Maybe Text
afterInfix needle t = case T.breakOn needle t of
    (_, rest) | not (T.null rest) -> Just (T.drop (T.length needle) rest)
    _ -> Nothing
