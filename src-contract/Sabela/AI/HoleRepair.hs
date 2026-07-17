{-# LANGUAGE OverloadedStrings #-}

{- | Pure helpers for typed-hole repair: read the goal type a not-in-scope error
implies, turn a hole-fit blob into candidate names, and substitute a wrong name
for a fit in source. Shared by the notebook repair path and the eval harness.
-}
module Sabela.AI.HoleRepair (
    goalFromError,
    holeTypeFromDiagnostic,
    droppableAnnotation,
    dropAnnotation,
    holeFitNames,
    holeFitRewrites,
    suggestedNames,
    orderBySimilarity,
    substituteName,
    substituteNameAt,
) where

import Data.Char (isAlphaNum)
import Data.List (nub, sortOn)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoleFits (HoleFit (..), parseHoleFits)

{- | The in-context type GHC infers for a typed hole: @"Found hole: _ :: T"@ →
@T@. Read non-committingly via @:type@ on a holed expression, so it reflects the
cell's real bindings — stronger than the type parsed from a not-in-scope error.
-}
holeTypeFromDiagnostic :: Text -> Maybe Text
holeTypeFromDiagnostic t = do
    rest <- afterInfix "Found hole: _ :: " t
    let ty = T.strip (T.takeWhile (/= '\n') rest)
    if T.null ty then Nothing else Just ty

{- | The bad expression annotation GHC names in @"In an expression type
signature: T"@ → @T@. A weak model often bolts a wrong @:: T@ on; dropping it
lets inference recover. 'Nothing' when the error is not about an annotation.
-}
droppableAnnotation :: Text -> Maybe Text
droppableAnnotation err = do
    rest <- afterInfix "In an expression type signature: " err
    let ty = T.strip (T.takeWhile (/= '\n') rest)
    if T.null ty then Nothing else Just ty

{- | Drop the first @:: T@ expression annotation GHC named, keeping the rest of
the line verbatim. Matches @T@ at a token boundary (so @(foo :: Int)@ keeps its
@)@); a no-op if absent, and the caller verifies the result compiles.
-}
dropAnnotation :: Text -> Text -> Text
dropAnnotation ty src = T.intercalate "\n" (go (T.lines src))
  where
    tyToks = T.words ty
    go [] = []
    go (l : ls)
        | Just l' <- scan "" l = l' : ls
        | otherwise = l : go ls
    -- Try each @::@ on the line; drop @:: ty@ at the first whose following tokens
    -- are exactly ty at a token boundary, splicing in the raw tail unchanged.
    scan acc rest = case T.breakOn "::" rest of
        (_, r) | T.null r -> Nothing
        (pre, r) ->
            let acc' = acc <> pre
             in case matchTail (T.drop 2 r) of
                    Just tl -> Just (T.stripEnd acc' <> tl)
                    Nothing -> scan (acc' <> "::") (T.drop 2 r)
    -- Raw text after ty if @afterColon@ begins (modulo whitespace) with exactly
    -- the ty tokens ending at a boundary; 'Nothing' otherwise.
    matchTail afterColon = matchToks tyToks (T.stripStart afterColon)
    matchToks [] rest
        | boundary rest = Just rest
        | otherwise = Nothing
    matchToks (tok : toks) rest = case T.stripPrefix tok rest of
        Just r -> matchToks toks (if null toks then r else T.stripStart r)
        Nothing -> Nothing
    boundary rest = case T.uncons rest of
        Nothing -> True
        Just (c, _) -> not (isIdent c)
    isIdent c = isAlphaNum c || c == '_' || c == '.' || c == '\''

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

{- | Candidate rewrites from a hole-fit @blob@: each plain fit substituted for
@wrong@ in @src@, deduped, dropping no-ops. The shared candidate source that
both the product and eval repair paths draw from.
-}
holeFitRewrites :: Text -> Text -> Text -> [Text]
holeFitRewrites wrong blob src =
    nub [s | n <- holeFitNames blob, let s = substituteName wrong n src, s /= src]

{- | Replace every ident-class token equal to @wrong@ with @fit@ in @src@. A
whole-token replace, blind to scope, strings, and comments, so the caller must
verify the result compiles before keeping it.
-}
substituteName :: Text -> Text -> Text -> Text
substituteName wrong fit src
    | wrong == fit || T.null wrong = src
    | otherwise = T.concat (map sub (T.groupBy sameClass src))
  where
    sub tok = if tok == wrong then fit else tok
    sameClass a b = isIdent a == isIdent b
    isIdent c = isAlphaNum c || c == '_' || c == '.' || c == '\''

{- | Replace only the ident-class token at 1-based @(line, col)@ with @fit@,
leaving other occurrences (including in strings and comments) untouched.
'Nothing' if the span does not point at @wrong@ or is out of range.
-}
substituteNameAt :: (Int, Int) -> Text -> Text -> Text -> Maybe Text
substituteNameAt (line, col) wrong fit src
    | T.null wrong || line < 1 || col < 1 = Nothing
    | otherwise = case splitAt (line - 1) (T.splitOn "\n" src) of
        (before, target : after) -> do
            newLine <- replaceAt target
            Just (T.intercalate "\n" (before ++ newLine : after))
        _ -> Nothing
  where
    replaceAt l =
        let (pre, rest) = T.splitAt (col - 1) l
            (tok, post) = T.span isIdent rest
         in if tok == wrong then Just (pre <> fit <> post) else Nothing
    isIdent c = isAlphaNum c || c == '_' || c == '.' || c == '\''

{- | Names GHC's "Perhaps use" did-you-mean offers: the backtick-quoted names
after the phrase. Spelling-corrections, so the best first candidates for a typo
(e.g. @lengthh@ -> @length@).
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
misspelling heals to the nearest valid spelling — @lengthh@ picks @length@, not
an arbitrary same-typed fit.
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
