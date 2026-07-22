{-# LANGUAGE OverloadedStrings #-}

{- | A general covering verifier for symbolic-regression-shaped tasks: extract
the arithmetic expression a run reported, evaluate it over the sample points,
and recompute the total squared error. The model PROPOSES an expression; this
VETS it by recomputation, so any zero-error expression passes and a wrong one
fails — no overfitting to a single known answer string.

The accepted grammar is @x@, numeric constants, @+@, @-@, @*@, @^@ and
parentheses (the task's search grammar plus a tolerant superset), so an
equivalent winner such as @x^2@ is accepted alongside @(x*x)@.
-}
module Eval.FitCheck (
    FitOutcome (..),
    fitOutcome,
    fitResult,
    fitsSample,
    extractCandidates,
    extractExpression,
    parseExpr,
    evalAt,
    Expr (..),
) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- | An arithmetic expression over the single variable @x@.
data Expr
    = X
    | Num Double
    | Neg Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Pow Expr Expr
    deriving (Eq, Show)

{- | The three-valued verify verdict (R5-T5): confirmation and refutation both
carry the recomputed error plus the expression it was recomputed FROM, so a
denial always has positive evidence; anything unextractable is unconfirmed.
-}
data FitOutcome
    = FitConfirmed Double Text
    | FitRefuted Double Text
    | FitUnconfirmed Text
    deriving (Eq, Show)

{- | Vet a run's output by recomputation over every parseable candidate the
text carries, format-agnostically. Any candidate within @tol@ confirms; else
the best non-trivial x-expression refutes with its recomputed error; a text
with neither routes to unconfirmed — never to a fabricated denial.
-}
fitOutcome :: [(Double, Double)] -> Double -> Text -> FitOutcome
fitOutcome points tol out
    | Just (err, e) <- best (const True) = FitConfirmed err e
    | Just (err, e) <- bestX = FitRefuted err e
    | otherwise =
        FitUnconfirmed "no parseable arithmetic expression in the reported output"
  where
    scored = [(errOf c, c) | c <- extractCandidates out]
    errOf c =
        maybe
            (1 / 0)
            (\e -> sum [(evalAt x e - y) ** 2 | (x, y) <- points])
            (parseExpr c)
    best keep = case [(err, c) | (err, c) <- scored, err <= tol, keep c] of
        [] -> Nothing
        cs -> Just (minimumBy (comparing fst) cs)
    bestX = case [(err, c) | (err, c) <- scored, nonTrivialX c] of
        [] -> Nothing
        cs -> Just (minimumBy (comparing fst) cs)

{- | Refutation-grade candidates must be a genuine function of @x@ — an @x@
with structure — so a stray prose @x@ can never manufacture a denial.
-}
nonTrivialX :: Text -> Bool
nonTrivialX c =
    T.any (\ch -> ch == 'x' || ch == 'X') c
        && T.any (\ch -> ch `elem` ("+-*^" :: String) || isDigit ch) c

{- | The recomputed total squared error of a run's reported expression over the
sample (the best candidate), or 'Left' when the output carries no parseable
expression. Trusts the sample, never the model's self-reported error number.
-}
fitResult :: [(Double, Double)] -> Text -> Either Text Double
fitResult points out = case errs of
    [] -> Left "no parseable arithmetic expression in the reported output"
    es -> Right (minimum es)
  where
    errs =
        [ sum [(evalAt x e - y) ** 2 | (x, y) <- points]
        | c <- extractCandidates out
        , Just e <- [parseExpr c]
        ]

{- | Does the run's reported expression fit the sample within @tol@ squared
error? A missing or unparseable expression never fits.
-}
fitsSample :: [(Double, Double)] -> Double -> Text -> Bool
fitsSample points tol out = case fitOutcome points tol out of
    FitConfirmed _ _ -> True
    _ -> False

{- | Every parseable expression candidate in a text, format-agnostically:
maximal runs of grammar characters, rejected when glued inside a word (the
@x@ inside \"expression\"), then balance-trimmed of unmatched parentheses.
-}
extractCandidates :: Text -> [Text]
extractCandidates out =
    [ c
    | (prev, run, next) <- runs (T.unpack out)
    , not (gluedStart prev run)
    , not (gluedEnd run next)
    , c <- [T.strip (T.pack run), balanceTrim (T.strip (T.pack run))]
    , not (T.null c)
    , Just _ <- [parseExpr c]
    ]
  where
    gluedStart prev run = case (prev, run) of
        (Just p, r : _) -> isAlpha p && isAlphaNum r
        _ -> False
    gluedEnd run next = case (next, reverse run) of
        (Just n, r : _) -> isAlpha n && isAlphaNum r
        _ -> False

-- | Maximal grammar-character runs with their adjacent boundary characters.
runs :: String -> [(Maybe Char, String, Maybe Char)]
runs = go Nothing
  where
    go _ [] = []
    go prev s = case span exprChar s of
        ([], c : rest) -> go (Just c) rest
        (run, rest) ->
            (prev, run, headMay rest) : case rest of
                (c : more) -> go (Just c) more
                [] -> []
    headMay (c : _) = Just c
    headMay [] = Nothing
    exprChar c = isDigit c || c `elem` ("xX.+-*^() " :: String)

-- | Drop unmatched leading @(@ and trailing @)@ so tuple fragments parse.
balanceTrim :: Text -> Text
balanceTrim t
    | opens > closes
    , Just rest <- T.stripPrefix "(" t =
        balanceTrim (T.strip rest)
    | closes > opens
    , Just rest <- T.stripSuffix ")" t =
        balanceTrim (T.strip rest)
    | otherwise = t
  where
    opens = T.count "(" t
    closes = T.count ")" t

{- | The expression a run printed, taken from the text after the word
@expression@: skip a leading @:@/@=@/@is@, then keep the leading run of
grammar characters (@x@, digits, @. + - * ^ ( )@, spaces). That stops cleanly at
the @, total squared error …@ or @ with error …@ tail without a fixed delimiter
list. 'Nothing' when absent or empty.
-}
extractExpression :: Text -> Maybe Text
extractExpression out
    | T.null rest = Nothing
    | T.null expr = Nothing
    | otherwise = Just expr
  where
    (_, rest) = T.breakOn "expression" out
    afterWord = dropLeading (T.drop (T.length "expression") rest)
    expr = T.strip (T.takeWhile exprChar afterWord)
    dropLeading = stripIs . T.dropWhile (\c -> isSpace c || c == ':' || c == '=')
    stripIs s = fromMaybe s (T.stripPrefix "is " s)
    exprChar c = isDigit c || c `elem` ("xX.+-*^() " :: String)

-- | Parse the whole text as one expression, or 'Nothing' if it does not.
parseExpr :: Text -> Maybe Expr
parseExpr t = case tokenize t >>= parseSum of
    Just (e, []) -> Just e
    _ -> Nothing

-- | Evaluate an expression at a given @x@.
evalAt :: Double -> Expr -> Double
evalAt x = go
  where
    go X = x
    go (Num n) = n
    go (Neg e) = negate (go e)
    go (Add a b) = go a + go b
    go (Sub a b) = go a - go b
    go (Mul a b) = go a * go b
    go (Pow a b) = go a ** go b

data Tok = TNum Double | TX | TPlus | TMinus | TMul | TPow | TL | TR
    deriving (Eq, Show)

tokenize :: Text -> Maybe [Tok]
tokenize = go . T.unpack
  where
    go [] = Just []
    go (c : cs)
        | isSpace c = go cs
        | c == '+' = (TPlus :) <$> go cs
        | c == '-' = (TMinus :) <$> go cs
        | c == '*' = (TMul :) <$> go cs
        | c == '^' = (TPow :) <$> go cs
        | c == '(' = (TL :) <$> go cs
        | c == ')' = (TR :) <$> go cs
        | c == 'x' || c == 'X' = (TX :) <$> go cs
        | isDigit c || c == '.' =
            let (num, rest) = span (\d -> isDigit d || d == '.') (c : cs)
             in (\n -> (TNum n :)) <$> readMaybe num <*> go rest
        | otherwise = Nothing

parseSum :: [Tok] -> Maybe (Expr, [Tok])
parseSum ts = do
    (l, ts') <- parseProduct ts
    chain l ts'
  where
    chain l (TPlus : rest) = parseProduct rest >>= \(r, r') -> chain (Add l r) r'
    chain l (TMinus : rest) = parseProduct rest >>= \(r, r') -> chain (Sub l r) r'
    chain l rest = Just (l, rest)

parseProduct :: [Tok] -> Maybe (Expr, [Tok])
parseProduct ts = do
    (l, ts') <- parsePow ts
    chain l ts'
  where
    chain l (TMul : rest) = parsePow rest >>= \(r, r') -> chain (Mul l r) r'
    chain l rest = Just (l, rest)

parsePow :: [Tok] -> Maybe (Expr, [Tok])
parsePow ts = do
    (b, ts') <- parseAtom ts
    case ts' of
        TPow : rest -> parsePow rest >>= \(e, r) -> Just (Pow b e, r)
        _ -> Just (b, ts')

parseAtom :: [Tok] -> Maybe (Expr, [Tok])
parseAtom (TNum n : rest) = Just (Num n, rest)
parseAtom (TX : rest) = Just (X, rest)
parseAtom (TMinus : rest) = parseAtom rest >>= \(e, r) -> Just (Neg e, r)
parseAtom (TL : rest) = do
    (e, rest') <- parseSum rest
    case rest' of
        TR : r -> Just (e, r)
        _ -> Nothing
parseAtom _ = Nothing
