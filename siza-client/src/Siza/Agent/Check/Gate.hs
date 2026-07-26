{-# LANGUAGE OverloadedStrings #-}

{- | C2 tasks 2 and 3: the symbolic half of the check gate. Compiling is not
enough — @True@ and @5 == 5@ compile perfectly and verify nothing. A proposal
must also REFERENCE the deliverable and DISCRIMINATE it:

* the reference gate (task 2) rejects a check naming no binding the task's
  own cells define, which kills the constant-expression class outright;
* the mutation gate (task 3) rejects a check that still passes when the
  deliverable's value is perturbed — a check that cannot fail is not a test.

Both are pure; 'Siza.Agent.Check.Vet' runs the perturbations through @try@.
-}
module Siza.Agent.Check.Gate (
    CheckRefusal (..),
    refusalNote,
    referenceGate,
    identifiersOf,
    mentionsAny,

    -- * Mutation gate (task 3)
    Perturbation (..),
    perturbationsFor,
    perturbCheck,
) where

import Data.Char (isAlpha, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

{- | Why a proposal was refused. Never silent: the user is told a check was
discarded, or they believe one was offered.
-}
data CheckRefusal
    = -- | Names no binding the task's own cells define.
      NoReference
    | -- | Passes on every perturbation of the value, so it cannot fail.
      Indiscriminate
    deriving (Eq, Show)

refusalNote :: CheckRefusal -> Text
refusalNote NoReference =
    "it references nothing this task defined, so it cannot be about the \
    \deliverable"
refusalNote Indiscriminate =
    "it still passes when the value is perturbed, so it cannot fail"

{- | Task 2: a check must mention at least one binding the task's own cells
define. @True@, @5 == 5@ and the rest of the constant class name none.
-}
referenceGate :: [Text] -> Text -> Maybe CheckRefusal
referenceGate owned check
    | mentionsAny owned check = Nothing
    | otherwise = Just NoReference

-- | Does @t@ use any of @names@ as a whole identifier token?
mentionsAny :: [Text] -> Text -> Bool
mentionsAny names t = any (`elem` identifiersOf t) names

{- | The whole identifier tokens of an expression. Splitting on non-identifier
characters keeps @xs@ out of @xss@, so a near-name never counts as a mention.
-}
identifiersOf :: Text -> [Text]
identifiersOf =
    filter (\w -> not (T.null w) && isAlpha (T.head w))
        . T.split (\c -> not (isAlphaNum c || c == '_' || c == '\''))

{- | One perturbation of the deliverable: a label, and the expression to
substitute for the binding.
-}
data Perturbation = Perturbation
    { pName :: !Text
    , pExpr :: !Text
    }
    deriving (Eq, Show)

{- | The perturbations that fit a value's type (task 3). Chosen so a check
that genuinely pins the value fails at least one, while a vacuous one
survives them all:

* numeric — off-by-one either way; @total == 42@ fails, @total > 0@ may not;
* string  — TRUNCATION, deliberately not emptying: @x \/= ""@ survives
  truncation and is therefore correctly refused, which is the whole point of
  the @x \/= ""@ specimen;
* list    — drop the last element and reverse, so a length or order claim
  fails while @not (null xs)@ survives.

An unrecognised type yields no perturbations, and 'Vet' treats that as
"cannot decide" rather than as a refusal.
-}
perturbationsFor :: Text -> Text -> [Perturbation]
perturbationsFor ty name
    | isNumeric = [off "+ 1" "1", off "- 1" "-1"]
    | isStringy = [Perturbation "truncated" ("init " <> paren name)]
    | isList =
        [ Perturbation "dropped last" ("init " <> paren name)
        , Perturbation "reversed" ("reverse " <> paren name)
        ]
    | otherwise = []
  where
    t = T.strip ty
    isNumeric = t `elem` ["Int", "Integer", "Double", "Float", "Rational"]
    isStringy = t `elem` ["String", "Text", "[Char]"]
    isList = "[" `T.isPrefixOf` t && not isStringy
    off lbl op =
        Perturbation ("offset by " <> op) (paren (name <> " " <> lbl))

paren :: Text -> Text
paren e = "(" <> e <> ")"

{- | The check with @name@ replaced by the perturbed expression, at whole
identifier tokens only — a substring rewrite would corrupt @xs@ inside @xss@.
-}
perturbCheck :: Text -> Perturbation -> Text -> Text
perturbCheck name p = rebuild . tokenise
  where
    rebuild = T.concat . map swap
    swap tok = if tok == name then pExpr p else tok
    -- Split into identifier and non-identifier runs, keeping both, so the
    -- expression reassembles byte-for-byte apart from the substitution.
    tokenise t
        | T.null t = []
        | isIdent (T.head t) =
            let (tok, rest) = T.span isIdent t in tok : tokenise rest
        | otherwise =
            let (gap, rest) = T.break isIdent t in gap : tokenise rest
    isIdent c = isAlphaNum c || c == '_' || c == '\''
