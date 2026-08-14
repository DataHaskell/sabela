{- |
Technique: check-vetting refusals [Gating/Repair].
Guarantee: this is NOT the compile gate; that is server-side 'gatedCandidate' in sabela.
Entry: 'checkRefusals'. Next: Siza.Agent.Check.Vet.
-}
module Siza.Agent.Check.Gate (
    CheckRefusal (..),
    MutationOutcome (..),
    checkRefusals,
    refusalNote,
    referenceGate,
    identifiersOf,
    mentionsAny,
    Perturbation (..),
    perturbationsFor,
    perturbCheck,
) where

import Data.Char (isAlpha, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

data CheckRefusal
    = NoReference
    | Indiscriminate
    | Ungrounded
    | Unperturbable
    deriving (Bounded, Enum, Eq, Show)

refusalNote :: CheckRefusal -> Text
refusalNote NoReference =
    "it references nothing this task committed, so it cannot be about the \
    \deliverable. verify reads committed notebook state: insert_cell the \
    \binding first, then check it"
refusalNote Indiscriminate =
    "it still passes when the value is perturbed, so it cannot fail"
refusalNote Ungrounded =
    "this task has committed no binding the check could be about; commit \
    \the work with insert_cell before verifying it"
refusalNote Unperturbable =
    "no perturbation of the values it names exists, so a pass proves nothing"

checkRefusals :: [CheckRefusal]
checkRefusals = [minBound .. maxBound]

-- | Whether a mutation of the checked value could falsify the check.
data MutationOutcome
    = Discriminating
    | NotDiscriminating
    | NoPerturbationAvailable
    deriving (Eq, Show)

{- | The check must be about a name this turn caused to exist. An empty
reference set is the strongest reason to refuse, not a reason to admit.
-}
referenceGate :: [Text] -> Text -> Maybe CheckRefusal
referenceGate owned check
    | null owned = Just Ungrounded
    | mentionsAny owned check = Nothing
    | otherwise = Just NoReference

mentionsAny :: [Text] -> Text -> Bool
mentionsAny names t = any (`elem` identifiersOf t) names

identifiersOf :: Text -> [Text]
identifiersOf =
    filter (\w -> not (T.null w) && isAlpha (T.head w))
        . T.split (\c -> not (isAlphaNum c || c == '_' || c == '\''))

data Perturbation = Perturbation
    { pName :: !Text
    , pExpr :: !Text
    }
    deriving (Eq, Show)

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

perturbCheck :: Text -> Perturbation -> Text -> Text
perturbCheck name p = rebuild . tokenise
  where
    rebuild = T.concat . map swap
    swap tok = if tok == name then pExpr p else tok
    tokenise t
        | T.null t = []
        | isIdent (T.head t) =
            let (tok, rest) = T.span isIdent t in tok : tokenise rest
        | otherwise =
            let (gap, rest) = T.break isIdent t in gap : tokenise rest
    isIdent c = isAlphaNum c || c == '_' || c == '\''
