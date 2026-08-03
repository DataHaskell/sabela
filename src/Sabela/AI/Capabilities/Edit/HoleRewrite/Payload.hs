{-# LANGUAGE OverloadedStrings #-}

{- | What a rewritten candidate earned, as the caller reads it. Every field is
taken from the block GHC wrote about the harness's own hole, and the payload
names the harness as the author of a rewrite nothing committed.
-}
module Sabela.AI.Capabilities.Edit.HoleRewrite.Payload (
    HoleRewrite (..),
    Placement (..),
    Trigger (..),
    holeFitCap,
    holeName,
    holeRewritePairs,
    holeWrap,
    triggerLabel,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.Char (isAlphaNum, isUpper)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.ArgRepair (ArgBlame (..))
import Sabela.AI.Capabilities.Edit.HoleRewrite.Section (
    foundHole,
    holeSection,
    typeVariables,
    uninformativeResult,
 )
import Sabela.AI.FitRule (holeFitsJson)
import Sabela.AI.Hints (expectedTypeOf)

-- | Which test recognised the rejection the hole answers.
data Trigger = TriggerCode Int | TriggerText | TriggerMismatchText
    deriving (Eq, Show)

triggerLabel :: Trigger -> Text
triggerLabel (TriggerCode c) = "GHC-" <> T.pack (show c)
triggerLabel TriggerText = "not-in-scope text"
triggerLabel TriggerMismatchText = "type-mismatch text"

{- | Where the hole was put: over a head the compiler could not resolve, or
around the argument whose type it rejected.
-}
data Placement
    = AtHead Text
    | AtArgument ArgBlame
    deriving (Eq, Show)

{- | One rewritten candidate and what the compiler said about it. The rewritten
text itself is not retained, so no caller can commit it or hand it to the
execution path's own hole check.
-}
data HoleRewrite = HoleRewrite
    { hrPlacement :: Placement
    , hrTrigger :: Trigger
    , hrBinder :: Maybe Text
    , hrSubsumed :: [Text]
    , hrUnanswered :: [Text]
    , hrDiagnostic :: Text
    }
    deriving (Eq, Show)

holeFitCap :: Int
holeFitCap = 8

{- | The hole the harness writes. It is named so that GHC's answer about it can
be told apart from GHC's answer about any hole the candidate already carried.
-}
holeName :: Text
holeName = "_sabelaHole"

{- | The adapter the harness writes around a rejected argument. GHC brackets a
compound expression it names in prefix argument position and leaves an
operator's operand bare, so the wrap brackets what the blame did not.
-}
holeWrap :: Text -> Text
holeWrap expr = "(" <> holeName <> " " <> asArgument (T.strip expr) <> ")"

{- | An expression as it can stand in argument position: one token or an
already-bracketed expression unchanged, anything else bracketed. A hole written
beside an unbracketed application takes its parts as arguments of its own.
-}
asArgument :: Text -> Text
asArgument expr
    | oneToken expr || bracketed expr = expr
    | otherwise = "(" <> expr <> ")"

-- | Whether brackets opened at the head of an expression close only at its end.
bracketed :: Text -> Bool
bracketed expr = case scanl depth (0 :: Int) (T.unpack expr) of
    (_ : rest@(_ : _)) ->
        T.head expr `elem` ("([" :: String)
            && last rest == 0
            && all (> 0) (init rest)
    _ -> False
  where
    depth d c
        | c `elem` ("([" :: String) = d + 1
        | c `elem` (")]" :: String) = d - 1
        | otherwise = d

{- | Whether an expression is a single name or literal, module qualifier and
all. A dot is part of a token only where the segment before it is a module
name, so an unspaced composition is bracketed rather than left bare.
-}
oneToken :: Text -> Bool
oneToken expr = case reverse (T.splitOn "." expr) of
    (base : qualifiers) -> plain base && all moduleSegment qualifiers
    [] -> False
  where
    plain b = not (T.null b) && T.all nameChar b
    moduleSegment q =
        plain q && maybe False (isUpper . fst) (T.uncons q)
    nameChar c = isAlphaNum c || c `elem` ("_'" :: String)

{- | What the rewrite earned, or nothing at all: the block GHC wrote about the
harness's own hole, under one key that carries the provenance. The one fit
dropped is the binder the hole sits in, which only says to recurse.
-}
holeRewritePairs :: HoleRewrite -> [Pair]
holeRewritePairs hr = case holeSection holeName (hrDiagnostic hr) of
    Nothing -> []
    Just section ->
        [ "holeRewrite"
            .= object
                ( placementPairs (hrPlacement hr)
                    <> [ "by" .= byNote
                       , "trigger" .= triggerLabel (hrTrigger hr)
                       ]
                    <> blamePairs (hrSubsumed hr) (hrUnanswered hr)
                    <> ["holeType" .= t | Just (_, t) <- [foundHole section]]
                    <> [ "typeVariables" .= vars
                       | vars <- [holeTypeVars section]
                       , not (null vars)
                       ]
                    <> [ "holeTypeCaveat" .= caveat r
                       | Just (_, t) <- [foundHole section]
                       , Just r <- [uninformativeResult t]
                       ]
                    <> ["holeFits" .= fits | fits <- [sectionFits section], not (null fits)]
                    <> ["expectedType" .= g | Just g <- [expectedTypeOf section]]
                )
        ]
  where
    sectionFits = filter (not . recursive) . holeFitsJson holeFitCap
    recursive fit = isJust (hrBinder hr) && writeOf fit == hrBinder hr
    holeTypeVars section = maybe [] (typeVariables . snd) (foundHole section)

{- | The other arguments the same rejection blamed: those the answered one
accounts for, and those it does not. Only a blame the compiler's own span puts
inside the answered expression is claimed to follow from it; of the rest the
payload says only that they were rejected too.
-}
blamePairs :: [Text] -> [Text] -> [Pair]
blamePairs [] [] = []
blamePairs follows unanswered =
    [ "blames"
        .= object
            ( ["follows" .= follows | not (null follows)]
                <> ["unanswered" .= unanswered | not (null unanswered)]
                <> ["note" .= T.unwords notes]
            )
    ]
  where
    notes =
        [ "follows stands inside the expression answered here."
        | not (null follows)
        ]
            <> [ "unanswered was rejected too and is not answered here."
               | not (null unanswered)
               ]

{- | What the harness did, in the words of the placement it chose. The argument
form names the call and the position so the caller can tell which of several
arguments the compiler was asked about.
-}
placementPairs :: Placement -> [Pair]
placementPairs (AtHead nm) = ["substituted" .= nm, "with" .= holeName]
placementPairs (AtArgument b) =
    [ "wrapped" .= abExpr b
    , "with" .= holeWrap (abExpr b)
    , "argument" .= abIndex b
    , "of" .= abFunction b
    ]

-- | The name a rendered fit tells the caller to write.
writeOf :: Value -> Maybe Text
writeOf (Object o) = case KM.lookup (Key.fromText "write") o of
    Just (String w) -> Just w
    _ -> Nothing
writeOf _ = Nothing

byNote :: Text
byNote =
    "the harness, in a throwaway compile; your cell was not edited and \
    \nothing was committed"

{- | Said of a hole whose result carries no information, because GHC prints
that result whether the slot wanted it or its own default rules chose it.
-}
caveat :: Text -> Text
caveat r =
    "result type "
        <> r
        <> " carries no information: GHC prints it both where the slot wants "
        <> r
        <> " and where its default rules chose it, so it is unconfirmed"
