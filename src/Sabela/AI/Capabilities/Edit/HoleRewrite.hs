{-# LANGUAGE OverloadedStrings #-}

{- | Rewrites a rejected candidate to ask the compiler a typed hole: over a
not-in-scope head, and around the argument whose type a mismatch blamed. The
rewrite lives only inside a disposable compile, and every payload says so.
-}
module Sabela.AI.Capabilities.Edit.HoleRewrite (
    HolePlan (..),
    HoleRewrite (..),
    Placement (..),
    Trigger (..),
    triggerLabel,
    holeName,
    holeWrap,
    rewriteTarget,
    enclosingBinder,
    holeRewriteSource,
    holeRewritePlan,
    runHoleRewrite,
    holeRewritePairs,
    headSpans,
    notInScopeCodes,
    holeFitCap,
) where

import Control.Applicative ((<|>))
import Data.Maybe (isJust, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.ArgRepair (ArgBlame (..), wrapExprAt)
import Sabela.AI.Capabilities.Edit.HoleRewrite.Adapter (
    AdapterTarget (..),
    adapterTarget,
 )
import Sabela.AI.Capabilities.Edit.HoleRewrite.Head (
    enclosingBinder,
    headSpans,
 )
import Sabela.AI.Capabilities.Edit.HoleRewrite.Payload (
    HoleRewrite (..),
    Placement (..),
    Trigger (..),
    holeFitCap,
    holeName,
    holeRewritePairs,
    holeWrap,
    triggerLabel,
 )
import Sabela.AI.Capabilities.Edit.HoleRewrite.Section (errorBlocks, foundHole)
import Sabela.AI.Health (scopeSubject)
import Sabela.AI.HoleRepair (substituteNameAt)
import Sabela.AI.TypedHole (containsTypedHole)
import Sabela.Errors (ghcCodeIn)
import Sabela.Parse (cellNames)

{- | The single rewrite a rejection earns, decided before any compile: where
the hole goes, what recognised the rejection, the binder it sits in, the blames
that follow from the one answered, and the candidate as the probe will see it.
-}
data HolePlan = HolePlan
    { hpPlacement :: Placement
    , hpTrigger :: Trigger
    , hpBinder :: Maybe Text
    , hpSubsumed :: [Text]
    , hpUnanswered :: [Text]
    , hpRewritten :: Text
    }
    deriving (Eq, Show)

-- | GHC's structural codes for a name that is not in scope.
notInScopeCodes :: [Int]
notInScopeCodes = [88464, 39999]

{- | The name to put a hole in, why it qualified, and where it stands. Nothing
when the candidate already asks an annotated hole, when no reported name is
applied to anything, or when the name is one the cell itself defines.
-}
rewriteTarget :: Text -> Text -> Maybe (Text, Trigger, (Int, Int))
rewriteTarget diagnostic src
    | alreadyAsking src = Nothing
    | otherwise =
        listToMaybe
            [ (name, trigger, place)
            | chunk <- T.splitOn "\n\n" diagnostic
            , termLevel chunk
            , Just trigger <- [triggerFor chunk]
            , Just name <- [scopeSubject chunk]
            , not (name `Set.member` defined)
            , place <- take 1 (headSpans name src)
            ]
  where
    defined = fst (cellNames src)

{- | Whether the candidate already puts a question to the compiler the harness
must not add a second one beside: an annotated hole, or the harness's own
marker left over from an earlier pass.
-}
alreadyAsking :: Text -> Bool
alreadyAsking src = containsTypedHole src || holeName `T.isInfixOf` src

{- | Whether the compiler already answered a hole of the caller's own. A bare
@_@ in an expression earns a @Found hole@ block; a pattern wildcard does not,
so this declines on the one that is a question and not on the one that is not.
-}
foreignHoleAsked :: Text -> Bool
foreignHoleAsked diagnostic =
    any (isJust . foundHole) (errorBlocks diagnostic)

{- | A hole is a term, so a diagnostic about a type constructor or class names
something a hole cannot stand for.
-}
termLevel :: Text -> Bool
termLevel chunk =
    not ("type constructor or class" `T.isInfixOf` T.toLower chunk)

triggerFor :: Text -> Maybe Trigger
triggerFor chunk = case ghcCodeIn chunk of
    Just c | c `elem` notInScopeCodes -> Just (TriggerCode c)
    _ | isJust (scopeSubject chunk) -> Just TriggerText
    _ -> Nothing

-- | The candidate with a hole written over the head, if one can be placed.
holeRewriteSource :: Text -> Text -> Maybe (Text, Trigger, Text)
holeRewriteSource diagnostic src = fmap forget (holeRewritePlan diagnostic src)
  where
    forget p = (subjectOf (hpPlacement p), hpTrigger p, hpRewritten p)

-- | The one thing the payload names as rewritten.
subjectOf :: Placement -> Text
subjectOf (AtHead nm) = nm
subjectOf (AtArgument b) = abExpr b

{- | The single rewrite a rejection earns, chosen once and purely: a hole over
an unresolved head, else a hole around the argument whose type was rejected.
Deciding before any compile is what holds the cost to one probe.
-}
holeRewritePlan :: Text -> Text -> Maybe HolePlan
holeRewritePlan diagnostic src
    | foreignHoleAsked diagnostic = Nothing
    | otherwise = headPlan diagnostic src <|> argumentPlan diagnostic src

headPlan :: Text -> Text -> Maybe HolePlan
headPlan diagnostic src = do
    (name, trigger, place) <- rewriteTarget diagnostic src
    rewritten <- substituteNameAt place name holeName src
    if rewritten == src
        then Nothing
        else
            Just
                HolePlan
                    { hpPlacement = AtHead name
                    , hpTrigger = trigger
                    , hpBinder = enclosingBinder place src
                    , hpSubsumed = []
                    , hpUnanswered = []
                    , hpRewritten = rewritten
                    }

argumentPlan :: Text -> Text -> Maybe HolePlan
argumentPlan diagnostic src
    | alreadyAsking src = Nothing
    | otherwise = do
        target <- adapterTarget diagnostic src
        let place = atSpan target
            blame = atBlame target
        rewritten <- wrapExprAt place (abExpr blame) holeWrap src
        if rewritten == src
            then Nothing
            else
                Just
                    HolePlan
                        { hpPlacement = AtArgument blame
                        , hpTrigger = maybe TriggerMismatchText TriggerCode (atCode target)
                        , hpBinder = enclosingBinder place src
                        , hpSubsumed = atSubsumed target
                        , hpUnanswered = atUnanswered target
                        , hpRewritten = rewritten
                        }

{- | At most one rewrite and one probe per rejection: a rewritten candidate
that itself fails yields nothing rather than a second rewrite. The rewritten
text goes to the probe and is then dropped.
-}
runHoleRewrite ::
    (Text -> IO Text) -> Text -> Text -> IO (Maybe HoleRewrite)
runHoleRewrite probe diagnostic src =
    case holeRewritePlan diagnostic src of
        Nothing -> pure Nothing
        Just p -> do
            blob <- probe (hpRewritten p)
            pure
                ( Just
                    HoleRewrite
                        { hrPlacement = hpPlacement p
                        , hrTrigger = hpTrigger p
                        , hrBinder = hpBinder p
                        , hrSubsumed = hpSubsumed p
                        , hrUnanswered = hpUnanswered p
                        , hrDiagnostic = blob
                        }
                )
