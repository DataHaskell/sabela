{-# LANGUAGE OverloadedStrings #-}

{- | The standing goal and its satisfaction judgement (search-api.md section
8.3): a held call-ready consumer signature whose argument type no held fact
produces makes that type the cluster goal, and every answer is judged against
it — found is not a verdict, goal satisfaction is. Keyed on the diagnostic
class required-argument-type-has-no-surfaced-producer, never a library name.
-}
module Siza.Agent.Discover.Goal (
    argTypesOf,
    genuineGaps,
    goalClusterKey,
    goalFromArgs,
    goalProduced,
    goalSatisfied,
    injectGoal,
    injectRecent,
    recentFromArgs,
    literalFill,
    nominalArgType,
    producesExact,
    producesGoal,
    resultOf,
    standingGoal,
    withGoal,
) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAlphaNum, isUpper)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Literal (literalConstructible, literalFill)
import Siza.Agent.Discover.Types (StandingGoal (..))

-- Signature shape ------------------------------------------------------------

-- | Split a signature on top-level @->@ only (paren\/bracket depth aware).
arrowSegments :: Text -> [Text]
arrowSegments = map (T.strip . T.pack) . go (0 :: Int) "" . T.unpack
  where
    go _ acc [] = [reverse acc]
    go 0 acc ('-' : '>' : rest) = reverse acc : go 0 "" rest
    go d acc (c : rest) = go (d + delta c) (c : acc) rest
    delta c
        | c `elem` ("([" :: String) = 1
        | c `elem` (")]" :: String) = -1
        | otherwise = 0

dropConstraints :: Text -> Text
dropConstraints = last . T.splitOn "=>"

-- | The argument types of a signature, constraints stripped.
argTypesOf :: Text -> [Text]
argTypesOf ty = case arrowSegments (dropConstraints ty) of
    [] -> []
    segs -> init segs

-- | The result type of a signature, constraints stripped.
resultOf :: Text -> Text
resultOf ty = case arrowSegments (dropConstraints ty) of
    [] -> ""
    segs -> last segs

{- | A goal-candidate argument type: one bare upper-headed nominal token.
Literal-constructible atoms a cell writes directly are never a goal.
-}
nominalArgType :: Text -> Bool
nominalArgType t =
    not (T.null t)
        && maybe False (isUpper . fst) (T.uncons t)
        && T.all (\c -> isAlphaNum c || c == '\'') t
        && t `notElem` literalConstructible

-- | Does @ty@ produce @goal@: its result type equals or ends in the goal.
producesGoal :: Text -> Text -> Bool
producesGoal goal ty =
    not (T.null ty)
        && (normType ty == g || ("-> " <> g) `T.isSuffixOf` normType ty)
  where
    g = normType goal

{- | The genuine gaps of @sig@ against @heldSigs@: its nominal argument types
that no held signature produces — the holes a candidate must carry, and (by
count) the seed-ranking proximity metric, fewest gaps winning.
-}
genuineGaps :: [Text] -> Text -> [Text]
genuineGaps heldSigs sig =
    [ ty
    | ty <- argTypesOf sig
    , nominalArgType ty
    , not (any (producesGoal ty) heldSigs)
    ]

-- | Does @ty@ produce @goal@ EXACTLY as its whole (nullary) result.
producesExact :: Text -> Text -> Bool
producesExact goal ty = normType ty == normType goal

normType :: Text -> Text
normType = T.unwords . T.words

-- The evidence-derived standing goal -----------------------------------------

{- | Held call-ready facts as (name, signature, package) triples — the shapes
'Siza.Agent.Discover.Advice.harvestFacts' and
'Siza.Agent.Discover.Facts.compilerFact' emit, package = provenance.

The signature ends at the provenance dash, WHICHEVER provenance follows.
Splitting only on @" — found in "@ left a compiler-confirmed fact carrying its
own marker into the signature, so @defaultReadOptions :: ReadOptions@ read as
the type @"ReadOptions — confirmed by the compiler (check_type)"@ and produced
nothing: the strongest evidence class could not close a goal, and
@live_test34_wine@ hunted a @ReadOptions@ producer the working cell already had.
-}
consumerFacts :: [Text] -> [(Text, Text, Text)]
consumerFacts facts =
    [ (name, sig, foundInPackage prov)
    | f <- facts
    , Just body <- [T.stripPrefix "`" f]
    , let (name, rest) = T.breakOn "` :: " body
    , not (T.null name)
    , Just sigProv <- [T.stripPrefix "` :: " rest]
    , let (sig0, prov) = T.breakOn provenanceMark sigProv
          sig = T.strip sig0
    , not (T.null sig)
    ]
  where
    provenanceMark = " — "
    -- Only "found in Module (package)" names a package; the compiler marker's
    -- own parenthesis is the tool that confirmed it, never provenance.
    foundInPackage prov = case T.stripPrefix (provenanceMark <> "found in ") prov of
        Just found -> case reverse (T.splitOn "(" found) of
            (lastPart : _ : _) -> T.strip (T.takeWhile (/= ')') lastPart)
            _ -> ""
        Nothing -> ""

{- | Derive the standing goal from held evidence: the MOST RECENT held
consumer whose argument type is nominal and produced by no held fact — the
active deliverable, never a list-order accident (section 8.3); the consumer
is the goal's citable derivation.
-}
standingGoal :: [Text] -> Maybe StandingGoal
standingGoal facts =
    listToMaybe
        [ StandingGoal ty name pkg
        | (name, sig, pkg) <- reverse consumers
        , ty <- argTypesOf sig
        , nominalArgType ty
        , not (any (producesGoal ty) heldSigs)
        ]
  where
    consumers = consumerFacts facts
    heldSigs = [sig | (_, sig, _) <- consumers]

-- The judgement --------------------------------------------------------------

{- | Section 8.3 satisfaction requires goal-class evidence: a hit whose
signature produces the goal type, an exact hit on the target when the target
is the goal's OWN business, or a card ONLY when its package matches the
goal's derivation provenance — a foreign card can never satisfy a goal it
says nothing about.

Target equality alone is not goal-class evidence. It let any exact-name
lookup close an unrelated hunt: goal @FilePath@ (from @readCsv@) went
satisfied on a query for @summary@ because a hit was named @summary@, and the
armed gate then collapsed the next three searches to duplicates
(@live_test33_wine@; @live_test_wine@ did the same via @Event@ and
@http-conduit@).
-}
goalSatisfied :: StandingGoal -> Text -> Value -> Bool
goalSatisfied sg target v = provenanceCard || any ok (jsonHits v)
  where
    g = sgType sg
    ok h = producesGoal g (jsonText "type" h) || namesGoalsOwnBusiness h
    namesGoalsOwnBusiness h =
        jsonText "name" h == target
            && (target == sgConsumer sg || producesExact g target)
    provenanceCard =
        not (T.null (sgPackage sg))
            && cardPackage v == Just (sgPackage sg)

-- | Does any hit of the envelope actually produce the goal type?
goalProduced :: Text -> Value -> Bool
goalProduced g v = any (producesGoal g . jsonText "type") (jsonHits v)

-- | The one ledger cluster a standing goal's hunt occupies, any spelling.
goalClusterKey :: Text -> Text
goalClusterKey g = "_goal:" <> T.toLower g

{- | Attach the in-band @goal@ disclosure to a fresh evaluation's envelope:
type, satisfied, derivation, and (when unsatisfied) the nearest hit named.
-}
withGoal :: StandingGoal -> Text -> Value -> Value
withGoal sg target v@(Object o) =
    Object (KM.insert "goal" disclosure o)
  where
    g = sgType sg
    sat = goalSatisfied sg target v
    disclosure =
        object
            (["type" .= g, "satisfied" .= sat] <> derivation <> note)
    derivation
        | T.null (sgConsumer sg) = []
        | otherwise = ["derivedFrom" .= sgConsumer sg]
    note
        | sat = []
        | otherwise = ["note" .= ("no hit produces " <> g <> nearest)]
    nearest = case jsonHits v of
        (h : _) ->
            "; nearest: "
                <> jsonText "name" h
                <> ( let t = jsonText "type" h
                      in if T.null t then "" else " :: " <> T.take 120 t
                   )
        [] -> ""
withGoal _ _ v = v

{- | Package attribution of an envelope's card, when it names one — the
provenance a card must share with the goal's derivation to satisfy it.
-}
cardPackage :: Value -> Maybe Text
cardPackage (Object o) = case KM.lookup "card" o of
    Just c | p <- jsonText "package" c, not (T.null p) -> Just p
    _ -> Nothing
cardPackage _ = Nothing

-- The guard-to-request seam --------------------------------------------------

-- | Read the guard-injected standing goal off a discover call's arguments.
goalFromArgs :: Value -> Maybe StandingGoal
goalFromArgs (Object o)
    | Just g@(Object _) <- KM.lookup "_goal" o
    , t <- jsonText "type" g
    , not (T.null t) =
        Just (StandingGoal t (jsonText "consumer" g) (jsonText "package" g))
goalFromArgs _ = Nothing

{- | Ride the standing goal on a discover call's arguments — the seam through
which ledger provenance reaches producer ranking without a schema change.
-}
injectGoal :: Maybe StandingGoal -> Value -> Value
injectGoal Nothing v = v
injectGoal (Just sg) (Object o) = Object (KM.insert "_goal" (goalArg sg) o)
injectGoal (Just sg) Null = object ["_goal" .= goalArg sg]
injectGoal _ v = v

{- | Ride the session's established packages on a discover call's arguments —
the same schema-free seam the goal uses. Refinement state RANKS downstream,
it never suppresses: the corrected use of ledger memory after the goal gate
collapsed correct searches into citations (honeycomb, live_test33).
-}
injectRecent :: [Text] -> Value -> Value
injectRecent [] v = v
injectRecent pkgs (Object o) = Object (KM.insert "_recent" (toJSON pkgs) o)
injectRecent pkgs Null = object ["_recent" .= pkgs]
injectRecent _ v = v

-- | The guard-injected recent packages, off a discover call's arguments.
recentFromArgs :: Value -> [Text]
recentFromArgs (Object o) = case KM.lookup "_recent" o of
    Just (Array xs) -> [t | String t <- foldr (:) [] xs]
    _ -> []
recentFromArgs _ = []

goalArg :: StandingGoal -> Value
goalArg sg =
    object
        [ "type" .= sgType sg
        , "consumer" .= sgConsumer sg
        , "package" .= sgPackage sg
        ]

-- Local envelope readers -----------------------------------------------------

jsonHits :: Value -> [Value]
jsonHits (Object o) = case KM.lookup "hits" o of
    Just (Array a) -> foldr (:) [] a
    _ -> []
jsonHits _ = []

jsonText :: Text -> Value -> Text
jsonText k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
jsonText _ _ = ""
