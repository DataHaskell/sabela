{-# LANGUAGE OverloadedStrings #-}

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

argTypesOf :: Text -> [Text]
argTypesOf ty = case arrowSegments (dropConstraints ty) of
    [] -> []
    segs -> init segs

resultOf :: Text -> Text
resultOf ty = case arrowSegments (dropConstraints ty) of
    [] -> ""
    segs -> last segs

nominalArgType :: Text -> Bool
nominalArgType t =
    not (T.null t)
        && maybe False (isUpper . fst) (T.uncons t)
        && T.all (\c -> isAlphaNum c || c == '\'') t
        && t `notElem` literalConstructible

producesGoal :: Text -> Text -> Bool
producesGoal goal ty =
    not (T.null ty)
        && (normType ty == g || ("-> " <> g) `T.isSuffixOf` normType ty)
  where
    g = normType goal

genuineGaps :: [Text] -> Text -> [Text]
genuineGaps heldSigs sig =
    [ ty
    | ty <- argTypesOf sig
    , nominalArgType ty
    , not (any (producesGoal ty) heldSigs)
    ]

producesExact :: Text -> Text -> Bool
producesExact goal ty = normType ty == normType goal

normType :: Text -> Text
normType = T.unwords . T.words

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
    foundInPackage prov = case T.stripPrefix (provenanceMark <> "found in ") prov of
        Just found -> case reverse (T.splitOn "(" found) of
            (lastPart : _ : _) -> T.strip (T.takeWhile (/= ')') lastPart)
            _ -> ""
        Nothing -> ""

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

goalProduced :: Text -> Value -> Bool
goalProduced g v = any (producesGoal g . jsonText "type") (jsonHits v)

goalClusterKey :: Text -> Text
goalClusterKey g = "_goal:" <> T.toLower g

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

cardPackage :: Value -> Maybe Text
cardPackage (Object o) = case KM.lookup "card" o of
    Just c | p <- jsonText "package" c, not (T.null p) -> Just p
    _ -> Nothing
cardPackage _ = Nothing

goalFromArgs :: Value -> Maybe StandingGoal
goalFromArgs (Object o)
    | Just g@(Object _) <- KM.lookup "_goal" o
    , t <- jsonText "type" g
    , not (T.null t) =
        Just (StandingGoal t (jsonText "consumer" g) (jsonText "package" g))
goalFromArgs _ = Nothing

injectGoal :: Maybe StandingGoal -> Value -> Value
injectGoal Nothing v = v
injectGoal (Just sg) (Object o) = Object (KM.insert "_goal" (goalArg sg) o)
injectGoal (Just sg) Null = object ["_goal" .= goalArg sg]
injectGoal _ v = v

injectRecent :: [Text] -> Value -> Value
injectRecent [] v = v
injectRecent pkgs (Object o) = Object (KM.insert "_recent" (toJSON pkgs) o)
injectRecent pkgs Null = object ["_recent" .= pkgs]
injectRecent _ v = v

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
