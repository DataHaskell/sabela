{-# LANGUAGE OverloadedStrings #-}

{- | What the adapter's two spec files share: the plan under test, the probe
that stands in for a compile, and the readers that pull one field out of the
payload. Nothing here asserts; the properties live beside the shapes they pin.
-}
module Test.HoleAdapterRig (
    arrow,
    carriesWrapAt,
    changedLines,
    convBlob,
    diagWith,
    endsInNewline,
    fitWrites,
    holeArguments,
    noBlameDiag,
    payloadOf,
    plan,
    provOf,
    planWith,
    provTyped,
    textAt,
    unbracket,
) where

import Data.Aeson (Value (..), object)
import Data.Aeson.Types (Pair)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.HoleRewrite (
    HolePlan,
    HoleRewrite,
    holeName,
    holeRewritePairs,
    holeRewritePlan,
    holeWrap,
    runHoleRewrite,
 )
import Test.HoleAdapterGen (
    ArgSource (..),
    argMismatchCode,
    mismatchDiag,
 )
import Test.HoleRewriteGen (countingProbe, field, namedHoleBlob)

plan :: ArgSource -> Text -> Text -> Maybe HolePlan
plan = planWith mismatchDiag

planWith ::
    (Text -> Text -> Text -> Int -> Text -> Text) ->
    ArgSource ->
    Text ->
    Text ->
    Maybe HolePlan
planWith mk as expected actual =
    holeRewritePlan
        (mk expected actual (asFn as) (asIndex as) (asArg as))
        (asSrc as)

diagWith :: ArgSource -> Text -> Text -> Text
diagWith as expected actual =
    mismatchDiag expected actual (asFn as) (asIndex as) (asArg as)

-- | A rejection GHC blamed on a statement rather than on any argument.
noBlameDiag :: Text -> Text -> Text
noBlameDiag expected actual =
    "<interactive>:7:15: error: [GHC-"
        <> T.pack (show argMismatchCode)
        <> "]\n    \8226 Couldn't match expected type \8216"
        <> expected
        <> "\8217 with actual type \8216"
        <> actual
        <> "\8217\n    \8226 In a stmt of a 'do' block"

convBlob :: Text -> Text -> Text -> Text
convBlob actual expected conv =
    namedHoleBlob holeName (arrow actual expected) [(conv, arrow actual expected)]

arrow :: Text -> Text -> Text
arrow a b = a <> " -> " <> b

-- | The pairs a rejection carries once a probe has answered with @ty@.
payloadOf :: ArgSource -> Text -> Text -> IO [Pair]
payloadOf as ty conv = do
    (probe, _) <- countingProbe (namedHoleBlob holeName ty [(conv, ty)])
    out <- runHoleRewrite probe (diagWith as "Ee" "Aa") (asSrc as)
    pure (maybe [] holeRewritePairs out)

-- | The rewrite's own object, out of whatever pairs a probe answer earned.
provOf :: Maybe HoleRewrite -> Maybe Value
provOf out = field "holeRewrite" (object (maybe [] holeRewritePairs out))

provTyped :: ArgSource -> Text -> Text -> IO (Maybe Value)
provTyped as ty conv =
    field "holeRewrite" . object <$> payloadOf as ty conv

-- | Whether the harness's wrap really stands at the argument's own column.
carriesWrapAt :: ArgSource -> Text -> Bool
carriesWrapAt as out =
    holeWrap (asArg as) `T.isPrefixOf` T.drop (col - 1) line
  where
    (row, col) = asSpan as
    line = fromMaybe "" (listToMaybe (drop (row - 1) (T.splitOn "\n" out)))

textAt :: Text -> Value -> Maybe Text
textAt k v = case field k v of
    Just (String s) -> Just s
    _ -> Nothing

fitWrites :: Maybe Value -> [Text]
fitWrites prov = case prov >>= field "holeFits" of
    Just (Array xs) -> [w | v <- foldr (:) [] xs, Just (String w) <- [field "write" v]]
    _ -> []

{- | The arguments the harness's hole stands applied to in a rewritten
candidate: the units at bracket depth zero inside its own brackets, read from
the text rather than from the wrap that wrote them.
-}
holeArguments :: Text -> [Text]
holeArguments out = case T.breakOn (holeName <> " ") out of
    (_, r)
        | T.null r -> []
        | otherwise ->
            topUnits (toClose (T.drop (T.length holeName + 1) r))

-- | One layer of enclosing brackets removed, when a pair encloses the whole.
unbracket :: Text -> Text
unbracket t
    | T.length t > 1
    , enclosed t =
        T.dropEnd 1 (T.drop 1 t)
    | otherwise = t

-- | Whether brackets opened at the head of a text close only at its end.
enclosed :: Text -> Bool
enclosed t = case scanl step (0 :: Int) (T.unpack t) of
    (_ : rest@(_ : _)) ->
        T.head t `elem` ("([" :: String)
            && last rest == 0
            && all (> 0) (init rest)
    _ -> False

step :: Int -> Char -> Int
step d c
    | c `elem` ("([" :: String) = d + 1
    | c `elem` (")]" :: String) = d - 1
    | otherwise = d

-- | The text up to the bracket that closes the one it stands inside.
toClose :: Text -> Text
toClose t = T.take (go 0 0) t
  where
    go d i
        | i >= T.length t = i
        | T.index t i == ')' && d == 0 = i
        | otherwise = go (step d (T.index t i)) (i + 1)

-- | A text split at the whitespace outside any bracket.
topUnits :: Text -> [Text]
topUnits t = filter (not . T.null) (go 0 0 0)
  where
    go d start i
        | i >= T.length t = [T.drop start t]
        | d == 0 && T.index t i == ' ' =
            T.take (i - start) (T.drop start t) : go 0 (i + 1) (i + 1)
        | otherwise = go (step d (T.index t i)) start (i + 1)

changedLines :: Text -> Text -> [Int]
changedLines a b =
    [ i
    | (i, x, y) <- zip3 [1 ..] (T.splitOn "\n" a) (T.splitOn "\n" b)
    , x /= y
    ]

endsInNewline :: Text -> Bool
endsInNewline = T.isSuffixOf "\n"
