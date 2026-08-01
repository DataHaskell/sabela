{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.ValueEcho (
    echoCharBound,
    echoTimeLimitMicros,
    listingCharBudget,
    synopsisSlack,
    valueSynopsis,
    elidedUnevaluated,
    holeLines,
    nullaryPureType,
    definedListing,
    echoListing,
    normalizeListing,
    noWritableBindings,
    partitionLive,
    staleNote,
    liveBindingsReport,
    writableName,
) where

import Data.Char (isAlphaNum, isLower)
import Data.List (partition)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Grammar.Synth (sanitizeTypeText)
import Sabela.AI.LeakShape (leakyLine)
import Sabela.AI.Shape (
    Grid (..),
    elementCount,
    ellipsed,
    gridDelimiters,
    gridHeader,
    gridOf,
 )

echoCharBound :: Int
echoCharBound = 80

echoTimeLimitMicros :: Int
echoTimeLimitMicros = 500000

listingCharBudget :: Int
listingCharBudget = 2500

{- | How far past the echo bound a synopsis may run: the measurements are
worth their bytes, but they are still bounded.
-}
synopsisSlack :: Int
synopsisSlack = 140

{- | What a value that will not fit IS, measured from its own rendering, so
a Map, an encoded JSON value and a rendered table all take one path and no
type name appears in the control flow.
-}
valueSynopsis :: Text -> Text
valueSynopsis v = "<" <> T.intercalate "; " (measures <> shapeFacts <> [headFact]) <> ">"
  where
    ls = filter (not . T.null . T.strip) (T.lines v)
    measures =
        [tShow (T.length v) <> " chars"]
            <> [tShow (length ls) <> " lines" | length ls > 1]
    shapeFacts = case gridOf gridDelimiters ls of
        Just g -> [tShow (gridWidth g) <> " columns"] <> headerFact g
        Nothing -> [tShow n <> " elements" | Just n <- [elementCount v]]
    headerFact g =
        ["header: " <> hs | h <- maybeToList (gridHeader g), Just hs <- [headerCells h]]
    headFact = "head: " <> ellipsed headBound (T.strip (T.takeWhile (/= '\n') v))

-- | How much of the header row, and of the first line, a synopsis may quote.
headerBound, headBound :: Int
headerBound = 60
headBound = 60

{- | Whole header cells only, and a count of the ones left out: a cell cut in
half would read as a column name the artefact does not have.
-}
headerCells :: [Text] -> Maybe Text
headerCells h
    | null kept = Nothing
    | null dropped = Just (T.intercalate " | " kept)
    | otherwise =
        Just (T.intercalate " | " kept <> " …" <> tShow (length dropped) <> " more")
  where
    (kept, dropped) = splitAt fitting h
    fitting =
        length (takeWhile (<= headerBound) (scanl1 (+) (map ((+ 3) . T.length) h)))

elidedUnevaluated :: Text
elidedUnevaluated =
    "<unevaluated: no value within the "
        <> tShow (echoTimeLimitMicros `div` 1000)
        <> "ms echo budget>"

holeLines :: Text -> [(Text, Text)]
holeLines raw = [b | l <- T.lines raw, Just b <- [holeLine l]]

holeLine :: Text -> Maybe (Text, Text)
holeLine l = case T.breakOn " :: " l of
    (lhs, rhs)
        | T.null rhs -> Nothing
        | otherwise -> case T.breakOn " = " (T.drop 4 rhs) of
            (ty, val)
                | T.strip (T.drop 3 val) == "_" ->
                    Just (T.strip lhs, T.strip ty)
            _ -> Nothing

nullaryPureType :: Text -> Bool
nullaryPureType ty =
    not ("->" `T.isInfixOf` t) && not ("IO " `T.isPrefixOf` t) && t /= "IO"
  where
    t = T.strip ty

definedListing :: [Text] -> Text -> Text
definedListing defined raw =
    T.unlines [l | l <- T.lines raw, bindingName l `elem` defined]

bindingName :: Text -> Text
bindingName l = T.strip (fst (T.breakOn " :: " l))

partitionLive :: [Text] -> Text -> (Text, [Text])
partitionLive defined raw = (T.unlines live, map bindingName stale)
  where
    (live, stale) =
        partition isLive [l | l <- T.lines raw, not (T.null (T.strip l))]
    isLive l = bindingName l `elem` defined

staleNote :: [Text] -> Text
staleNote [] = ""
staleNote names =
    "stale (no current cell defines these; left over from replaced or deleted \
    \cells): "
        <> T.take 90 (T.intercalate ", " names)
        <> (if T.length (T.intercalate ", " names) > 90 then "…" else "")
        <> "\n"

noWritableBindings :: Text
noWritableBindings = "no writable bindings in the live session yet."

writableName :: Text -> Bool
writableName n =
    maybe False (\(c, _) -> isLower c || c == '_') (T.uncons n)
        && T.all (\c -> isAlphaNum c || c `elem` ("_'" :: String)) n

normalizeListing :: Text -> Text
normalizeListing raw =
    T.unlines
        [ l
        | logical <- joinContinuations (T.lines raw)
        , Just l <- [renderLogical logical]
        ]

joinContinuations :: [Text] -> [Text]
joinContinuations = go . filter (not . T.null . T.strip)
  where
    go [] = []
    go (l : ls) = walk l ls
    walk cur [] = [cur]
    walk cur (x : xs)
        | isBindingHead x = cur : walk x xs
        | otherwise = walk (cur <> " " <> T.strip x) xs

isBindingHead :: Text -> Bool
isBindingHead l = case T.breakOn " :: " l of
    (n, rest) -> not (T.null rest) && writableName (T.strip n)

renderLogical :: Text -> Maybe Text
renderLogical l
    | not (isBindingHead l) = Nothing
    | leakyLine ty' = Nothing
    | otherwise = Just (name <> " :: " <> ty' <> valuePart)
  where
    (rawName, rest) = T.breakOn " :: " l
    name = T.strip rawName
    (ty, val) = T.breakOn " = " (T.drop 4 rest)
    ty' = T.strip (sanitizeTypeText ty)
    value = T.strip (T.drop 3 val)
    valuePart
        | T.null val = ""
        | value == "_" = " = _"
        | leakyLine value = ""
        | otherwise = " = " <> value

liveBindingsReport :: [Text] -> (Text -> Maybe Text) -> Text -> Text
liveBindingsReport defined echo raw
    | T.null (T.strip listed) = noWritableBindings
    | otherwise = listed
  where
    (live, stale) = partitionLive defined (normalizeListing raw)
    listed = echoListing echo live <> staleNote (filter writableName stale)

echoListing :: (Text -> Maybe Text) -> Text -> Text
echoListing echo raw = boundListing (T.unlines (map line (T.lines raw)))
  where
    line l = case holeLine l of
        Just (n, ty)
            | nullaryPureType ty -> n <> " :: " <> ty <> " = " <> shown n
            | otherwise -> n <> " :: " <> ty
        Nothing
            | "= _" `T.isSuffixOf` T.stripEnd l ->
                T.stripEnd (T.dropEnd 3 (T.stripEnd l))
            | otherwise -> l
    shown n = case echo n of
        Nothing -> elidedUnevaluated
        Just v
            | T.length v > echoCharBound -> valueSynopsis v
            | otherwise -> v

boundListing :: Text -> Text
boundListing t
    | T.length t <= listingCharBudget = t
    | otherwise = T.unlines (kept ++ [cutNote])
  where
    ls = T.lines t
    kept = takeWithin (listingCharBudget - 60) ls
    cutNote =
        "…["
            <> tShow (length ls - length kept)
            <> " more bindings over the listing budget]"
    takeWithin _ [] = []
    takeWithin budget (x : rest)
        | cost <= budget = x : takeWithin (budget - cost) rest
        | otherwise = []
      where
        cost = T.length x + 1

tShow :: Int -> Text
tShow = T.pack . show
