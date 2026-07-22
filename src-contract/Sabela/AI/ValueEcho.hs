{-# LANGUAGE OverloadedStrings #-}

{- | Evidence-backed value echo (docs/discover/search-api.md §9.2): a nullary
pure binding's value is echoed within a pinned size\/time budget, elision
states the exceeded bound, and the @= _@ placeholder never reaches the model
(TRIAGE M14). Pure; the evaluator is injected by the caller.
-}
module Sabela.AI.ValueEcho (
    echoCharBound,
    echoTimeLimitMicros,
    listingCharBudget,
    elidedOverSize,
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
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Grammar.Synth (sanitizeTypeText)
import Sabela.AI.LeakShape (leakyLine)

-- | Longest inlined echo value, in characters.
echoCharBound :: Int
echoCharBound = 80

-- | Longest wall-clock wait for one echo evaluation, in microseconds.
echoTimeLimitMicros :: Int
echoTimeLimitMicros = 500000

-- | The whole listing's budget (R3.9), aligned with the envelope cap.
listingCharBudget :: Int
listingCharBudget = 2500

elidedOverSize :: Text
elidedOverSize =
    "<unevaluated: exceeds the "
        <> tShow echoCharBound
        <> "-char echo budget>"

elidedUnevaluated :: Text
elidedUnevaluated =
    "<unevaluated: no value within the "
        <> tShow (echoTimeLimitMicros `div` 1000)
        <> "ms echo budget>"

-- | @(name, type)@ of every @name :: ty = _@ line in a bindings listing.
holeLines :: Text -> [(Text, Text)]
holeLines raw = [b | l <- T.lines raw, Just b <- [holeLine l]]

-- | Parse one @name :: ty = _@ line; 'Nothing' for any other line shape.
holeLine :: Text -> Maybe (Text, Text)
holeLine l = case T.breakOn " :: " l of
    (lhs, rhs)
        | T.null rhs -> Nothing
        | otherwise -> case T.breakOn " = " (T.drop 4 rhs) of
            (ty, val)
                | T.strip (T.drop 3 val) == "_" ->
                    Just (T.strip lhs, T.strip ty)
            _ -> Nothing

{- | A type whose value is a plain constant: no function arrow and not an
IO action — the only class whose echo is safe and meaningful.
-}
nullaryPureType :: Text -> Bool
nullaryPureType ty =
    not ("->" `T.isInfixOf` t) && not ("IO " `T.isPrefixOf` t) && t /= "IO"
  where
    t = T.strip ty

{- | Restrict a bindings listing to the given defined names — the write-ack
echo shows only what THIS cell defined, never the whole session.
-}
definedListing :: [Text] -> Text -> Text
definedListing defined raw =
    T.unlines [l | l <- T.lines raw, bindingName l `elem` defined]

bindingName :: Text -> Text
bindingName l = T.strip (fst (T.breakOn " :: " l))

{- | R6.5 kernel truth: split a session bindings listing into lines whose name
a CURRENT cell defines (live) and leftovers of replaced\/deleted cells (stale
names, in listing order) — the session keeps them alive; the report must not.
-}
partitionLive :: [Text] -> Text -> (Text, [Text])
partitionLive defined raw = (T.unlines live, map bindingName stale)
  where
    (live, stale) =
        partition isLive [l | l <- T.lines raw, not (T.null (T.strip l))]
    isLive l = bindingName l `elem` defined

-- | One bounded line flagging stale session bindings; empty when none.
staleNote :: [Text] -> Text
staleNote [] = ""
staleNote names =
    "stale (no current cell defines these; left over from replaced or deleted \
    \cells): "
        <> T.take 90 (T.intercalate ", " names)
        <> (if T.length (T.intercalate ", " names) > 90 then "…" else "")
        <> "\n"

-- | The ONE bounded statement of a session with nothing writable to list.
noWritableBindings :: Text
noWritableBindings = "no writable bindings in the live session yet."

-- | A pasteable binding name: lowercase\/underscore head, identifier chars.
writableName :: Text -> Bool
writableName n =
    maybe False (\(c, _) -> isLower c || c == '_') (T.uncons n)
        && T.all (\c -> isAlphaNum c || c `elem` ("_'" :: String)) n

{- | Reduce a raw session listing to writable @name :: Type@ logical lines
(R3.10): wrapped continuations rejoin their binding, types pass the same
name-reduction as hit signatures, and a leak-shaped value is dropped rather
than dumped. Anything unwritable is omitted, never shown.
-}
normalizeListing :: Text -> Text
normalizeListing raw =
    T.unlines
        [ l
        | logical <- joinContinuations (T.lines raw)
        , Just l <- [renderLogical logical]
        ]

-- | Rejoin physical lines that continue the previous binding's line.
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

-- | One writable @name :: ty [= value]@ line, or 'Nothing' when unwritable.
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

{- | The @list_bindings@ report over the normalised listing: live bindings
echoed, stale ones flagged in one bounded line, and a session with nothing
writable answered by 'noWritableBindings' — internals never cross (R3.10).
-}
liveBindingsReport :: [Text] -> (Text -> Maybe Text) -> Text -> Text
liveBindingsReport defined echo raw
    | T.null (T.strip listed) = noWritableBindings
    | otherwise = listed
  where
    (live, stale) = partitionLive defined (normalizeListing raw)
    listed = echoListing echo live <> staleNote (filter writableName stale)

{- | Rewrite a bindings listing so @= _@ is unrepresentable: a nullary pure
binding echoes its value within 'echoCharBound' (elision names the exceeded
bound), a function or IO binding answers with its type alone, and the whole
listing stays within 'listingCharBudget'.
-}
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
            | T.length v > echoCharBound -> elidedOverSize
            | otherwise -> v

-- | Keep whole lines within 'listingCharBudget', disclosing what was cut.
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
