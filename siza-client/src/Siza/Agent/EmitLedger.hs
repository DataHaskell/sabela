{-# LANGUAGE OverloadedStrings #-}

{- | One cross-turn content ledger over every harness-injected surface (R3.8
generalised): a block transmits verbatim once; a byte-identical repeat becomes
a bounded back-reference, changed content a 'sourceDelta' diff. Shape-keyed.
-}
module Siza.Agent.EmitLedger (
    EmitLedger,
    backRefLimit,
    blockFloor,
    dedupInjected,
    dedupText,
    eligibleBlocks,
    emitTurn,
    emptyEmitLedger,
    loadBearingKeys,
    newEmitLedger,
    recordText,
) where

import Data.Aeson (Value (..), decode, encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit, isSpace)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (mapAccumL)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Sabela.AI.SelfHeal (sourceDelta)

-- | Block-bytes -> first turn seen, plus anchor line -> latest (turn, bytes).
data EmitLedger = EmitLedger
    { elSeen :: Map Text Int
    , elAnchor :: Map Text (Int, Text)
    }

{- | Only blocks at least this long participate: a back-reference must be a
net byte win, so short blocks always pass through verbatim.
-}
blockFloor :: Int
blockFloor = 160

-- | Upper bound on a back-reference's rendered length (R3.9).
backRefLimit :: Int
backRefLimit = 120

{- | Elision-exempt envelope keys (search-api.md section 10): dedup never
rewrites a value under one of these — the response that cites a fact must
carry the fact. @candidate@ is the section 8.1/8.3 typed-hole cell.
-}
loadBearingKeys :: [Text]
loadBearingKeys =
    [ "type"
    , "signature"
    , "use"
    , "cabal"
    , "name"
    , "next"
    , "exports"
    , "candidate"
    ]
        ++ actionableKeys

{- | G5.8: the diagnostic the model is being asked to act on, and the fix that
resolves it. A repeat is the working material, not noise — live_test8 collapsed
a rejection to a back-reference and withdrew the error it demanded be fixed.

A cell's OWN output is the same material under a different key. A cell that
succeeds while printing @Decode error: parse error (Failed reading …)@ carries
its failure in @oiOutput@, not @error@, so the exemption missed it: live_test36
read the truncated back-reference, said "we need full message", re-ran the cell
and was handed the same citation, then stopped. Re-reading output is how a
model checks its own work; it cannot be answered with a reference to itself.
-}
actionableKeys :: [Text]
actionableKeys = ["diagnostic", "error", "stderr", "autofix"]

emptyEmitLedger :: EmitLedger
emptyEmitLedger = EmitLedger Map.empty Map.empty

newEmitLedger :: IO (IORef EmitLedger)
newEmitLedger = newIORef emptyEmitLedger

{- | A block's anchor: its first line, the stable identity a changed
re-emission is diffed under.
-}
anchorOf :: Text -> Text
anchorOf = T.strip . T.takeWhile (/= '\n')

{- | Record a block: first-seen turn is kept; the anchor tracks the LATEST
bytes so a later change diffs against the newest established version.
-}
recordBlock :: Int -> Text -> EmitLedger -> EmitLedger
recordBlock turn block (EmitLedger seen anch) =
    EmitLedger
        (Map.insertWith (\_ old -> old) block turn seen)
        (Map.insert (anchorOf block) (turn, block) anch)

-- | The bounded back-reference replacing a byte-identical repeat.
backRef :: Int -> Text -> Text
backRef turn block =
    "[as established turn "
        <> T.pack (show turn)
        <> " (unchanged): "
        <> T.take 40 (anchorOf block)
        <> "…]"

{- | The line diff replacing changed content, via the same 'sourceDelta' the
self-heal notes trust; carries every removed and added line.
-}
deltaText :: Int -> Text -> Text -> Text
deltaText turn old new =
    "[changed since turn "
        <> T.pack (show turn)
        <> "]"
        <> T.concat ["\n- " <> l | l <- removed]
        <> T.concat ["\n+ " <> l | l <- added]
  where
    (removed, added) = sourceDelta old new

{- | How one eligible block leaves the ledger: kept (first occurrence),
referenced (byte-identical repeat), or diffed (changed under its anchor).
-}
rewriteBlock :: Int -> Text -> EmitLedger -> (EmitLedger, Maybe Text)
rewriteBlock turn block led = case Map.lookup block (elSeen led) of
    Just t -> (led, Just (backRef t block))
    Nothing -> case Map.lookup (anchorOf block) (elAnchor led) of
        Just (t, old)
            | d <- deltaText t old block
            , T.length d < T.length block ->
                (recordBlock turn block led, Just d)
        _ -> (recordBlock turn block led, Nothing)

{- | Rewrite one content text against the ledger (pure core): paragraph
blocks first, then quoted spans inside surviving paragraphs.
-}
dedupText :: Int -> Text -> EmitLedger -> (Text, EmitLedger)
dedupText turn text led0 =
    let (led', chunks) = mapAccumL chunkPass led0 (T.splitOn "\n\n" text)
     in (T.intercalate "\n\n" chunks, led')
  where
    chunkPass led chunk
        | T.length chunk < blockFloor = (led, chunk)
        -- A chunk carrying an actionable diagnostic is never whole-replaced:
        -- the model is being asked to fix precisely this text.
        | carriesActionable chunk || carriesVerdict chunk || reportsFailure chunk =
            (recordBlock turn chunk led, chunk)
        | otherwise = case rewriteBlock turn chunk led of
            (led', Just replacement) -> (led', replacement)
            (led', Nothing) -> spanPass turn chunk led'

{- | Does this chunk carry a diagnostic the model must act on? Only a NON-EMPTY
string counts: @"error":null@ rides along in every cell echo, and exempting
those disabled the dedup that sheds ~15k re-injected bytes per request.
-}
carriesActionable :: Text -> Bool
carriesActionable chunk = any nonEmptyValue actionableKeys
  where
    nonEmptyValue k =
        ("\"" <> k <> "\":\"") `T.isInfixOf` chunk
            && not (("\"" <> k <> "\":\"\"") `T.isInfixOf` chunk)

{- | A verify-channel verdict is BY CONSTRUCTION the payload the model is
being asked to act on, so it is never elided (G5.9). live_test9 collapsed it
to a back-reference six times while still demanding the check be fixed.
-}
carriesVerdict :: Text -> Bool
carriesVerdict = T.isInfixOf "\"tool_name\":\"verify\""

-- | Record a text's blocks without rewriting (model-authored turns).
recordText :: Int -> Text -> EmitLedger -> EmitLedger
recordText turn text led0 = foldl' chunkRecord led0 (T.splitOn "\n\n" text)
  where
    chunkRecord led chunk
        | T.length chunk < blockFloor = led
        | otherwise =
            foldl'
                (flip (recordBlock turn))
                (recordBlock turn chunk led)
                (spansOf chunk)

{- | Span-position context of the quoted-span walk: the previous span's
logical text and whether the walk sits inside a protected key's array value.
-}
data SpanCtx = SpanCtx {scPrev :: Maybe Text, scInArray :: Bool}

startCtx :: SpanCtx
startCtx = SpanCtx Nothing False

{- | One walk step over the text BETWEEN spans: whether the span that opens
after @pre@ is the JSON value of a load-bearing key (protect-by-key,
section 10), plus the context the following span sees.
-}
spanStep :: SpanCtx -> Text -> Text -> (Bool, SpanCtx)
spanStep ctx pre logical = (prot, SpanCtx (Just logical) inArray)
  where
    sep = T.filter (not . isSpace) pre
    inArray
        | scInArray ctx = not ("]" `T.isInfixOf` sep)
        | otherwise = protKey && ":[" `T.isPrefixOf` sep
    prot = inArray || (protKey && sep == ":")
    protKey = maybe False (`elem` loadBearingKeys) (scPrev ctx)

{- | Rewrite the eligible quoted spans inside a kept chunk; a span in value
position of a load-bearing key passes verbatim (elision-exempt).
-}
spanPass :: Int -> Text -> EmitLedger -> (EmitLedger, Text)
spanPass turn chunk led0 =
    let (led', segs) = go led0 startCtx chunk
     in (led', T.concat segs)
  where
    go led ctx t = case T.breakOn "\"" t of
        (pre, rest)
            | T.null rest -> (led, [pre])
            | otherwise -> case takeSpan (T.drop 1 rest) of
                Nothing -> (led, [pre, rest])
                Just (raw, after) ->
                    let logical = unescapeSpan raw
                        (prot, ctx') = spanStep ctx pre logical
                        (led2, raw')
                            | prot = (led, raw)
                            | otherwise = spanBlock led raw
                        (led3, segs) = go led2 ctx' after
                     in (led3, pre : "\"" : raw' : "\"" : segs)
    spanBlock led raw
        | T.length logical < blockFloor = (led, raw)
        | otherwise = case rewriteBlock turn logical led of
            (led', Just replacement) -> (led', escapeSpan replacement)
            (led', Nothing) -> (led', raw)
      where
        logical = unescapeSpan raw

{- | The logical texts of a chunk's eligible quoted spans, protected values
excluded — they neither dedup nor establish a stub identity.
-}
spansOf :: Text -> [Text]
spansOf = go startCtx
  where
    go ctx t = case T.breakOn "\"" t of
        (pre, rest)
            | T.null rest -> []
            | otherwise -> case takeSpan (T.drop 1 rest) of
                Nothing -> []
                Just (raw, after) ->
                    let logical = unescapeSpan raw
                        (prot, ctx') = spanStep ctx pre logical
                        more = go ctx' after
                     in if not prot && T.length logical >= blockFloor
                            then logical : more
                            else more

-- | Scan a quoted span's body up to its closing unescaped quote.
takeSpan :: Text -> Maybe (Text, Text)
takeSpan = go []
  where
    go acc t = case T.uncons t of
        Nothing -> Nothing
        Just ('\\', rest) -> case T.uncons rest of
            Just (c, rest') -> go (c : '\\' : acc) rest'
            Nothing -> Nothing
        Just ('"', rest) -> Just (T.pack (reverse acc), rest)
        Just (c, rest) -> go (c : acc) rest

-- | Undo JSON string escaping (identity on a non-JSON quoted stretch).
unescapeSpan :: Text -> Text
unescapeSpan raw =
    fromMaybe raw (decode (LBS.fromStrict (TE.encodeUtf8 ("\"" <> raw <> "\""))))

-- | JSON-escape a replacement so it can sit inside a quoted span.
escapeSpan :: Text -> Text
escapeSpan t = T.dropEnd 1 (T.drop 1 (TE.decodeUtf8 (LBS.toStrict (encode t))))

{- | The dedup-eligible blocks of a content text: paragraphs and embedded
quoted spans (logical form) at or above 'blockFloor'.
-}
eligibleBlocks :: Text -> [Text]
eligibleBlocks text =
    concat
        [ chunk : spansOf chunk
        | chunk <- T.splitOn "\n\n" text
        , T.length chunk >= blockFloor
        ]

{- | Dedup a batch of injected messages' @content@ fields in place, EXCEPT
where the message answers a tool whose whole purpose is to hand content back
('contentRequest'). Those are still recorded, so later incidental echoes of
them can back-reference.
-}
dedupInjected :: IORef EmitLedger -> Int -> [Value] -> IO [Value]
dedupInjected ref turn = mapM one
  where
    one (Object o)
        | Just (String c) <- KM.lookup "content" o
        , contentRequest o = do
            atomicModifyIORef' ref $ \led -> (recordText turn c led, ())
            pure (Object o)
        | Just (String c) <- KM.lookup "content" o = do
            c' <-
                atomicModifyIORef' ref $ \led ->
                    let (out, led') = dedupText turn c led in (led', out)
            pure (Object (KM.insert "content" (String c') o))
    one v = pure v

{- | Does this message answer a call that ASKED for content? Re-running or
re-reading a cell is a request for that cell's output, so answering it with a
citation of the output answers the question with the question.

live_test36 printed @Decode error: parse error (Failed reading …)@ from a cell
that SUCCEEDED, so the failure rode in @oiOutput@ and no key-based exemption
applied. The model read the truncated back-reference, said "we need full
message", called @execute_cell@, was handed the same citation, and stopped.

Keyed on the CALLER, not the text: a cell that prints @Total squared error:
0.0@ is reporting a result, and no keyword can separate the two — that
distinction is what the caller's intent supplies.
-}
contentRequest :: KM.KeyMap Value -> Bool
contentRequest o = case KM.lookup "tool_name" o of
    Just (String n) -> n `elem` contentRequestTools
    _ -> False

-- | The tools whose result IS the content the caller asked to see.
contentRequestTools :: [Text]
contentRequestTools = ["execute_cell", "read_cell", "read_cell_output"]

{- | The loop's per-turn seam: record the model's own turn (so injected
echoes of it can back-reference), then dedup the injected tail. Returns
the turn message followed by the rewritten injected messages.
-}
emitTurn :: IORef EmitLedger -> Int -> Value -> [Value] -> IO [Value]
emitTurn ref turn turnMsg injected = do
    atomicModifyIORef' ref $ \led ->
        (recordText turn (TE.decodeUtf8 (LBS.toStrict (encode turnMsg))) led, ())
    (turnMsg :) <$> dedupInjected ref turn injected

{- | Does a cell's own OUTPUT report a failure? Errors are sent in full and
informational output is contracted, but a cell can SUCCEED while printing its
failure, so the failure lands in @oiOutput@ where no @error@ key marks it.

The discriminator is what follows the marker's colon. An error reports prose:

> Decode error: parse error (Failed reading …)

A statistic reports a number, and is not a failure at all:

> Total squared error: 0.0

so @squared error@, @standard error@ and @mean absolute error@ keep
contracting, while a diagnostic the model must read never does
(@live_test36@).
-}
reportsFailure :: Text -> Bool
reportsFailure = any (any failureLine . T.lines) . outputValues
  where
    failureLine l = unambiguous l || markedProse l
    unambiguous l = any (`T.isInfixOf` l) ["*** Exception", "Not in scope:", "CallStack"]
    markedProse l = any (prosePast l) failureMarkers
    prosePast l m = case T.breakOn (m <> ":") (T.toLower l) of
        (_, rest)
            | T.null rest -> False
            | otherwise -> isProse (T.drop (T.length m + 1) rest)
    isProse rest = case T.uncons (T.stripStart rest) of
        Just (c, _) -> not (isDigit c) && c /= '-'
        Nothing -> False

{- | Words that head a failure report. Each must still be followed by a colon
and prose to count, so a metric that merely contains one does not qualify.
-}
failureMarkers :: [Text]
failureMarkers = ["error", "exception", "failure", "failed", "warning"]

{- | The @oiOutput@ string values in a chunk. Scanned as text, never re-parsed
as JSON: this runs on every injected message.
-}
outputValues :: Text -> [Text]
outputValues = go
  where
    key = "\"oiOutput\":\""
    go t = case T.breakOn key t of
        (_, rest)
            | T.null rest -> []
            | otherwise ->
                let body = T.drop (T.length key) rest
                    (v, after) = T.breakOn "\"" (T.replace "\\\"" "  " body)
                 in v : (if T.null after then [] else go (T.drop 1 after))
