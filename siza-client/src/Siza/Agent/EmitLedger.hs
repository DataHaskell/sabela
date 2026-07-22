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
import Data.Char (isSpace)
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
    ["type", "signature", "use", "cabal", "name", "next", "exports", "candidate"]

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
        | otherwise = case rewriteBlock turn chunk led of
            (led', Just replacement) -> (led', replacement)
            (led', Nothing) -> spanPass turn chunk led'

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

-- | Dedup a batch of injected messages' @content@ fields in place.
dedupInjected :: IORef EmitLedger -> Int -> [Value] -> IO [Value]
dedupInjected ref turn = mapM one
  where
    one (Object o)
        | Just (String c) <- KM.lookup "content" o = do
            c' <-
                atomicModifyIORef' ref $ \led ->
                    let (out, led') = dedupText turn c led in (led', out)
            pure (Object (KM.insert "content" (String c') o))
    one v = pure v

{- | The loop's per-turn seam: record the model's own turn (so injected
echoes of it can back-reference), then dedup the injected tail. Returns
the turn message followed by the rewritten injected messages.
-}
emitTurn :: IORef EmitLedger -> Int -> Value -> [Value] -> IO [Value]
emitTurn ref turn turnMsg injected = do
    atomicModifyIORef' ref $ \led ->
        (recordText turn (TE.decodeUtf8 (LBS.toStrict (encode turnMsg))) led, ())
    (turnMsg :) <$> dedupInjected ref turn injected
