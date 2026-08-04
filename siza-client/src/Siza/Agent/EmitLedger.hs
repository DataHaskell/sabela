{-# LANGUAGE OverloadedStrings #-}

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
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (foldl', mapAccumL)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Sabela.AI.SelfHeal (sourceDelta)
import Siza.Agent.Compact (actionableKeys, mustKeep)
import Siza.Agent.Recall (freshId, recallHint, withRecallStore)

data EmitLedger = EmitLedger
    { elSeen :: Map Text Int
    , elAnchor :: Map Text (Int, Text)
    , elStore :: Map Int Text
    }

blockFloor :: Int
blockFloor = 160

{- | A back-reference must stay well under 'blockFloor', or eliding a block
costs more than keeping it. It also carries the index that reads it back.
-}
backRefLimit :: Int
backRefLimit = 140

{- | Fields whose value is the answer, not a restatement of it. `summary`
belongs here because it is the only content a duplicate envelope carries:
elide it and the reference points at nothing.
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
    , "summary"
    ]
        ++ actionableKeys

emptyEmitLedger :: EmitLedger
emptyEmitLedger = EmitLedger Map.empty Map.empty Map.empty

newEmitLedger :: IO (IORef EmitLedger)
newEmitLedger = newIORef emptyEmitLedger

anchorOf :: Text -> Text
anchorOf = T.strip . T.takeWhile (/= '\n')

recordBlock :: Int -> Text -> EmitLedger -> EmitLedger
recordBlock turn block led =
    led
        { elSeen = Map.insertWith (\_ old -> old) block turn (elSeen led)
        , elAnchor = Map.insert (anchorOf block) (turn, block) (elAnchor led)
        }

{- | The marker that replaces a repeated block, and the ledger that can hand
that block back: one step, so the index the marker names holds those bytes.
-}
backRef :: Int -> Text -> EmitLedger -> (Text, EmitLedger)
backRef turn block led = (marker, led{elStore = Map.insert n block (elStore led)})
  where
    n = freshId (elStore led) block
    marker =
        "[as established turn "
            <> T.pack (show turn)
            <> " (unchanged): "
            <> T.take 40 (anchorOf block)
            <> "… — "
            <> recallHint n
            <> "]"

deltaText :: Int -> Text -> Text -> Text
deltaText turn old new =
    "[changed since turn "
        <> T.pack (show turn)
        <> "]"
        <> T.concat ["\n- " <> l | l <- removed]
        <> T.concat ["\n+ " <> l | l <- added]
  where
    (removed, added) = sourceDelta old new

rewriteBlock :: Int -> Text -> EmitLedger -> (EmitLedger, Maybe Text)
rewriteBlock turn block led = case Map.lookup block (elSeen led) of
    Just t -> let (marker, led') = backRef t block led in (led', Just marker)
    Nothing -> case Map.lookup (anchorOf block) (elAnchor led) of
        Just (t, old)
            | d <- deltaText t old block
            , T.length d < T.length block ->
                (recordBlock turn block led, Just d)
        _ -> (recordBlock turn block led, Nothing)

dedupText :: Int -> Text -> EmitLedger -> (Text, EmitLedger)
dedupText turn text led0 =
    let (led', chunks) = mapAccumL chunkPass led0 (T.splitOn "\n\n" text)
     in (T.intercalate "\n\n" chunks, led')
  where
    chunkPass led chunk
        | T.length chunk < blockFloor = (led, chunk)
        | mustKeep chunk = (recordBlock turn chunk led, chunk)
        | otherwise = case rewriteBlock turn chunk led of
            (led', Just replacement) -> (led', replacement)
            (led', Nothing) -> spanPass turn chunk led'

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

data SpanCtx = SpanCtx {scPrev :: Maybe Text, scInArray :: Bool}

startCtx :: SpanCtx
startCtx = SpanCtx Nothing False

spanStep :: SpanCtx -> Text -> Text -> (Bool, SpanCtx)
spanStep ctx pre logical = (prot, SpanCtx (Just logical) inArray)
  where
    sep = T.filter (not . isSpace) pre
    inArray
        | scInArray ctx = not ("]" `T.isInfixOf` sep)
        | otherwise = protKey && ":[" `T.isPrefixOf` sep
    prot = inArray || (protKey && sep == ":")
    protKey = maybe False (`elem` loadBearingKeys) (scPrev ctx)

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

unescapeSpan :: Text -> Text
unescapeSpan raw =
    fromMaybe raw (decode (LBS.fromStrict (TE.encodeUtf8 ("\"" <> raw <> "\""))))

escapeSpan :: Text -> Text
escapeSpan t = T.dropEnd 1 (T.drop 1 (TE.decodeUtf8 (LBS.toStrict (encode t))))

eligibleBlocks :: Text -> [Text]
eligibleBlocks text =
    concat
        [ chunk : spansOf chunk
        | chunk <- T.splitOn "\n\n" text
        , T.length chunk >= blockFloor
        ]

dedupInjected :: IORef EmitLedger -> Int -> [Value] -> IO [Value]
dedupInjected ref turn = mapM one
  where
    one (Object o)
        | Just (String c) <- KM.lookup "content" o
        , contentRequest o = do
            modifyLedger ref (\led -> ((), recordText turn c led))
            pure (Object o)
        | Just (String c) <- KM.lookup "content" o = do
            c' <- modifyLedger ref (dedupText turn c)
            pure (Object (KM.insert "content" (String c') o))
    one v = pure v

{- | Run a ledger step against the live recall store, so what the step elides
is retrievable by the index its marker names before that marker is emitted.
-}
modifyLedger :: IORef EmitLedger -> (EmitLedger -> (a, EmitLedger)) -> IO a
modifyLedger ref step = do
    led <- readIORef ref
    (out, led') <- withRecallStore $ \store ->
        let (a, l) = step led{elStore = store} in ((a, l), elStore l)
    writeIORef ref led'
    pure out

contentRequest :: KM.KeyMap Value -> Bool
contentRequest o = case KM.lookup "tool_name" o of
    Just (String n) -> n `elem` contentRequestTools
    _ -> False

contentRequestTools :: [Text]
contentRequestTools = ["execute_cell", "read_cell", "read_cell_output"]

emitTurn :: IORef EmitLedger -> Int -> Value -> [Value] -> IO [Value]
emitTurn ref turn turnMsg injected = do
    modifyLedger ref $ \led ->
        ((), recordText turn (TE.decodeUtf8 (LBS.toStrict (encode turnMsg))) led)
    (turnMsg :) <$> dedupInjected ref turn injected
