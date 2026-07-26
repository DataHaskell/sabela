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

data EmitLedger = EmitLedger
    { elSeen :: Map Text Int
    , elAnchor :: Map Text (Int, Text)
    }

blockFloor :: Int
blockFloor = 160

backRefLimit :: Int
backRefLimit = 120

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

actionableKeys :: [Text]
actionableKeys = ["diagnostic", "error", "stderr", "autofix"]

emptyEmitLedger :: EmitLedger
emptyEmitLedger = EmitLedger Map.empty Map.empty

newEmitLedger :: IO (IORef EmitLedger)
newEmitLedger = newIORef emptyEmitLedger

anchorOf :: Text -> Text
anchorOf = T.strip . T.takeWhile (/= '\n')

recordBlock :: Int -> Text -> EmitLedger -> EmitLedger
recordBlock turn block (EmitLedger seen anch) =
    EmitLedger
        (Map.insertWith (\_ old -> old) block turn seen)
        (Map.insert (anchorOf block) (turn, block) anch)

backRef :: Int -> Text -> Text
backRef turn block =
    "[as established turn "
        <> T.pack (show turn)
        <> " (unchanged): "
        <> T.take 40 (anchorOf block)
        <> "…]"

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
    Just t -> (led, Just (backRef t block))
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
        | carriesActionable chunk || carriesVerdict chunk || reportsFailure chunk =
            (recordBlock turn chunk led, chunk)
        | otherwise = case rewriteBlock turn chunk led of
            (led', Just replacement) -> (led', replacement)
            (led', Nothing) -> spanPass turn chunk led'

carriesActionable :: Text -> Bool
carriesActionable chunk = any nonEmptyValue actionableKeys
  where
    nonEmptyValue k =
        ("\"" <> k <> "\":\"") `T.isInfixOf` chunk
            && not (("\"" <> k <> "\":\"\"") `T.isInfixOf` chunk)

carriesVerdict :: Text -> Bool
carriesVerdict = T.isInfixOf "\"tool_name\":\"verify\""

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
            atomicModifyIORef' ref $ \led -> (recordText turn c led, ())
            pure (Object o)
        | Just (String c) <- KM.lookup "content" o = do
            c' <-
                atomicModifyIORef' ref $ \led ->
                    let (out, led') = dedupText turn c led in (led', out)
            pure (Object (KM.insert "content" (String c') o))
    one v = pure v

contentRequest :: KM.KeyMap Value -> Bool
contentRequest o = case KM.lookup "tool_name" o of
    Just (String n) -> n `elem` contentRequestTools
    _ -> False

contentRequestTools :: [Text]
contentRequestTools = ["execute_cell", "read_cell", "read_cell_output"]

emitTurn :: IORef EmitLedger -> Int -> Value -> [Value] -> IO [Value]
emitTurn ref turn turnMsg injected = do
    atomicModifyIORef' ref $ \led ->
        (recordText turn (TE.decodeUtf8 (LBS.toStrict (encode turnMsg))) led, ())
    (turnMsg :) <$> dedupInjected ref turn injected

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

failureMarkers :: [Text]
failureMarkers = ["error", "exception", "failure", "failed", "warning"]

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
