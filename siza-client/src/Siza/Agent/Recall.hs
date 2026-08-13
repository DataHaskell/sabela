{-# LANGUAGE OverloadedStrings #-}

{- |
Technique: dropped-result escrow [Context Economy].
Guarantee: an elided result stays readable by its stub id from a process-global store.
Entry: 'answerRecall'. Trap: multi-episode drivers must call 'resetRecallStore'.
-}
module Siza.Agent.Recall (
    answerRecall,
    freshId,
    readRecall,
    recallHint,
    recallHintShape,
    recallOutcome,
    recallResult,
    recallToolDef,
    recallToolName,
    resetRecallStore,
    resultId,
    withRecallStore,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Bits (xor)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import System.IO.Unsafe (unsafePerformIO)

import Sabela.AI.Types (ToolOutcome (..))

recallToolName :: Text
recallToolName = "recall_result"

{- | How to read an elided block back, in the words the router answers to.
Every elision path names its index through this, so the shape the tool's own
description quotes is the shape the markers carry.
-}
recallHint :: Int -> Text
recallHint = recallHintFor . tshow

-- | 'recallHint' with @N@ where the index goes: what a description may quote.
recallHintShape :: Text
recallHintShape = recallHintFor "N"

recallHintFor :: Text -> Text
recallHintFor idx =
    "read it with " <> recallToolName <> " {\"index\": " <> idx <> "}"

{- | An index derived from the content, so two producers naming the same bytes
name the same index and a later prompt cannot reuse an earlier one.
-}
resultId :: Text -> Int
resultId t = 1 + fromIntegral (T.foldl' step 2166136261 t `mod` 999983)
  where
    step :: Word32 -> Char -> Word32
    step h c = (h `xor` fromIntegral (fromEnum c)) * 16777619

{- | The content's own index, moved on only if those bytes are not already
stored there. Every producer mints with this, so no insertion displaces bytes
another marker already names.
-}
freshId :: Map Int Text -> Text -> Int
freshId store full = probe (resultId full)
  where
    probe i = case Map.lookup i store of
        Just held | held /= full -> probe (i + 1)
        _ -> i

{- | What has been elided since the last 'resetRecallStore', by the index its
markers name. Both elision paths publish here and the recall tool answers from
here, so a marker minted at any depth of the call graph resolves later.
-}
{-# NOINLINE recallStore #-}
recallStore :: IORef (Map Int Text)
recallStore = unsafePerformIO (newIORef Map.empty)

readRecall :: IO (Map Int Text)
readRecall = readIORef recallStore

{- | Drop everything the store holds. A driver that runs several episodes in
one process calls this between them, so no episode can read another's bytes
back and no run grows the store without bound.
-}
resetRecallStore :: IO ()
resetRecallStore = atomicModifyIORef' recallStore (const (Map.empty, ()))

{- | Run an elision pass against the live store and publish what it minted, so
the index a marker names is the index its bytes were stored at.
-}
withRecallStore :: (Map Int Text -> (a, Map Int Text)) -> IO a
withRecallStore f =
    atomicModifyIORef' recallStore (\s -> let (a, s') = f s in (s', a))

-- | Answer a recall against what the store actually holds.
answerRecall :: Value -> IO ToolOutcome
answerRecall args = flip recallOutcome args <$> readRecall

{- | A miss is an error: a reference the model cannot follow is a failed read,
and reporting it as a result would make the failure look like evidence.
-}
recallOutcome :: Map Int Text -> Value -> ToolOutcome
recallOutcome store args = case recallResult store args of
    Right full -> ToolOk (object ["result" .= full])
    Left miss -> ToolErr (object ["error" .= miss])

-- | The bytes an index names, or why there are none.
recallResult :: Map Int Text -> Value -> Either Text Text
recallResult store args = case indexArg of
    Nothing ->
        Left
            ( recallToolName
                <> " needs an integer `index`: the N from an elision marker's \""
                <> recallHintShape
                <> "\"."
            )
    Just n -> maybe (Left (missText n)) Right (Map.lookup n store)
  where
    missText n
        | Map.null store =
            "No result #" <> tshow n <> ": nothing has been elided yet."
        | otherwise =
            "No result #"
                <> tshow n
                <> ". "
                <> tshow (Map.size store)
                <> " elided result(s) are readable, by index: "
                <> T.intercalate ", " (map tshow (take listedKeys (Map.keys store)))
                <> (if Map.size store > listedKeys then ", …" else "")
                <> "."
    indexArg = case args of
        Object o -> case KM.lookup (K.fromText "index") o of
            Just (Number s) -> Just (round s)
            Just (String t) -> readIndex t
            _ -> Nothing
        _ -> Nothing

{- | How many indices a miss lists. The store grows all episode, so naming
every key would cost more context than the result the model asked for.
-}
listedKeys :: Int
listedKeys = 12

readIndex :: Text -> Maybe Int
readIndex t = case reads (T.unpack (T.strip t)) of
    [(n, "")] -> Just n
    _ -> Nothing

recallToolDef :: Value
recallToolDef =
    object
        [ "type" .= ("function" :: Text)
        , "function"
            .= object
                [ "name" .= recallToolName
                , "description"
                    .= ( "Read an elided tool result back in full, by the index \
                         \its marker names. A result that was dropped to leave \
                         \room is replaced by a marker carrying \""
                            <> recallHintShape
                            <> "\"; pass that N here."
                       )
                , "parameters"
                    .= object
                        [ "type" .= ("object" :: Text)
                        , "properties"
                            .= object
                                [ "index"
                                    .= object
                                        [ "type" .= ("integer" :: Text)
                                        , "description"
                                            .= ( "The N from the marker's \""
                                                    <> recallHintShape
                                                    <> "\"."
                                               )
                                        ]
                                ]
                        , "required" .= (["index"] :: [Text])
                        ]
                ]
        ]

tshow :: (Show a) => a -> Text
tshow = T.pack . show
