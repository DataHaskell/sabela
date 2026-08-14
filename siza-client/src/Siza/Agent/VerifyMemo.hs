{- |
Technique: repeated-verify memo [Episode].
Guarantee: a memoised pass is returned only under a seal proving no cell source and no kernel generation changed since the run that earned it; only a pass is ever memoised.
Entry: 'memoHit' / 'memoRecord', wired in Siza.Agent.Stack.Call.
-}
module Siza.Agent.VerifyMemo (
    VerifyMemo,
    Seal,
    newVerifyMemo,
    verifyCheckOf,
    currentSeal,
    memoHit,
    memoRecord,
) where

import Data.Aeson (Value (..), object)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))

{- | What must be identical for a past pass to still hold: every cell's
source hash, and the kernel generation (a restart resets the bindings behind
unchanged sources).
-}
data Seal = Seal
    { sealCells :: [(Int, Text)]
    , sealKernel :: Value
    }
    deriving (Eq, Show)

newtype VerifyMemo = VerifyMemo (IORef (Map Text (Seal, Value)))

newVerifyMemo :: IO VerifyMemo
newVerifyMemo = VerifyMemo <$> newIORef Map.empty

-- | The check a verify call carries, when it carries one worth keying on.
verifyCheckOf :: ToolCall -> Maybe Text
verifyCheckOf call
    | tcName call == "verify"
    , Object o <- tcArgs call
    , Just (String c) <- KM.lookup (K.fromText "check") o
    , not (T.null (T.strip c)) =
        Just c
    | otherwise = Nothing

{- | The seal as the notebook stands now, from the two cheapest probes. A
probe that fails yields no seal, and no seal means no memo either way.
-}
currentSeal ::
    (ToolCall -> IO (Either Text ToolOutcome)) -> IO (Maybe Seal)
currentSeal disp = do
    cells <- disp (ToolCall "list_cells" (object []))
    status <- disp (ToolCall "kernel_status" (object []))
    pure (Seal <$> cellHashes cells <*> kernelGen status)

cellHashes :: Either Text ToolOutcome -> Maybe [(Int, Text)]
cellHashes (Right (ToolOk (Object o)))
    | Just (Array cs) <- KM.lookup (K.fromText "cells") o =
        traverse cellHash (foldr (:) [] cs)
cellHashes _ = Nothing

cellHash :: Value -> Maybe (Int, Text)
cellHash (Object c)
    | Just (Number i) <- KM.lookup (K.fromText "id") c
    , Just (String h) <- KM.lookup (K.fromText "hash") c =
        Just (round i, h)
cellHash _ = Nothing

kernelGen :: Either Text ToolOutcome -> Maybe Value
kernelGen (Right (ToolOk (Object o))) = KM.lookup (K.fromText "ksGen") o
kernelGen _ = Nothing

memoHit :: VerifyMemo -> Text -> Seal -> IO (Maybe Value)
memoHit (VerifyMemo ref) check seal = do
    m <- readIORef ref
    pure $ case Map.lookup check m of
        Just (recorded, payload) | recorded == seal -> Just (unchanged payload)
        _ -> Nothing

-- | Only a pass is worth remembering: a fail or refusal is retried after work.
memoRecord :: VerifyMemo -> Text -> Seal -> Either Text ToolOutcome -> IO ()
memoRecord (VerifyMemo ref) check seal (Right (ToolOk payload@(Object o)))
    | KM.lookup (K.fromText "verdict") o == Just (String "pass") =
        atomicModifyIORef' ref (\m -> (Map.insert check (seal, payload) m, ()))
memoRecord _ _ _ _ = pure ()

unchanged :: Value -> Value
unchanged (Object o) =
    Object (KM.insert (K.fromText "unchanged") (String unchangedNote) o)
unchanged v = v

unchangedNote :: Text
unchangedNote =
    "this exact check passed earlier this session, and every cell's source \
    \hash and the kernel generation are identical now, so the scratch run \
    \was not repeated"
