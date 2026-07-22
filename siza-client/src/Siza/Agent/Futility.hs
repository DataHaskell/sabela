{-# LANGUAGE OverloadedStrings #-}

{- | Retry-futility guard: when a call byte-identical to an earlier one fails
with the identical error, the response says so and directs the model away from
payload rewriting — the second identical failure proves the payload is not the
fault. A success clears the memory for that call.
-}
module Siza.Agent.Futility (
    FutilityGuard,
    newFutilityGuard,
    guardDispatch,
    futilityNote,
) where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))

-- | Remembered failures keyed by the exact (tool, arguments) bytes.
newtype FutilityGuard = FutilityGuard (IORef (Map (Text, Text) Text))

newFutilityGuard :: IO FutilityGuard
newFutilityGuard = FutilityGuard <$> newIORef Map.empty

futilityNote :: Text
futilityNote =
    "This call was byte-identical to an earlier call and failed with the \
    \identical error. Re-sending or re-phrasing the same payload will not \
    \change the outcome - the payload is not the fault. Change approach: \
    \check kernel_status / list_cells, use a different tool, or take a \
    \smaller step."

{- | Wrap a dispatch: pass outcomes through untouched, except that a repeat of
an identically-failing identical call gains the 'futilityNote'.
-}
guardDispatch ::
    FutilityGuard ->
    (ToolCall -> IO (Either Text ToolOutcome)) ->
    ToolCall ->
    IO (Either Text ToolOutcome)
guardDispatch (FutilityGuard ref) dispatch call = do
    out <- dispatch call
    let key = callKey call
    case failureText out of
        Nothing -> do
            atomicModifyIORef' ref (\m -> (Map.delete key m, ()))
            pure out
        Just ft -> do
            prev <-
                atomicModifyIORef'
                    ref
                    (\m -> (Map.insert key ft m, Map.lookup key m))
            pure (if prev == Just ft then annotate out else out)

callKey :: ToolCall -> (Text, Text)
callKey (ToolCall n a) = (n, encodeText a)

failureText :: Either Text ToolOutcome -> Maybe Text
failureText (Left e) = Just e
failureText (Right (ToolErr v)) = Just (encodeText v)
failureText (Right (ToolOk _)) = Nothing

annotate :: Either Text ToolOutcome -> Either Text ToolOutcome
annotate (Left e) = Left (e <> " " <> futilityNote)
annotate (Right (ToolErr (Object o))) =
    Right (ToolErr (Object (KM.insert (K.fromText "futility") note o)))
annotate (Right (ToolErr v)) =
    Right (ToolErr (object ["error" .= v, "futility" .= note]))
annotate ok = ok

note :: Value
note = String futilityNote

encodeText :: Value -> Text
encodeText = TE.decodeUtf8With TEE.lenientDecode . LBS.toStrict . encode
