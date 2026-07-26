{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Futility (
    FutilityGuard,
    newFutilityGuard,
    guardDispatch,
    futilityNote,
    sourceFaultNote,
    noteFor,
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

sourceFaultNote :: Text
sourceFaultNote =
    "This exact source was rejected before with the identical diagnostic. It \
    \is deterministic: the fault is in the source, not the kernel or the \
    \environment. Read the diagnostic above and change the source it names."

noteFor :: Either Text ToolOutcome -> Text
noteFor out
    | deterministicRejection out = sourceFaultNote
    | otherwise = futilityNote

deterministicRejection :: Either Text ToolOutcome -> Bool
deterministicRejection (Right (ToolErr (Object o))) =
    KM.member (K.fromText "refusal") o
        || KM.lookup (K.fromText "verdict") o == Just (String "diagnostic")
        || KM.member (K.fromText "diagnostic") o
deterministicRejection _ = False

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
            pure (if prev == Just ft then annotate (noteFor out) out else out)

callKey :: ToolCall -> (Text, Text)
callKey (ToolCall n a) = (n, encodeText a)

failureText :: Either Text ToolOutcome -> Maybe Text
failureText (Left e) = Just e
failureText (Right (ToolErr v)) = Just (encodeText v)
failureText (Right (ToolOk _)) = Nothing

annotate :: Text -> Either Text ToolOutcome -> Either Text ToolOutcome
annotate n (Left e) = Left (e <> " " <> n)
annotate n (Right (ToolErr (Object o))) =
    Right (ToolErr (Object (KM.insert (K.fromText "futility") (String n) o)))
annotate n (Right (ToolErr v)) =
    Right (ToolErr (object ["error" .= v, "futility" .= String n]))
annotate _ ok = ok

encodeText :: Value -> Text
encodeText = TE.decodeUtf8With TEE.lenientDecode . LBS.toStrict . encode
