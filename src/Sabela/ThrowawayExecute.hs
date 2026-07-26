{-# LANGUAGE OverloadedStrings #-}

module Sabela.ThrowawayExecute (
    ExecuteVerdict (..),
    ExecuteReason (..),
    SandboxStatus (..),
    ContainmentStatus (..),
    ExecuteQualification (..),
    ExecuteResult (..),
    executeFlagEnabled,
    admitExecute,
    encodeFrame,
    decodeFrame,
) where

import Data.Char (isDigit, toLower)
import Data.Text (Text)
import qualified Data.Text as T

data ExecuteVerdict
    = ExecuteOk
    | ExecuteCompileError
    | ExecuteTimedOut
    | ExecuteUnavailable
    deriving (Eq, Show)

data ExecuteReason
    = HelperVersionMismatch
    | ContainmentNotProven
    deriving (Eq, Show)

data SandboxStatus = SandboxMacOS | SandboxMissing
    deriving (Eq, Show)

data ContainmentStatus = ContainmentProven | ContainmentUnproven
    deriving (Eq, Show)

data ExecuteQualification = ExecuteQualification
    { qualificationNotebookGhc :: String
    , qualificationHelperGhc :: String
    , qualificationSandbox :: SandboxStatus
    , qualificationContainment :: ContainmentStatus
    }
    deriving (Eq, Show)

data ExecuteResult = ExecuteResult
    { executeVerdict :: ExecuteVerdict
    , executeReason :: Maybe ExecuteReason
    }
    deriving (Eq, Show)

executeFlagEnabled :: Maybe String -> Bool
executeFlagEnabled = maybe False ((`notElem` ["0", "off", "false", "no", ""]) . map toLower)

admitExecute :: ExecuteQualification -> IO () -> IO ExecuteResult
admitExecute qualification runCandidate
    | qualificationNotebookGhc qualification /= qualificationHelperGhc qualification =
        pure (ExecuteResult ExecuteUnavailable (Just HelperVersionMismatch))
    | qualificationSandbox qualification == SandboxMissing
        || qualificationContainment qualification /= ContainmentProven =
        pure (ExecuteResult ExecuteUnavailable (Just ContainmentNotProven))
    | otherwise = do
        runCandidate
        pure (ExecuteResult ExecuteOk Nothing)

encodeFrame :: Text -> Text
encodeFrame payload = T.pack (show (T.length payload)) <> "\n" <> payload

decodeFrame :: Text -> Either Text Text
decodeFrame frame = case T.breakOn "\n" frame of
    (sizeText, rest)
        | T.null rest -> Left "missing frame delimiter"
        | T.null sizeText || T.any (not . isDigit) sizeText ->
            Left "invalid frame length"
        | otherwise ->
            let wanted = read (T.unpack sizeText) :: Int
                payload = T.drop 1 rest
             in if T.length payload == wanted
                    then Right payload
                    else Left "frame length mismatch"
