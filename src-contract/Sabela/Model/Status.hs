{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The kernel's status vocabulary. Each value has a tag a client branches on
and a message for a human; only the tag is a contract.
-}
module Sabela.Model.Status (
    SessionStatus (..),
    statusTag,
    statusMessage,
    KernelPhase (..),
    kernelPhaseTag,
) where

import Data.Text (Text)
import qualified Data.Text as T

data SessionStatus
    = SReset
    | SCrashed
    | SUpdateDeps [Text]
    | SDepsUpToDate
    | SStarting
    | SReady
    deriving (Eq)

{- | What a client branches on. The message beside it is prose for a human and
may be reworded freely; this tag is the contract.
-}
statusTag :: SessionStatus -> Text
statusTag SReady = "ready"
statusTag SReset = "reset"
statusTag SCrashed = "crashed"
statusTag (SUpdateDeps _) = "installing"
statusTag SStarting = "starting"
statusTag SDepsUpToDate = "depsUpToDate"

statusMessage :: SessionStatus -> Text
statusMessage SReady = "ready"
statusMessage SReset = "reset"
statusMessage SCrashed = "crashed"
statusMessage (SUpdateDeps deps) = "installing: " <> T.intercalate ", " deps
statusMessage SStarting = "starting session"
statusMessage SDepsUpToDate = "dependencies up to date"

instance Show SessionStatus where
    show :: SessionStatus -> String
    show = T.unpack . statusMessage

{- | Where a kernel failure happened. A build that ran out of time is worth
retrying at a longer budget; one that failed to compile is not.
-}
data KernelPhase
    = KpBuildTimeout
    | KpBuildFailed
    | KpPreludeFailed
    | KpCrashed
    deriving (Eq, Show)

kernelPhaseTag :: KernelPhase -> Text
kernelPhaseTag KpBuildTimeout = "buildTimeout"
kernelPhaseTag KpBuildFailed = "buildFailed"
kernelPhaseTag KpPreludeFailed = "preludeFailed"
kernelPhaseTag KpCrashed = "crashed"
