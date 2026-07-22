{-# LANGUAGE OverloadedStrings #-}

{- | R7-T3 x R8.4: serialisation-in-string and package-hash atoms fail the
lint on VERIFIER tool surfaces (5.2); non-verifier channels and model text
are exempt. Detectors shared with "Sabela.AI.LeakShape".
-}
module Eval.VerifierLeakLint (
    verifierLeakProblems,
    verifierToolSurfaces,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

import Eval.VerdictLint (verifierChannels)
import Sabela.AI.LeakShape (doubleEncodedJson, leakyToken)

{- | Verifier surfaces the model calls directly, plus the harness verifier
channels of "Eval.VerdictLint".
-}
verifierToolSurfaces :: [Text]
verifierToolSurfaces = ["check_type", "list_bindings"] ++ verifierChannels

-- | Each leak on a verifier surface as a @(rule, detail)@ pair.
verifierLeakProblems :: [Value] -> [(Text, Text)]
verifierLeakProblems msgs =
    [ issue
    | m <- msgs
    , strAt "role" m == "tool"
    , strAt "tool_name" m `elem` verifierToolSurfaces
    , issue <- leakIssues (strAt "content" m)
    ]

leakIssues :: Text -> [(Text, Text)]
leakIssues c =
    [ ("verifier-serialisation-in-string", T.take 80 c)
    | doubleEncodedJson c
    ]
        ++ [ ("verifier-package-hash", w)
           | w <- take 3 (filter leakyToken (T.words c))
           ]

strAt :: Text -> Value -> Text
strAt k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
strAt _ _ = ""
