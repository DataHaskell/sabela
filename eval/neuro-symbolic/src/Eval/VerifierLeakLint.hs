{-# LANGUAGE OverloadedStrings #-}

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

verifierToolSurfaces :: [Text]
verifierToolSurfaces = ["check_type", "list_bindings"] ++ verifierChannels

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
