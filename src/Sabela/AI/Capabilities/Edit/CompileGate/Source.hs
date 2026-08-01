{-# LANGUAGE OverloadedStrings #-}

{- | What the caller wrote and what the compiler actually saw. They differ
whenever the harness rewrote the cell, and the difference is the model's to
know: it can only debug a diagnostic it can locate in its own text.
-}
module Sabela.AI.Capabilities.Edit.CompileGate.Source (
    GateSource (..),
    submittedOnly,
    compiledSourcePairs,
) where

import Data.Aeson ((.=))
import Data.Aeson.Types (Pair)
import Data.Text (Text)

data GateSource = GateSource
    { gateSubmitted :: Text
    , gateCompiled :: Text
    }

submittedOnly :: Text -> GateSource
submittedOnly s = GateSource{gateSubmitted = s, gateCompiled = s}

{- | The compiled text, disclosed under its own key only when it is not the
submitted text. With nothing to disclose there is one source and it is the
caller's, which is what @source@ already holds.
-}
compiledSourcePairs :: GateSource -> [Pair]
compiledSourcePairs gsrc
    | gateCompiled gsrc == gateSubmitted gsrc = []
    | otherwise = ["compiledSource" .= gateCompiled gsrc]
