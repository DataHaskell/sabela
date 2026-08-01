{-# LANGUAGE OverloadedStrings #-}

{- | 'Sabela.AI.FitRule' decides freeness from a hole fit GHC wrote. An index
answer carries the same two facts — a name and a signature — but no type
applications, so the law is applied to it here rather than restated: a
restatement would drift from the oracle the compiler-backed path uses.
-}
module Sabela.AI.FitSignature (
    Freeness (..),
    classifyRow,
) where

import Data.Text (Text)

import Sabela.AI.FitRule (Freeness (..), classifyFit)
import Sabela.AI.HoleFits (HoleFit (..))

{- | The freeness of an index row. With no applications recorded, the
constraint-discharged-at-a-function clause cannot fire, which is correct: an
index row states no applications.
-}
classifyRow :: Text -> Text -> Freeness
classifyRow name sig = classifyFit (HoleFit name sig False Nothing [])
