{-# LANGUAGE OverloadedStrings #-}

{- | Detect a source-level typed-hole probe. Separate from "Sabela.AI.HoleRepair"
(which reasons about GHC's hole-fit DIAGNOSTICS) because this reasons about
the CANDIDATE SOURCE itself, and is used at a different seam (G2's cascade
entry guard, before any diagnostic exists).
-}
module Sabela.AI.TypedHole (containsTypedHole) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

{- | A standalone @_@ token immediately (modulo whitespace) followed by @::@ —
a typed-hole probe (@_ :: Point@), the harness's deliverable diagnostic (G3).
G2: self_heal must not touch a cell holding one; the diagnostic IS the
answer, not an error to paper over.
-}
containsTypedHole :: Text -> Bool
containsTypedHole = go . T.groupBy sameClass
  where
    sameClass a b = isHoleIdent a == isHoleIdent b
    isHoleIdent c = isAlphaNum c || c == '_' || c == '\''
    -- A non-ident run mixes whitespace and punctuation into one token
    -- (`groupBy` only tells idents from non-idents apart), so re-join
    -- everything after `_` and strip leading space before testing `::`.
    go (tok : rest)
        | tok == "_" = "::" `T.isPrefixOf` T.stripStart (T.concat rest) || go rest
        | otherwise = go rest
    go [] = False
