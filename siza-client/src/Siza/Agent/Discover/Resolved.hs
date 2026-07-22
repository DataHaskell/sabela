{-# LANGUAGE OverloadedStrings #-}

{- | Compiler-proof helpers for the resolved-names ledger (R7-T1, search-api
3.3/11): names a clean @check_type@ or a landed compile proved cancel a
lexical not_found in every mode/filter. Keyed on diagnostic class only.
-}
module Siza.Agent.Discover.Resolved (
    provenNames,
    resolvedWhy,
) where

import Data.Char (isAlpha, isAlphaNum)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

{- | The identifier tokens of a proven expression or cell source, each
qualified token contributing its components too (@D.colList@ proves
@D.colList@, @D@ and @colList@) — the compiler accepted every one of them.
-}
provenNames :: Text -> [Text]
provenNames src =
    nub (concatMap withComponents (filter ident (T.split (not . identChar) src)))
  where
    identChar c = isAlphaNum c || c `elem` ("_'." :: String)
    ident t = maybe False (\(c, _) -> isAlpha c || c == '_') (T.uncons t)
    withComponents t = t : filter (not . T.null) (T.splitOn "." t)

-- | Why a denial of a compiler-proven name is blocked (blockedDenial wording).
resolvedWhy :: Text
resolvedWhy =
    "compiler-proven in this session (a clean check_type or a landed compile \
    \resolved it) — the type checker outranks the lexical index"
