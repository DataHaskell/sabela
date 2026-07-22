{-# LANGUAGE OverloadedStrings #-}

{- | The @search_capability@ tool: capability discovery over ALL of Hackage. A
plain-language description, a type signature, or a name all work, so the model
can DISCOVER a library it does not yet know before importing it.

Two paths, best-first:

1. The SHIP retriever (high recall, benchmark R@10 ~0.90): shell out to
   @tools/capability_search.hs@, which runs gpt-oss query expansion + nomic
   embedding + BM25 + RRF + popularity rerank over the offline index in @data/@.
   See 'Sabela.AI.Capabilities.CapabilityHelper'. This adds an ollama runtime
   dependency for the high-recall path.
2. Fallback: the LOCAL hoogle lexical query (the previous behaviour) — used when
   the helper is absent, errors, or returns no hits. The public hoogle API is
   never consulted.

RESULT→USE bridge: each top package the SHIP path names is ENRICHED (via
'Sabela.AI.Capabilities.CapabilityApi') with the key exported functions +
signatures + the module to import, plus a paste-able usage example (import line +
a call skeleton for the top export) for the top few packages, so the model can
CALL the library instead of just learning its name.

A NON-kernel tool: no live session needed, usable before the first cell runs. On
any failure it returns an OK outcome with no hits, so the model just sees
"nothing found" rather than an error.
-}
module Sabela.AI.Capabilities.CapabilitySearch (
    execSearchCapability,
    capabilityOutcome,
    enrichedOutcome,
    exactOnlyHits,
    packageBuckets,
    semanticWanted,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.CapabilityApi (
    ApiFn (..),
    enrichPackages,
    usageExample,
 )
import Sabela.AI.Capabilities.CapabilityHelper (
    HelperHit (..),
    runCapabilityHelper,
 )
import Sabela.AI.Capabilities.Util (fieldText)
import Sabela.AI.HoogleResolve (HoogleHit (..), hoogleQuery)
import Sabela.AI.Types (ToolOutcome, okOutcome)
import Sabela.State (App)

-- | How many ranked candidate packages to return to the model.
maxHits :: Int
maxHits = 18

{- | How many top packages to enrich with their key API. 5 (not 3) so a
slightly mis-ranked-but-relevant package still gets its callable API surfaced —
e.g. the QR retriever ranks @qrcode-core@ ~5th, and the encoder is the whole
point of the discovery.
-}
enrichPkgs :: Int
enrichPkgs = 5

-- | How many key exports to surface per enriched package.
perPkgApi :: Int
perPkgApi = 6

{- | How many top packages get a paste-able usage example (import + a call
skeleton from the top export). Kept to the top few so the result stays concise.
-}
examplePkgs :: Int
examplePkgs = 3

-- | Docs/synopsis blurb truncation, in characters.
maxDocsChars :: Int
maxDocsChars = 160

{- | @query@ is a plain-language capability description, a type signature, or a
name. Empty query yields no hits. Tries the SHIP helper first; if it returns no
hits (absent/down/empty) falls back to the local hoogle lexical query. The top
packages are then enriched with their key API. Shells out only — no live session
needed, so it is usable before the first cell is run.

Two optional flags (search-api.md sections 2 and 7): @semantic: false@ skips
the SHIP retriever (the A/B lever gates enrichment only — the lexical channel
always runs); @exact: true@ is the stage-0 exact-name lookup, distinct from
the widened fuzzy scan.
-}
execSearchCapability :: App -> Value -> IO ToolOutcome
execSearchCapability _ input
    | T.null (T.strip q) = pure (enrichedOutcome q [])
    | otherwise = do
        helperHits <-
            if semanticWanted input && not exact
                then runCapabilityHelper maxHits q
                else pure []
        pkgs <-
            if null helperHits
                then packageBuckets . exactFilter <$> hoogleQuery maxHits q
                else pure [(hePackage h, heSynopsis h) | h <- helperHits]
        enriched <- enrichPackages enrichPkgs perPkgApi q pkgs
        pure (enrichedOutcome q enriched)
  where
    q = fieldText "query" input
    exact = boolField "exact" input == Just True
    exactFilter = if exact then exactOnlyHits q else id

-- | The SHIP retriever runs unless the caller passed @semantic: false@.
semanticWanted :: Value -> Bool
semanticWanted input = boolField "semantic" input /= Just False

boolField :: Text -> Value -> Maybe Bool
boolField k v = case v of
    Object o | Just (Bool b) <- KM.lookup (Key.fromText k) o -> Just b
    _ -> Nothing

{- | Stage 0 (section 7): the exact subset of a hoogle answer — name, module
or package equality, a lookup rather than a ranking. Package equality is what
lets a prose term naming a package reach its symbols (section 2).
-}
exactOnlyHits :: Text -> [HoogleHit] -> [HoogleHit]
exactOnlyHits q = filter match
  where
    lq = T.toLower q
    match h =
        hhName h == q
            || hhModule h == q
            || T.toLower (hhPackage h) == lq

{- | Collapse symbol-level hoogle hits (the lexical fallback) into per-package
buckets, preserving first-seen order. The synopsis is the first hit's docs blurb.
The fallback's own symbol hits seed the enrichment, so this keeps the order.
-}
packageBuckets :: [HoogleHit] -> [(Text, Text)]
packageBuckets hits =
    [ (p, synOf p)
    | p <- nub (map hhPackage hits)
    , not (T.null p)
    ]
  where
    synOf p = case filter (\h -> hhPackage h == p) hits of
        (h : _) -> hhDocs h
        [] -> ""

{- | Shape enriched packages into
@{query, hits:[{package, synopsis, cabal, modules, api:[{name,module,type}], example}]}@.
The @cabal@ line is the @-- cabal: build-depends:@ string the model pastes as a
cell's first line; @modules@ are the distinct modules to import; @api@ is the key
exports with signatures; @example@ is a paste-able usage skeleton (import + a
call wiring the top export) for the top 'examplePkgs' packages, so the model sees
HOW to call the library, not just its name. Always an OK outcome; an empty hit
list is a valid "nothing found".
-}
enrichedOutcome :: Text -> [(Text, Text, [ApiFn])] -> ToolOutcome
enrichedOutcome q hits =
    okOutcome $
        object (["query" .= q, "hits" .= zipWith hitJSON [0 ..] hits] <> nameFallback)
  where
    -- No index hit: a single-token query is usually the package name itself
    -- (megaparsec, fgl). Suggest declaring it — hoogle indexes definitions, not
    -- package names, and a wrong name is a clean, recoverable compile error.
    nameFallback
        | not (null hits) = []
        | T.null qs || T.any (== ' ') qs = []
        | otherwise = ["tryCabal" .= ("-- cabal: build-depends: " <> qs)]
    qs = T.strip q
    hitJSON i (pkg, syn, api) =
        object $
            [ "package" .= pkg
            , "synopsis" .= truncDocs syn
            , "cabal" .= ("-- cabal: build-depends: " <> pkg)
            , "modules" .= nub (filter (not . T.null) (map afModule api))
            , "api" .= map apiJSON api
            ]
                ++ ["example" .= ex | i < examplePkgs, ex <- [usageExample api], not (T.null ex)]
    apiJSON a =
        object
            [ "name" .= afName a
            , "module" .= afModule a
            , "type" .= afType a
            ]
    truncDocs d
        | T.length d <= maxDocsChars = d
        | otherwise = T.take maxDocsChars d <> "…"

{- | The prior @{query, hits:[{name, type, module, package, docs}]}@ shaping,
kept for tests that pin the flat symbol-level shape. Always an OK outcome.
-}
capabilityOutcome :: Text -> [HoogleHit] -> ToolOutcome
capabilityOutcome q hits =
    okOutcome $ object ["query" .= q, "hits" .= map hitJSON hits]
  where
    hitJSON h =
        object
            [ "name" .= hhName h
            , "type" .= hhType h
            , "module" .= hhModule h
            , "package" .= hhPackage h
            , "docs" .= truncDocs (hhDocs h)
            ]
    truncDocs d
        | T.length d <= maxDocsChars = d
        | otherwise = T.take maxDocsChars d <> "…"
