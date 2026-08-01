{-# LANGUAGE OverloadedStrings #-}

{- | The identity a live scratchpad session may be reused under: every part of
the notebook's metadata its project was built from, not the caller's dependency
list, which a notebook can leave alone while changing the project under it.
-}
module Sabela.AI.Capabilities.Scratchpad.Key (
    ScratchpadPlan (..),
    metaFields,
    scratchpadIdentity,
    scratchpadKey,
    scratchpadPlan,
) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import qualified ScriptHs.Parser as Scripths

import Sabela.AI.Types (ScratchpadSession (..))
import Sabela.Deps (collectMetadata, mergedMeta)
import Sabela.Model (Notebook)
import Sabela.SessionTypes (CellLang (..), SessionBackend)

{- | What a request may do with the live scratchpad: reuse it, or build a fresh
one from this metadata and remember it under this key.
-}
data ScratchpadPlan
    = ReuseScratchpad SessionBackend
    | FreshScratchpad Scripths.CabalMeta [Text]

{- | The single derivation a scratchpad's reuse and its stored identity share,
so a session is looked up by the identity it was stored under.
-}
scratchpadPlan ::
    CellLang ->
    Set Text ->
    Notebook ->
    [Text] ->
    Maybe ScratchpadSession ->
    ScratchpadPlan
scratchpadPlan lang globalDeps nb deps mSp
    | Just sp <- mSp
    , spLang sp == lang
    , spDeps sp == key =
        ReuseScratchpad (spBackend sp)
    | otherwise = FreshScratchpad meta key
  where
    (meta, key) = scratchpadIdentity lang globalDeps nb deps

{- | The metadata a scratchpad's project is generated from, and the key its
session is reusable under. Derived before the cache is consulted, because a
notebook can change the project without changing the caller's dependency list.
-}
scratchpadIdentity ::
    CellLang -> Set Text -> Notebook -> [Text] -> (Scripths.CabalMeta, [Text])
scratchpadIdentity Haskell globalDeps nb deps =
    (meta, scratchpadKey deps (metaFields meta))
  where
    meta = mergedMeta (globalDeps <> S.fromList deps) (collectMetadata nb)
scratchpadIdentity _ _ _ deps =
    (Scripths.mergeMetas [], scratchpadKey deps [])

{- | A canonical fingerprint: values sorted, deduplicated, trimmed and labelled
by the directive they came from, so reordering or repeating a declaration is
the same key, and a value moving between directives is not.
-}
scratchpadKey :: [Text] -> [(Text, [Text])] -> [Text]
scratchpadKey deps fields =
    [ label <> ": " <> v
    | (label, vs) <- M.toAscList grouped
    , v <- S.toAscList vs
    ]
  where
    grouped =
        M.fromListWith
            S.union
            [ (label, S.fromList (clean vs))
            | (label, vs) <- (depsField, deps) : fields
            ]
    clean = filter (not . T.null) . map T.strip

depsField :: Text
depsField = "build-depends"

-- | Every metadata field the scratchpad's project is generated from.
metaFields :: Scripths.CabalMeta -> [(Text, [Text])]
metaFields m =
    [ (depsField, Scripths.metaDeps m)
    , ("default-extensions", Scripths.metaExts m)
    , ("ghc-options", Scripths.metaGhcOptions m)
    , ("extra-lib-dirs", Scripths.metaExtraLibDirs m)
    , ("extra-include-dirs", Scripths.metaExtraIncludeDirs m)
    , ("packages", Scripths.metaPackages m)
    , ("source-repository-package", map pinKey (Scripths.metaSourceRepos m))
    ]

pinKey :: Scripths.SourceRepoPin -> Text
pinKey p =
    T.unwords
        [ Scripths.srpLocation p
        , Scripths.srpRef p
        , fromMaybe "" (Scripths.srpSubdir p)
        ]
