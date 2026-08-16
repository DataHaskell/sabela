{- | The identity a live scratchpad session may be reused under: every part of
the notebook's metadata its project was built from, not the caller's dependency
list, which a notebook can leave alone while changing the project under it.
-}
module Sabela.AI.Capabilities.Scratchpad.Key (
    ScratchpadPlan (..),
    scratchpadIdentity,
    scratchpadKey,
    scratchpadPlan,
) where

import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import qualified ScriptHs.Parser as Scripths

import Sabela.AI.Types (ScratchpadSession (..))
import Sabela.Deps (collectMetadata, mergedMeta)
import Sabela.Model (Notebook)
import Sabela.Session.EnvKey (canonicalKeyText, resolveLocalPackages)
import Sabela.SessionTypes (CellLang (..), SessionBackend)

{- | What a request may do with the live scratchpad: reuse it, or build a fresh
one from this metadata and remember it under this key.
-}
data ScratchpadPlan
    = ReuseScratchpad SessionBackend
    | FreshScratchpad Scripths.CabalMeta [FilePath] [Text]

{- | The single derivation a scratchpad's reuse and its stored identity share,
so a session is looked up by the identity it was stored under.
-}
scratchpadPlan ::
    Text ->
    FilePath ->
    [FilePath] ->
    CellLang ->
    Set Text ->
    Notebook ->
    [Text] ->
    Maybe ScratchpadSession ->
    ScratchpadPlan
scratchpadPlan ghcVersion workDir envLocals lang globalDeps nb deps mSp
    | Just sp <- mSp
    , spLang sp == lang
    , spDeps sp == key =
        ReuseScratchpad (spBackend sp)
    | otherwise = FreshScratchpad meta localPkgs key
  where
    (meta, localPkgs, key) =
        scratchpadIdentity ghcVersion workDir envLocals lang globalDeps nb deps

{- | The metadata a scratchpad's project is generated from, and the key its
session is reusable under. Derived before the cache is consulted, because a
notebook can change the project without changing the caller's dependency list.
-}
scratchpadIdentity ::
    Text ->
    FilePath ->
    [FilePath] ->
    CellLang ->
    Set Text ->
    Notebook ->
    [Text] ->
    (Scripths.CabalMeta, [FilePath], [Text])
scratchpadIdentity ghcVersion workDir envLocals Haskell globalDeps nb deps =
    (meta, localPkgs, scratchpadKey ghcVersion localPkgs deps meta)
  where
    meta = mergedMeta (globalDeps <> S.fromList deps) (collectMetadata nb)
    localPkgs = resolveLocalPackages workDir envLocals meta
scratchpadIdentity ghcVersion _ envLocals _ _ _ deps =
    (empty, envLocals, scratchpadKey ghcVersion envLocals deps empty)
  where
    empty = Scripths.mergeMetas []

{- | The canonical environment identity as sorted lines: the caller's own
deps join the notebook's build-depends, and the resolved local-package
overlay and compiler are part of the key.
-}
scratchpadKey :: Text -> [FilePath] -> [Text] -> Scripths.CabalMeta -> [Text]
scratchpadKey ghcVersion localPkgs deps meta =
    sort (T.lines (canonicalKeyText localPkgs withCallerDeps ghcVersion))
  where
    withCallerDeps =
        meta{Scripths.metaDeps = Scripths.metaDeps meta <> deps}
