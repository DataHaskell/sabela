{- | Pull inferred types from the live session for top-level binds that
lack a signature (redesign 6.6 phase 4).

'unsignedTopLevelBinds' finds, from the parsed cell, the top-level value
binders with no 'Hs.TypeSig'. 'annotateCell' then asks a 'TypeQuery' — a
@:type@ pull against the running GHCi via @check_type@
— for each, and assembles an 'AnnotateReport'.

The 'TypeQuery' is injected, so the assembly is testable without a server,
and the report degrades gracefully: a 'Left' from the query (cold session,
ambiguous type) becomes an 'AnnFailed' line, never a crash.
-}
module Siza.Annotate (
    -- * Finding unsigned binds
    unsignedTopLevelBinds,

    -- * Querying + assembly
    TypeQuery,
    Annotation (..),
    AnnotateReport (..),
    annotateModule,
    annotateCell,

    -- * Rendering
    renderReport,
    annotatedSource,
) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import qualified GHC.Hs as Hs
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SrcLoc (unLoc)

import Siza.Lang.Haskell (parseModuleE)
import Siza.Language (Diagnostic, renderDiagnostic)

-- ---------------------------------------------------------------------------
-- Finding unsigned top-level binds
-- ---------------------------------------------------------------------------

{- | The top-level value binders of a cell that carry no type signature, in
source order with duplicates removed. A bind whose name already appears in a
'Hs.TypeSig' is skipped — it is already annotated. Class/data/instance decls
and pattern bindings (which name no single function) are not candidates.
-}
unsignedTopLevelBinds :: Hs.HsModule Hs.GhcPs -> [Text]
unsignedTopLevelBinds m =
    dedup [n | n <- valBinds, not (n `S.member` signed)]
  where
    decls = map unLoc (Hs.hsmodDecls m)
    valBinds = concatMap funBindName decls
    signed = S.unions (map sigNames decls)

-- | The single function-bind name a top-level 'Hs.ValD' introduces, if any.
funBindName :: Hs.HsDecl Hs.GhcPs -> [Text]
funBindName = \case
    Hs.ValD _ Hs.FunBind{Hs.fun_id = lname} -> [rdrText (unLoc lname)]
    _ -> []

-- | Names bound by a top-level 'Hs.TypeSig' (so already annotated).
sigNames :: Hs.HsDecl Hs.GhcPs -> Set Text
sigNames = \case
    Hs.SigD _ (Hs.TypeSig _ lns _) ->
        S.fromList [rdrText (unLoc ln) | ln <- lns]
    _ -> S.empty

-- | The bare textual name of a parsed reader name, as 'Sabela.Parse' renders it.
rdrText :: RdrName -> Text
rdrText = T.pack . occNameString . rdrNameOcc

-- | Stable nub: keep first occurrence, drop later duplicates.
dedup :: [Text] -> [Text]
dedup = go S.empty
  where
    go _ [] = []
    go seen (x : xs)
        | x `S.member` seen = go seen xs
        | otherwise = x : go (S.insert x seen) xs

-- ---------------------------------------------------------------------------
-- Querying + assembly
-- ---------------------------------------------------------------------------

{- | Ask the live session for a name's inferred type. @Right ty@ is the
rendered signature body (the right of @::@); @Left reason@ is a graceful
failure — a cold session, or a type the kernel could not infer.
-}
type TypeQuery = Text -> IO (Either Text Text)

-- | One bind's annotation outcome: an inferred type, or why it could not be.
data Annotation
    = AnnInferred Text Text
    | AnnFailed Text Text
    deriving (Eq, Show)

{- | The result of annotating a cell: a parse error (the source did not
parse), or one 'Annotation' per unsigned top-level bind.
-}
data AnnotateReport
    = AnnParseError [Diagnostic]
    | AnnReport [Annotation]
    deriving (Eq, Show)

{- | Annotate a parsed module: run the 'TypeQuery' for each unsigned
top-level bind and collect the outcomes, in source order.
-}
annotateModule :: TypeQuery -> Hs.HsModule Hs.GhcPs -> IO [Annotation]
annotateModule query m = mapM annotate1 (unsignedTopLevelBinds m)
  where
    annotate1 name = do
        r <- query name
        pure $ case r of
            Right ty -> AnnInferred name (T.strip ty)
            Left reason -> AnnFailed name reason

{- | Annotate a cell's source end to end: parse it, then 'annotateModule'.
A syntax error short-circuits to 'AnnParseError' with no query round-trips.
-}
annotateCell :: TypeQuery -> Text -> IO AnnotateReport
annotateCell query src =
    case parseModuleE src of
        Left ds -> pure (AnnParseError ds)
        Right m -> AnnReport <$> annotateModule query m

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

{- | A human-readable report: one @name :: type@ line per inferred bind,
and a @-- name: <reason>@ line per failure. A parse error renders its
diagnostics. Empty when the cell has no unsigned top-level binds.
-}
renderReport :: AnnotateReport -> Text
renderReport = \case
    AnnParseError ds -> T.unlines (map renderDiagnostic ds)
    AnnReport [] -> "-- no unsigned top-level binds"
    AnnReport anns -> T.unlines (map renderAnnotation anns)

renderAnnotation :: Annotation -> Text
renderAnnotation = \case
    AnnInferred name ty -> name <> " :: " <> ty
    AnnFailed name reason -> "-- " <> name <> ": " <> reason

{- | The cell source with an inferred signature inserted above each unsigned
bind. Failed binds are left untouched (no signature is fabricated). The
inferred lines are prepended as a block, which keeps the transform simple
and order-stable; the originals follow verbatim.
-}
annotatedSource :: AnnotateReport -> Text -> Text
annotatedSource report src = case report of
    AnnParseError _ -> src
    AnnReport anns ->
        let sigs = [name <> " :: " <> ty | AnnInferred name ty <- anns]
         in if null sigs
                then src
                else T.unlines sigs <> src
