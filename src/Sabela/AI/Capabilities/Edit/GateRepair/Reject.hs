{- | The rejection a repair frontier produces: the errors the model still has
to answer after every proven fix, with the facts the IO around the gate
fetched (exposing package, undeclared imports, verified façade claims).
-}
module Sabela.AI.Capabilities.Edit.GateRepair.Reject (
    frontierRejection,
    frontierRejectionJson,
    frontierSource,
) where

import Data.Aeson (Value)
import Data.Aeson.Types (Pair)
import Data.Text (Text)

import Sabela.AI.Capabilities.Edit.CompileGate (
    GateSource (..),
    exposingPackage,
    gateHoleNudge,
    prevDefinedNames,
    rejectionJson,
 )
import Sabela.AI.Capabilities.Edit.GateFrontier (
    Frontier (..),
    disposableDiagnostic,
    frontierPairs,
 )
import Sabela.AI.Capabilities.Edit.HoleNudge (attachPairs)
import Sabela.AI.Capabilities.Edit.OrphanGate (undeclaredImportPairs)
import Sabela.AI.TypeOriginProbe (
    annotateDisposableWith,
    exportedByPairs,
    facadeClaims,
 )
import Sabela.Session.MaterializeStage (DisposableResult (..))
import Sabela.State (App)

frontierSource :: Frontier -> GateSource
frontierSource frontier =
    GateSource
        { gateSubmitted = frontierSubmitted frontier
        , gateCompiled = frontierSrc frontier
        }

{- | Rejects on the frontier rather than on the submission: the errors the
model still has to answer are the ones left after every proven fix, not the
first one that stopped the compiler.
-}
frontierRejection :: App -> Maybe Int -> Frontier -> IO Value
frontierRejection app mReplaces frontier = do
    prevDefined <- prevDefinedNames app mReplaces
    nudge <- gateHoleNudge app mReplaces verdict diagnostic (frontierSrc frontier)
    exposedBy <- exposingPackage diagnostic
    diverge <- undeclaredImportPairs app (frontierResult frontier)
    claims <-
        facadeClaims
            app
            (disposableDependencies (frontierResult frontier))
            diagnostic
    pure
        ( frontierRejectionJson
            exposedBy
            mReplaces
            prevDefined
            (nudge <> diverge <> exportedByPairs claims)
            frontier
                { frontierResult =
                    annotateDisposableWith claims (frontierResult frontier)
                }
        )
  where
    verdict = disposableVerdict (frontierResult frontier)
    diagnostic = disposableDiagnostic (frontierResult frontier)

{- | The rejection a frontier produces, given the facts the IO around it went
and fetched. Pure, so the payload the model reads can be pinned without a
session standing behind it.
-}
frontierRejectionJson ::
    Maybe Text -> Maybe Int -> [Text] -> [Pair] -> Frontier -> Value
frontierRejectionJson exposedBy mReplaces prevDefined nudge frontier =
    attachPairs (nudge <> frontierPairs frontier) $
        rejectionJson
            exposedBy
            mReplaces
            (frontierSource frontier)
            prevDefined
            (frontierResult frontier)
