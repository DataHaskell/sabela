{-# LANGUAGE OverloadedStrings #-}

{- | The type-directed repair candidate sources for the repair search: the
speculative hole-fit tier and the typed-hole engine (annotation-drop + in-context
hole-fill). The compiler is the oracle — GHC's typed holes and hole-fits drive the
candidates — so these are general (any library) and library-agnostic; each candidate
is vetted by the caller's verify-and-revert. Split from "Sabela.AI.Capabilities.Edit.Repair"
to keep both under the size cap.
-}
module Sabela.AI.Capabilities.Edit.HoleSearch (
    holeFitCandidates,
    holeSearchCandidates,
    selectByTypeCheck,
    goalSpans,
) where

import Data.List (nub)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.Repair (resultErrorText)
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.Health (healthOfTypeQuery, isClean)
import Sabela.AI.HoleRepair (
    dropAnnotation,
    droppableAnnotation,
    goalFromError,
    holeFitNames,
    holeTypeFromDiagnostic,
    orderBySimilarity,
    substituteNameAt,
    suggestedNames,
 )
import Sabela.AI.Repair (firstJustM)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Errors.Json (parseJsonInteractive)
import Sabela.Model (CellError (..))
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), getHaskellSession)

{- | Speculative hole-fit candidates for a red not-in-scope-with-type cell: query
GHC hole fits for the inferred goal type and substitute the wrong name. Checked
compile-only before any is committed; empty unless the error carries a goal type.
-}
holeFitCandidates :: App -> Either Text ExecutionResult -> Text -> IO [Text]
holeFitCandidates app res src = do
    on <- featureEnabled "SABELA_HOLE_FIT"
    mBackend <- getHaskellSession (appSessions app)
    case (on, mBackend, goalSpans res) of
        (True, Just backend, goals@(_ : _)) ->
            nub . concat <$> mapM (candidatesFor backend) goals
        _ -> pure []
  where
    errText = resultErrorText res
    candidatesFor backend (wrong, ty, mspan) = do
        blob <- ST.sbQueryHoleFits backend ("_ :: " <> ty)
        let fitText = decodeDiagnostics blob
            -- Did-you-mean first, then hole fits, ranked by spelling closeness
            -- so a typo heals to the nearest valid name.
            names =
                orderBySimilarity
                    wrong
                    (nub (suggestedNames errText ++ holeFitNames fitText))
        pure [s | n <- names, s <- rewrites mspan wrong n, s /= src]
    -- Span-localized rewrite only: a global lexical replace would corrupt the
    -- name inside strings/comments (which the compile check cannot catch), so a
    -- diagnostic with no usable span yields no candidate rather than a risky one.
    rewrites mspan wrong n =
        maybe [] (\sp -> maybeToList (substituteNameAt sp wrong n src)) mspan

{- | Two compiler-driven moves, selected compile-only by the caller: drop the
@:: T@ GHC flagged, or hole the name and fill from hole-fits of its in-context
@:type@ (stronger than the error-parsed type). @SABELA_HOLE_SEARCH=0@ disables.
-}
holeSearchCandidates :: App -> Either Text ExecutionResult -> Text -> IO [Text]
holeSearchCandidates app res src = do
    enabled <- featureEnabled "SABELA_HOLE_SEARCH"
    if not enabled
        then pure []
        else do
            mBackend <- getHaskellSession (appSessions app)
            let annDrops =
                    [ s
                    | Just ty <- [droppableAnnotation errText]
                    , let s = dropAnnotation ty src
                    , s /= src
                    ]
            fills <- case mBackend of
                Just backend -> nub . concat <$> mapM (inContextFills backend) (goalSpans res)
                Nothing -> pure []
            pure (annDrops ++ fills)
  where
    errText = resultErrorText res
    inContextFills _ (_, _, Nothing) = pure []
    inContextFills backend (wrong, tyErr, Just (l, c)) = do
        let holed = fromMaybe src (substituteNameAt (l, c) wrong "_" src)
        typeResp <- ST.sbQueryType backend (rhsAt l holed)
        let ty = fromMaybe tyErr (holeTypeFromDiagnostic (decodeDiagnostics typeResp))
        blob <- ST.sbQueryHoleFits backend ("_ :: " <> ty)
        let names = orderBySimilarity wrong (nub (holeFitNames (decodeDiagnostics blob)))
        pure [s | n <- names, Just s <- [substituteNameAt (l, c) wrong n src], s /= src]
    -- The expression on line l to :type: the RHS of an @x = e@ binding, else the line.
    rhsAt l holed = case drop (l - 1) (T.lines holed) of
        (ln : _) ->
            let (_, rhs) = T.breakOn " = " ln
             in if T.null rhs then T.strip ln else T.strip (T.drop 3 rhs)
        _ -> ""

{- | The @(wrong, goalType, span)@ triples a failed run implies, one per
not-in-scope diagnostic. A holistic error carries no span, so only the global
rewrite applies to it.
-}
goalSpans :: Either Text ExecutionResult -> [(Text, Text, Maybe (Int, Int))]
goalSpans (Left e) = [(w, t, Nothing) | Just (w, t) <- [goalFromError e]]
goalSpans (Right er) =
    [ (w, t, (,) <$> ceLine ce <*> ceCol ce)
    | ce <- diags
    , Just (w, t) <- [goalFromError (ceMessage ce)]
    ]
  where
    diags =
        maybe [] (\m -> [CellError Nothing Nothing m]) (erError er)
            ++ erErrors er

{- | The first candidate whose expression @:type@-checks clean, WITHOUT running
it. 'Nothing' when none check clean (or there is no session).
-}
selectByTypeCheck :: App -> [Text] -> IO (Maybe Text)
selectByTypeCheck _ [] = pure Nothing
selectByTypeCheck app cands = do
    mBackend <- getHaskellSession (appSessions app)
    case mBackend of
        Nothing -> pure Nothing
        Just backend -> fmap fst <$> firstJustM (checkClean backend) cands
  where
    checkClean backend c = do
        out <- ST.sbQueryType backend (typeCheckTarget c)
        pure (if isClean (healthOfTypeQuery out) then Just () else Nothing)

{- | The expression to @:type@ for a candidate source: the RHS of a simple
@x = expr@ binding, else the whole stripped source. A non-checkable candidate
fails the check and is skipped, so at worst a repair is missed, never kept.
-}
typeCheckTarget :: Text -> Text
typeCheckTarget src
    | T.count "\n" stripped == 0
    , (_, rhs) <- T.breakOn " = " stripped
    , not (T.null rhs) =
        T.strip (T.drop 3 rhs)
    | otherwise = stripped
  where
    stripped = T.strip src

{- | Decode a live @:type@ / hole-fit reply to the plain text the parsers expect:
the session emits @-fdiagnostics-as-json@, so fits arrive JSON-escaped.
-}
decodeDiagnostics :: Text -> Text
decodeDiagnostics raw =
    let (errs, _, rest) = parseJsonInteractive raw
     in T.unlines (map ceMessage errs) <> rest
