{-# LANGUAGE OverloadedStrings #-}

{- | The type-directed repair candidate sources for the repair search: the
speculative hole-fit tier and the typed-hole engine (annotation-drop + in-context
hole-fill). The compiler is the oracle — GHC's typed holes and hole-fits drive the
candidates — so these are general (any library) and library-agnostic; each candidate
is vetted by the caller's verify-and-revert. Split from "Sabela.AI.Capabilities.Edit.Repair"
to keep both under the size cap.
-}
module Sabela.AI.Capabilities.Edit.HoleSearch (
    argInsertCandidates,
    holeFitCandidates,
    holeSearchCandidates,
    goalSpans,
    vacuousFit,
) where

import Control.Exception (SomeException, try)
import Data.List (nub)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Set as S
import Sabela.AI.ArgRepair (
    argFillCandidates,
    insertArgAt,
    missingArgType,
    tooFewArgsTarget,
 )
import Sabela.AI.Capabilities.Edit.Repair (resultErrorText)
import Sabela.AI.Capabilities.Edit.ScratchVet (scratchScopeBackend)
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.HoleFits (refinementFits)
import Sabela.AI.HoleRepair (
    dropAnnotation,
    droppableAnnotation,
    goalSpans,
    holeFitNames,
    holeQueryFor,
    holeSpans,
    holeTypeFromDiagnostic,
    orderBySimilarity,
    substituteNameAt,
    substituteNameAtAll,
    suggestedNames,
 )
import Sabela.AI.Repair (interleave)
import Sabela.AI.SelfHeal (plausibleRename)
import Sabela.AI.Store (AIStore)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Errors.Json (parseJsonInteractive)
import Sabela.Model (CellError (..))
import Sabela.Parse (cellNames)
import Sabela.Session.Query (TypecheckResult (..), typecheckValueWith)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), getHaskellSession)
import System.Environment (lookupEnv)

{- | Speculative hole-fit candidates for a red not-in-scope-with-type cell: query
GHC hole fits for the inferred goal type and substitute the wrong name; returns
(type-endorsed refinement rewrites, lexical did-you-mean rewrites). Empty
unless the error carries a goal type.

The query runs in the SCRATCH session with the cell's scope replayed: GHC may
spell the goal with the cell's own type synonym, which the live prompt cannot
parse. Falls back to the live backend if the scratch fails.
-}
holeFitCandidates ::
    App ->
    AIStore ->
    Either Text ExecutionResult ->
    Text ->
    IO ([Text], [Text])
holeFitCandidates app store res src = do
    on <- featureEnabled "SABELA_HOLE_FIT"
    mBackend <- queryBackend
    case (on, mBackend, nameGoals (goalSpans res ++ holeSpans res)) of
        (True, Just backend, goals@(_ : _)) -> do
            pairs <- mapM (candidatesFor backend) goals
            pure
                ( nub (interleave (map fst pairs))
                , nub (interleave (map snd pairs))
                )
        _ -> pure ([], [])
  where
    queryBackend = do
        primitive <- featureEnabled "SABELA_TYPECHECK_PRIMITIVE"
        if primitive
            then getHaskellSession (appSessions app)
            else do
                scratch <-
                    try (scratchScopeBackend app store [] src) ::
                        IO (Either SomeException ST.SessionBackend)
                either (const (getHaskellSession (appSessions app))) (pure . Just) scratch
    errText = resultErrorText res
    -- One goal per WRONG NAME with every reported site: a multi-site name is
    -- one entry in the message-set health, so only an all-sites candidate can
    -- show improvement. Names the CELL defines are knock-on casualties, never
    -- repair targets.
    nameGoals goals =
        [ (w, ty, [sp | (w', _, Just sp) <- goals, w' == w])
        | (w, ty) <- nub [(w, ty) | (w, ty, _) <- goals]
        , not (w `S.member` cellDefs)
        ]
    cellDefs = fst (cellNames src)
    candidatesFor backend (wrong, ty, spans) = do
        blob <- queryHole backend (holeQueryFor ty)
        debugDumpFits (holeQueryFor ty) blob
        let fitText = decodeDiagnostics blob
            -- Did-you-mean first, then hole fits, ranked by spelling closeness
            -- so a typo heals to the nearest valid name. Vacuous fits dropped
            -- ('vacuousFit'); lexically distant fits declined ('renameOk') —
            -- except for a literal hole, whose fits GHC already type-endorsed.
            names =
                filter (renameOk wrong) $
                    orderBySimilarity
                        wrong
                        ( nub
                            ( suggestedNames errText
                                ++ filter (not . vacuousFit) (holeFitNames fitText)
                            )
                        )
        let plain = [s | n <- names, s <- rewrites spans wrong n, s /= src]
        refined <-
            concat
                <$> mapM
                    (refinementRewrites backend spans wrong)
                    (filter (renameOk wrong . fst) (refinementFits fitText))
        -- Refined candidates are TYPE-ENDORSED (GHC proposed them for the
        -- full goal); plain did-you-mean names are lexical only. Split so the
        -- caller does not re-check endorsed ones against a scope that cannot
        -- see the cell's own bindings.
        pure (refined, plain)
    {- A refinement fit @fn (_ :: ArgTy)@ proposes BOTH the right name and its
    missing argument's type (takeWhileP + Maybe String); fill the sub-hole by
    its own hole fits and splice @(fn fill)@ at every site of the wrong name. -}
    refinementRewrites backend spans wrong (fn, argTy) = do
        fillBlob <- queryHole backend (holeQueryFor argTy)
        let fills = take 3 (argFillCandidates (decodeDiagnostics fillBlob))
        pure
            [ s
            | fill <- fills
            , Just s <-
                [ substituteNameAtAll
                    spans
                    wrong
                    ("(" <> fn <> " " <> fill <> ")")
                    src
                ]
            , s /= src
            ]
    -- Span-localized rewrites only: a global lexical replace would corrupt the
    -- name inside strings/comments (which the compile check cannot catch), so a
    -- diagnostic with no usable span yields no candidate rather than a risky one.
    rewrites spans wrong n = maybeToList (substituteNameAtAll spans wrong n src)
    -- Lexical closeness is meaningless for the hole token: its fits are
    -- type-endorsed by GHC, so only real renames pass 'plausibleRename'.
    renameOk wrong n = wrong == "_" || plausibleRename wrong n

{- | Raw hole-fit responses appended to the file @SABELA_DEBUG_HOLE_FITS@ names
— a debugging tap for when the tier looks inert; off unless the var is set.
-}
debugDumpFits :: Text -> Text -> IO ()
debugDumpFits query blob = do
    mp <- lookupEnv "SABELA_DEBUG_HOLE_FITS"
    case mp of
        Just p
            | not (null p)
            , p /= "0" ->
                appendFile
                    p
                    (T.unpack ("== " <> query <> " ==\n" <> blob <> "\n"))
        _ -> pure ()

{- | Argument-insertion candidates for GHC's \"applied to too few arguments\"
diagnostic: the model (or a rename) wrote @fn args@ where @fn@ wants one more
leading argument; GHC names @fn@ and the missing argument's type, a hole fit of
that type feeds the fill (@takeWhileP Nothing@). Same site discipline as every
substitution; vetted by the caller. @SABELA_ARG_INSERT=0@ disables.
-}
argInsertCandidates :: App -> Either Text ExecutionResult -> Text -> IO [Text]
argInsertCandidates app res src = do
    enabled <- featureEnabled "SABELA_ARG_INSERT"
    mBackend <- getHaskellSession (appSessions app)
    case (enabled, mBackend, tooFewArgsTarget errText) of
        (True, Just backend, Just fn) ->
            case missingArgType errText fn of
                Nothing -> pure []
                Just argTy -> do
                    blob <- queryHole backend (holeQueryFor argTy)
                    let fills = take 3 (argFillCandidates (decodeDiagnostics blob))
                    pure
                        [ s
                        | sp <- tooFewArgsSites fn res
                        , fill <- fills
                        , Just s <- [insertArgAt sp fn fill src]
                        , s /= src
                        ]
        _ -> pure []
  where
    errText = resultErrorText res

-- | The reported spans of the too-few-arguments diagnostics naming @fn@.
tooFewArgsSites :: Text -> Either Text ExecutionResult -> [(Int, Int)]
tooFewArgsSites _ (Left _) = []
tooFewArgsSites fn (Right er) =
    [ (l, c)
    | ce <- erErrors er
    , ("`" <> fn <> "' is applied to too few arguments")
        `T.isInfixOf` ceMessage ce
    , Just l <- [ceLine ce]
    , Just c <- [ceCol ce]
    ]

{- | Generic hole-fits GHC offers for any Monoid/Foldable/Maybe goal — @mempty@,
@[]@, @undefined@, @Nothing@… — that compile but discard meaning. Never a valid
heal for a mis-typed reference (substituting one for a not-in-scope name silently
empties the cell), so drop them: a free name with no real match stays a surfaced
error instead.
-}
vacuousFit :: Text -> Bool
vacuousFit n =
    T.strip n
        `elem` ["mempty", "undefined", "[]", "Nothing", "mzero", "empty", "()"]

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
        typeResp <- queryHole backend (rhsAt l holed)
        let ty = fromMaybe tyErr (holeTypeFromDiagnostic (decodeDiagnostics typeResp))
        blob <- queryHole backend (holeQueryFor ty)
        let names =
                orderBySimilarity
                    wrong
                    (filter (not . vacuousFit) (nub (holeFitNames (decodeDiagnostics blob))))
        pure [s | n <- names, Just s <- [substituteNameAt (l, c) wrong n src], s /= src]
    -- The expression on line l to :type: the RHS of an @x = e@ binding, else the line.
    rhsAt l holed = case drop (l - 1) (T.lines holed) of
        (ln : _) ->
            let (_, rhs) = T.breakOn " = " ln
             in if T.null rhs then T.strip ln else T.strip (T.drop 3 rhs)
        _ -> ""

{- | Default-on Path-2 hole inspection. The escape hatch retains the prior
backend command, while the normal route is exactly a live @:type@ query.
-}
queryHole :: ST.SessionBackend -> Text -> IO Text
queryHole backend expression = do
    primitive <- featureEnabled "SABELA_TYPECHECK_PRIMITIVE"
    if primitive
        then
            tcDiagnostics
                <$> typecheckValueWith
                    (ST.sbQueryType backend)
                    (ST.sbQueryBindings backend)
                    expression
        else ST.sbQueryHoleFits backend expression

{- | Decode a live @:type@ / hole-fit reply to the plain text the parsers expect:
the session emits @-fdiagnostics-as-json@, so fits arrive JSON-escaped.
-}
decodeDiagnostics :: Text -> Text
decodeDiagnostics raw =
    let (errs, _, rest) = parseJsonInteractive raw
     in T.unlines (map ceMessage errs) <> rest
