{-# LANGUAGE OverloadedStrings #-}

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
        pure (refined, plain)
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
    rewrites spans wrong n = maybeToList (substituteNameAtAll spans wrong n src)
    renameOk wrong n = wrong == "_" || plausibleRename wrong n

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

vacuousFit :: Text -> Bool
vacuousFit n =
    T.strip n
        `elem` ["mempty", "undefined", "[]", "Nothing", "mzero", "empty", "()"]

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
    rhsAt l holed = case drop (l - 1) (T.lines holed) of
        (ln : _) ->
            let (_, rhs) = T.breakOn " = " ln
             in if T.null rhs then T.strip ln else T.strip (T.drop 3 rhs)
        _ -> ""

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

decodeDiagnostics :: Text -> Text
decodeDiagnostics raw =
    let (errs, _, rest) = parseJsonInteractive raw
     in T.unlines (map ceMessage errs) <> rest
