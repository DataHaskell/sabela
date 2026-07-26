{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.Repair.Mitigate.Loop (
    MitigationFix (..),
    runMitigations,
    mitigationDisclosure,
) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.Cascade.Commit (verifyAndRevert)
import Sabela.AI.Capabilities.Edit.CompileGate.Render (renderForDiagnostics)
import Sabela.AI.Capabilities.Edit.Repair.Mitigate (
    Discharge (..),
    MitigationRow (..),
    mitigationTable,
    rootErrors,
 )
import Sabela.AI.Health (healthOfResult, isClean)
import Sabela.AI.RepairTrace (RepairEvent (..), recordRepair)
import Sabela.AI.SelfHeal (sourceDelta)
import Sabela.AI.Store (AIStore)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Anthropic.Types (CancelToken)
import Sabela.Errors (parseErrors)
import Sabela.Handlers (ReactiveNotebook)
import Sabela.Model (Cell (..), CellError (..), CellType (..), lookupCell)
import Sabela.Session.Materialize (
    CandidateSpec (..),
    DisposableResult (..),
    DisposableVerdict (..),
    runDisposableTry,
 )
import Sabela.SessionTypes (CellLang (..))
import Sabela.State

data MitigationFix = MitigationFix
    { mfClass :: Text
    , mfRemoved :: [Text]
    , mfAdded :: [Text]
    }

factEntry :: Text -> [Text] -> Value
factEntry cls cands = object ["class" .= cls, "compilingCandidates" .= cands]

probeResult ::
    App -> Int -> CellLang -> CellType -> Text -> IO (Either Text ExecutionResult)
probeResult app cid lang ty candidate
    | ty /= CodeCell || lang /= Haskell =
        pure (Right (ExecutionResult [] Nothing [] []))
    | otherwise = do
        result <-
            runDisposableTry
                app
                CandidateSpec
                    { candidateMetadataSource = candidate
                    , candidateSetup = renderForDiagnostics candidate
                    , candidateExpression = Nothing
                    , candidateReplacesCellId = Just cid
                    , candidateDeliberate = True
                    }
        pure $ case disposableVerdict result of
            DisposableOk -> Right (ExecutionResult [] Nothing [] [])
            _ ->
                let errs = parseErrors (disposableStderr result)
                    holistic = if null errs then Just (disposableStderr result) else Nothing
                 in Right (ExecutionResult [] holistic errs [])

foldRow ::
    App ->
    AIStore ->
    Int ->
    CellLang ->
    CellType ->
    MitigationRow ->
    Either Text ExecutionResult ->
    Text ->
    IO (Maybe (Text, Text, Int), Maybe Value, Text)
foldRow app store cid lang ty row observed curSrc = do
    baseRes <- probeResult app cid lang ty curSrc
    let baseRootErrs = rootErrors curSrc baseRes <> observedRoots
        observedRoots =
            [ce | ce <- rootErrors curSrc observed, mitDetect row ce]
        withObserved (Right er) = Right er{erErrors = erErrors er <> observedRoots}
        withObserved other = other
        expected =
            Set.fromList
                [ceMessage ce | ce <- baseRootErrs, not (mitDetect row ce)]
        parseBlocking = mitClass row == "missing-extension"
    if not (any (mitDetect row) baseRootErrs)
        then pure (Nothing, Nothing, curSrc)
        else do
            cands <- mitGenerate row app store (withObserved baseRes) curSrc
            probed <- mapM (\c -> (,) c <$> probeResult app cid lang ty c) cands
            let clears (c, r) =
                    let newRoots = rootErrors c r
                     in if parseBlocking
                            then not (any (mitDetect row) newRoots)
                            else Set.fromList (map ceMessage newRoots) `Set.isSubsetOf` expected
                cleared = [c | (c, r) <- probed, clears (c, r)]
            pure $ case (mitDischarge row, cleared) of
                (_, []) -> (Nothing, Nothing, curSrc)
                (ServeAsArtifact, cs) ->
                    (Nothing, Just (factEntry (mitClass row) cs), curSrc)
                (Apply, [one]) ->
                    (Just (mitClass row, one, length cands), Nothing, one)
                (Apply, many) ->
                    (Nothing, Just (factEntry (mitClass row) many), curSrc)

compileFold ::
    App ->
    AIStore ->
    Int ->
    CellLang ->
    CellType ->
    Either Text ExecutionResult ->
    Text ->
    IO ([Value], [(Text, Text, Int)], Text)
compileFold app store cid lang ty observed src0 = go mitigationTable src0 [] []
  where
    go [] curSrc facts applied = pure (reverse facts, reverse applied, curSrc)
    go (row : rest) curSrc facts applied = do
        (mApplied, mFact, nextSrc) <-
            foldRow app store cid lang ty row observed curSrc
        let facts' = maybe facts (: facts) mFact
            applied' = maybe applied (: applied) mApplied
        go rest nextSrc facts' applied'

runMitigations ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    Int ->
    CancelToken ->
    IORef [Value] ->
    IORef Bool ->
    Int ->
    Either Text ExecutionResult ->
    IO (Maybe (Either Text ExecutionResult), Maybe Value)
runMitigations app store rn cid cancelTok sugRef staleRef k0 res0 = do
    (fixes, facts, finalRes, committedAny, remaining) <-
        go [] [] k0 res0 False Nothing
    pure
        ( if committedAny then Just finalRes else Nothing
        , mitigationDisclosure fixes remaining facts
        )
  where
    currentSrc = maybe "" cellSource . lookupCell cid <$> readNotebook (appNotebook app)

    workingSrc = maybe currentSrc pure

    cellLangType =
        maybe (Haskell, CodeCell) (\c -> (cellLang c, cellType c)) . lookupCell cid
            <$> readNotebook (appNotebook app)
    go fixes facts k res committedAny mWorking
        | isClean (healthOfResult res) =
            pure (reverse fixes, facts, res, committedAny, [])
        | k <= (0 :: Int) = do
            src <- workingSrc mWorking
            pure
                (reverse fixes, facts, res, committedAny, map ceMessage (rootErrors src res))
        | otherwise = do
            src <- workingSrc mWorking
            (lang, ty) <- cellLangType
            (newFacts, applied, composed) <- compileFold app store cid lang ty res src
            if null applied
                then
                    pure
                        ( reverse fixes
                        , facts ++ newFacts
                        , res
                        , committedAny
                        , map ceMessage (rootErrors src res)
                        )
                else do
                    mKept <- verifyAndRevert app rn cancelTok cid sugRef staleRef res src [composed]
                    let stepsList = steps src applied
                        newFixes =
                            [ MitigationFix cls removed added
                            | (cls, before, after, _) <- stepsList
                            , let (removed, added) = sourceDelta before after
                            ]
                    case mKept of
                        Nothing -> do
                            composedRes <- probeResult app cid lang ty composed
                            if composed /= src
                                then
                                    go
                                        (reverse newFixes ++ fixes)
                                        (facts ++ newFacts)
                                        (k - 1)
                                        composedRes
                                        committedAny
                                        (Just composed)
                                else do
                                    let remaining = map ceMessage (rootErrors composed composedRes)
                                    pure
                                        ( reverse fixes ++ newFixes
                                        , facts ++ newFacts
                                        , res
                                        , committedAny
                                        , remaining
                                        )
                        Just newRes -> do
                            mapM_
                                ( \(cls, _, _, n) ->
                                    recordRepair (envWorkDir (appEnv app)) (RepairEvent cid [(cls, n)] (Just cls))
                                )
                                stepsList
                            go (reverse newFixes ++ fixes) (facts ++ newFacts) (k - 1) newRes True Nothing
    steps _ [] = []
    steps before ((cls, after, n) : rest) = (cls, before, after, n) : steps after rest

mitigationDisclosure :: [MitigationFix] -> [Text] -> [Value] -> Maybe Value
mitigationDisclosure fixes remaining facts
    | null fixes && null facts = Nothing
    | otherwise =
        Just $
            object
                [ "appliedInOrder" .= map fixJSON fixes
                , "factLists" .= facts
                , "resolved" .= n
                , "total" .= (n + length remaining)
                , "remaining" .= remaining
                , "status" .= (if null remaining then "complete" else "partial" :: Text)
                , "note" .= noteFor
                ]
  where
    n = length fixes
    fixJSON f = object ["class" .= mfClass f, "removed" .= mfRemoved f, "added" .= mfAdded f]
    noteFor
        | null remaining =
            "resolved "
                <> tshow n
                <> " diagnostic"
                <> plural
                <> "; notebook compiles clean."
        | otherwise =
            "resolved "
                <> tshow n
                <> " of "
                <> tshow (n + length remaining)
                <> " diagnostics; "
                <> tshow (n + 1)
                <> " remains: "
                <> fromMaybe "" (listToMaybe remaining)
    plural = if n == 1 then "" else "s" :: Text
    tshow = T.pack . show
