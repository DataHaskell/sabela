{-# LANGUAGE OverloadedStrings #-}

{- | check_type, inverted: a name is a question about the world and is answered
from the local index without a session or a lock; an expression is a question
for a compiler. Every clause of the answer carries the source that computed it.
-}
module Sabela.AI.Capabilities.Query.CheckType (
    checkTypeAnswer,
    importScopeLines,
) where

import Data.Aeson (Value (..), (.=))
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.AnswerSource (AnswerSource (..), sourceTag)
import Sabela.AI.Capabilities.Edit.ScratchVet (scratchScopeBackend)
import Sabela.AI.Capabilities.Query.IndexAnswer (
    IndexAnswer (..),
    answerModule,
    availablePackages,
    builtinAnswer,
    classifyIndexHit,
    consultedIndex,
    consultedNotebook,
    consultedSession,
    indexAnswerPairs,
    indexLookup,
    looksNotInScope,
 )
import Sabela.AI.Capabilities.Query.Route (
    Plan (..),
    classifyQuery,
    lookupText,
    planFor,
    refinePlan,
 )
import Sabela.AI.Capabilities.Query.SessionFacts (
    NotebookFacts (..),
    notebookAliasEnv,
    notebookFactPairs,
    notebookFacts,
 )
import Sabela.AI.Capabilities.Query.Struct (
    looksResolved,
    typeStructure,
    withInstances,
 )
import Sabela.AI.Capabilities.Util (field, fieldText)
import Sabela.AI.QualifiedName (QualifiedName (..), lookupSpelling)
import Sabela.AI.VerifierDistill (answerVerdict, distillTypeAnswer)
import Sabela.Api (errorJson)
import Sabela.Model (Notebook)
import Sabela.SessionTypes (SessionBackend (..))
import Sabela.State (App (..), getAIStore)
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)

{- | The payload fields and prose of one check_type call, or the reason no
answer could be attempted.
-}
checkTypeAnswer :: App -> Value -> IO (Either Value ([Pair], Text))
checkTypeAnswer app input
    | T.null expr =
        pure
            ( Left
                (errorJson "expr required (an expression, value, type, or class name)")
            )
    | otherwise = case planFor imports (classifyQuery expr) of
        SessionPlan -> answerFromSession app expr imports
        IndexPlan n -> do
            nb <- readNotebook (appNotebook app)
            let qn = lookupSpelling (notebookAliasEnv nb) (lookupText n)
                facts = notebookFacts nb (qnBare qn) (qnModule qn)
            case refinePlan (not (null (nfDefines facts))) (IndexPlan n) of
                SessionPlan -> answerFromSession app expr imports
                IndexPlan _ -> answerFromIndex app expr qn nb facts
  where
    expr = T.strip (fieldText "expr" input)
    imports = importScopeLines (fieldTextList "imports" input)

type Answer = Either Value ([Pair], Text)

{- | The notebook facts are recomputed against the module the index answered
with, so the session clause never describes a module nobody looked for.
-}
answerFromIndex ::
    App -> Text -> QualifiedName -> Notebook -> NotebookFacts -> IO Answer
answerFromIndex app expr qn nb facts
    | Just b <- builtinAnswer (qnBare qn) =
        pure (Right (tagged BuiltinVocab (composed b)))
    | otherwise = do
        mHit <- indexLookup (qnModule qn) (qnBare qn)
        available <- availablePackages app
        case classifyIndexHit available indexTrace mHit of
            WorldMiss _ -> sessionRescue app expr qn facts
            found -> pure (Right (tagged LocalIndex (composed found)))
  where
    composed answer = indexAnswerPairs qn answer (factsFor answer)
    factsFor answer =
        notebookFacts nb (qnBare qn) (answerModule answer (qnModule qn))

indexTrace :: [Text]
indexTrace = [consultedIndex, consultedNotebook]

{- | A name the index does not hold may still be one a live session knows. The
session is asked only then, and the trace records that it was.
-}
sessionRescue :: App -> Text -> QualifiedName -> NotebookFacts -> IO Answer
sessionRescue app expr qn facts = do
    mBackend <- getHaskellSession (appSessions app)
    case mBackend of
        Nothing -> pure (Right (missed indexTrace))
        Just backend -> do
            answered <- dispatchCheckType backend expr
            pure . Right $
                if looksNotInScope (snd answered)
                    then missed withSession
                    else
                        scoped
                            "live_session"
                            withSession
                            (notebookFactPairs facts)
                            answered
  where
    withSession = indexTrace <> [consultedSession]
    missed trace =
        tagged LocalIndex (indexAnswerPairs qn (WorldMiss trace) facts)

answerFromSession :: App -> Text -> [Text] -> IO Answer
answerFromSession app expr imports =
    withCheckTypeScope app imports $ \scope backend ->
        scoped scope [consultedSession] ["imports" .= imports]
            <$> dispatchCheckType backend expr

-- | The source tag and the verdict travel with every answer.
tagged :: AnswerSource -> ([Pair], Text) -> ([Pair], Text)
tagged src (ps, result) =
    ( ["via" .= sourceTag src] <> ps <> ["verdict" .= answerVerdict result]
    , result
    )

scoped :: Text -> [Text] -> [Pair] -> (AnswerSource, Text) -> ([Pair], Text)
scoped scope consulted extra (src, result) =
    tagged src (["scope" .= scope, "consulted" .= consulted] <> extra, result)

{- | Which session answers the question. With no imports asked for it is the
live one, as before; with imports it is a scratch scope, because the live
session's scope is the notebook's and must not be changed to answer a question.
-}
withCheckTypeScope ::
    App -> [Text] -> (Text -> SessionBackend -> IO ([Pair], Text)) -> IO Answer
withCheckTypeScope app [] k = do
    mBackend <- getHaskellSession (appSessions app)
    case mBackend of
        Nothing ->
            pure
                ( Left
                    ( errorJson
                        "No live Haskell session — run a cell first to start GHCi."
                    )
                )
        Just backend -> Right <$> k "live_session" backend
withCheckTypeScope app imports k = do
    mStore <- getAIStore app
    case mStore of
        Nothing ->
            pure
                ( Left
                    ( errorJson
                        "imports need a scratch scope, which this server has not \
                        \started; retry without imports to ask the live session."
                    )
                )
        Just store -> do
            backend <- scratchScopeBackend app store [] (T.unlines imports)
            Right <$> k "scratch" backend

fieldTextList :: Text -> Value -> [Text]
fieldTextList key v = case field key v of
    Just (Array items) -> [t | String t <- foldr (:) [] items]
    Just (String t) -> [t]
    _ -> []

{- | Accept a module name with or without its @import@ keyword: the caller is
naming a module to have in scope, not writing a source line.
-}
importScopeLines :: [Text] -> [Text]
importScopeLines raw =
    [ if "import " `T.isPrefixOf` t then t else "import " <> t
    | l <- raw
    , let t = T.strip l
    , not (T.null t)
    ]

dispatchCheckType :: SessionBackend -> Text -> IO (AnswerSource, Text)
dispatchCheckType backend expr
    | length (T.words expr) > 1 =
        (,) SessionType . distillTypeAnswer <$> sbQueryType backend expr
    | otherwise = do
        ty <- distillTypeAnswer <$> sbQueryType backend expr
        if looksResolved ty
            then do
                struct <- typeStructure backend ty
                pure (SessionType, if T.null struct then ty else ty <> "\n\n" <> struct)
            else do
                raw <- sbQueryInfo backend expr
                pure (SessionInfo, withInstances raw (distillTypeAnswer raw))
