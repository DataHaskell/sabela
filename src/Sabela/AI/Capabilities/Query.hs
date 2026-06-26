{-# LANGUAGE OverloadedStrings #-}

{- | Live-session introspection tools, split from the former @ghci_query@
multiplexer into single-intent tools (@list_bindings@, @check_type@,
@find_by_type@, @describe_function@), plus the static @api_reference@ card and
'explore_result' drill-down into handles returned by other tools.
-}
module Sabela.AI.Capabilities.Query (
    execListBindings,
    execCheckType,
    execFindByType,
    execDescribeFunction,
    execApiReference,
    execExploreResult,
    execPeekData,

    -- * Pieces
    runExplore,
    ExploreOp (..),
    parseExploreOp,
    guidedOutcome,
) where

import Control.Exception (try)
import Control.Exception.Base (IOException)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Types (Pair)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath)
import System.FilePath (normalise, splitDirectories, (</>))

import Sabela.AI.Capabilities.Util (field, fieldInt, fieldText)
import Sabela.AI.Handles (
    HandleId (..),
    LargeResult (..),
    grepLines,
    headLines,
    lookupHandle,
    sliceLines,
    tailLines,
 )
import Sabela.AI.PeekData (peekData, peekResultJSON)
import Sabela.AI.ReferenceCard (sliceApiReference)
import Sabela.AI.Store
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.Api (errorJson)
import Sabela.Diagnose (diagnose, guidancePairs)
import Sabela.SessionTypes (SessionBackend (..))
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import Sabela.State.SessionManager (getHaskellSession)

{- | Run @k@ against the live Haskell session, or return the "no session"
error when GHCi has not started. The shared preamble for every introspection
tool below.
-}
withBackend :: App -> (SessionBackend -> IO ToolOutcome) -> IO ToolOutcome
withBackend app k = do
    mBackend <- getHaskellSession (appSessions app)
    case mBackend of
        Nothing ->
            pure
                ( errOutcome
                    (errorJson "No live Haskell session — run a cell first to start GHCi.")
                )
        Just backend -> k backend

{- | Shape an introspection result into its tool outcome, attaching the same
@-- cabal:@ (and other) guidance a failed cell gets, diagnosed from the result.
A hidden-package wall thus becomes the action the notebook needs (declare the
dependency) rather than a misleading @:set -package@.
-}
guidedOutcome :: [Pair] -> Text -> ToolOutcome
guidedOutcome fields result =
    okOutcome $
        object (fields <> ["result" .= result] <> guidancePairs (diagnose result))

-- | @list_bindings@: every variable currently bound in the session, with types.
execListBindings :: App -> Value -> IO ToolOutcome
execListBindings app _ =
    withBackend app $ \backend -> do
        result <- sbQueryBindings backend
        pure (guidedOutcome [] result)

{- | @check_type@: the type of an expression, or the kind/definition of a type
or class — without running anything. The BACKEND owns the dispatch (no @op@ for
the model to get wrong): a multi-token expression goes to @:type@; a bare name
tries @:type@ first (clean @name :: ty@ for a value) and falls back to @:info@
when @:type@ cannot resolve it (a type or class). The answering command rides
back in @via@.
-}
execCheckType :: App -> Value -> IO ToolOutcome
execCheckType app input = do
    let expr = T.strip (fieldText "expr" input)
    if T.null expr
        then
            pure
                ( errOutcome
                    (errorJson "expr required (an expression, value, type, or class name)")
                )
        else withBackend app $ \backend -> do
            (via, result) <- dispatchCheckType backend expr
            pure (guidedOutcome ["expr" .= expr, "via" .= via] result)

dispatchCheckType :: SessionBackend -> Text -> IO (Text, Text)
dispatchCheckType backend expr
    | length (T.words expr) > 1 = (,) "type" <$> sbQueryType backend expr
    | otherwise = do
        ty <- sbQueryType backend expr
        if looksResolved ty
            then pure ("type", ty)
            else (,) "info" <$> sbQueryInfo backend expr

-- | True when GHCi actually resolved the query (not empty / not-in-scope).
looksResolved :: Text -> Bool
looksResolved t =
    not (T.null (T.strip t))
        && not ("Not in scope" `T.isInfixOf` t)
        && not ("error:" `T.isInfixOf` t)

{- | @find_by_type@: in-scope names whose type fits a goal type. Accepts a bare
type (@[Int] -> Int@) or a hole (@_ :: [Int] -> Int@); a bare type is wrapped
into a hole for GHC's valid-hole-fits.
-}
execFindByType :: App -> Value -> IO ToolOutcome
execFindByType app input = do
    let goal = T.strip (fieldText "goal" input)
    if T.null goal
        then
            pure
                (errOutcome (errorJson "goal required (a type like \"[Int] -> Int\")"))
        else withBackend app $ \backend -> do
            let hole = if "_" `T.isPrefixOf` goal then goal else "_ :: " <> goal
            result <- sbQueryHoleFits backend hole
            pure (guidedOutcome ["goal" .= goal] result)

-- | @describe_function@: the haddock documentation for a name (@:doc@ prose).
execDescribeFunction :: App -> Value -> IO ToolOutcome
execDescribeFunction app input = do
    let name = T.strip (fieldText "name" input)
    if T.null name
        then pure (errOutcome (errorJson "name required"))
        else withBackend app $ \backend -> do
            result <- sbQueryDoc backend name
            pure (guidedOutcome ["name" .= name] result)

execApiReference :: Value -> IO ToolOutcome
execApiReference input = do
    let mName = case field "module" input of
            Just (String s) -> s
            _ -> ""
        body = sliceApiReference mName
    pure $ okOutcome $ object ["module" .= mName, "reference" .= body]

execExploreResult :: AIStore -> Value -> IO ToolOutcome
execExploreResult store input = do
    let hidText = fieldText "handle_id" input
        op = fieldText "op" input
    if T.null hidText
        then pure (errOutcome (errorJson "handle_id required"))
        else do
            mLr <- lookupHandle (aiHandles store) (HandleId hidText)
            case mLr of
                Nothing ->
                    pure
                        (errOutcome (errorJson ("Handle not found (may have expired): " <> hidText)))
                -- 'runExplore' returns 'errorJson' for an unknown op; propagate
                -- that as a typed tool error rather than masquerading as
                -- success.
                Just lr -> case parseExploreOp op of
                    Nothing ->
                        pure (errOutcome (runExplore op input lr))
                    Just _ ->
                        pure (okOutcome (runExplore op input lr))

{- | The four 'explore_result' ops the schema declares an enum for.
Parsed at the boundary so 'runExplore' is total.
-}
data ExploreOp = ExHead | ExTail | ExSlice | ExGrep
    deriving (Eq, Show)

parseExploreOp :: Text -> Maybe ExploreOp
parseExploreOp "head" = Just ExHead
parseExploreOp "tail" = Just ExTail
parseExploreOp "slice" = Just ExSlice
parseExploreOp "grep" = Just ExGrep
parseExploreOp _ = Nothing

runExplore :: Text -> Value -> LargeResult -> Value
runExplore rawOp input lr = case parseExploreOp rawOp of
    Nothing -> errorJson ("Unknown op: " <> rawOp <> " (use head|tail|slice|grep)")
    Just ExHead ->
        let n = fromMaybe 20 (fieldInt "n" input)
         in object ["lines" .= headLines n lr, "totalLines" .= lrTotalLines lr]
    Just ExTail ->
        let n = fromMaybe 20 (fieldInt "n" input)
         in object ["lines" .= tailLines n lr, "totalLines" .= lrTotalLines lr]
    Just ExSlice ->
        let from = fromMaybe 1 (fieldInt "from" input)
            to = fromMaybe (from + 20) (fieldInt "to" input)
         in object
                [ "lines" .= sliceLines from to lr
                , "from" .= from
                , "to" .= to
                , "totalLines" .= lrTotalLines lr
                ]
    Just ExGrep ->
        let pat = fieldText "pattern" input
            hits = [object ["line" .= i, "text" .= t] | (i, t) <- grepLines pat lr]
         in object ["hits" .= hits, "totalLines" .= lrTotalLines lr]

-- | Default number of data rows 'execPeekData' returns when @n@ is omitted.
defaultPeekRows :: Int
defaultPeekRows = 10

{- | Read a delimited file (path-checked to stay within the work dir) and
report its inferred delimiter, header, per-column type guesses, and first
@n@ rows — so the model can shape a dataframe load without guessing.
-}
execPeekData :: App -> Value -> IO ToolOutcome
execPeekData app input = do
    let relPath = T.unpack (fieldText "path" input)
        n = fromMaybe defaultPeekRows (fieldInt "n" input)
        workDir = envWorkDir (appEnv app)
    if null relPath
        then pure (errOutcome (errorJson "path required"))
        else do
            canon <- canonicalizePath (workDir </> relPath)
            if not (isWithinPath workDir canon)
                then pure (errOutcome (errorJson "Path escapes the work directory."))
                else do
                    eText <- try (TIO.readFile canon) :: IO (Either IOException Text)
                    case eText of
                        Left e ->
                            pure
                                ( errOutcome
                                    (errorJson ("Could not read file: " <> T.pack (show e)))
                                )
                        Right raw ->
                            pure $
                                okOutcome $
                                    object ["path" .= relPath, "peek" .= peekResultJSON (peekData n raw)]

-- | True when @child@ is @parent@ or nested under it (case-folded, normalised).
isWithinPath :: FilePath -> FilePath -> Bool
isWithinPath parent child =
    let p = splitDirectories (normalise parent)
        c = splitDirectories (normalise child)
     in p == take (length p) c
