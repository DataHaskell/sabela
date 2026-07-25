{-# LANGUAGE OverloadedStrings #-}

{- | Live-session introspection tools, split from the former @ghci_query@
multiplexer into single-intent tools (@list_bindings@, @check_type@,
@find_by_type@, @describe_function@), plus 'explore_result' drill-down into
handles returned by other tools. (@api_reference@ lives in the sibling
"Sabela.AI.Capabilities.ApiRef".)
-}
module Sabela.AI.Capabilities.Query (
    execCheckType,
    execFindByType,
    execDescribeFunction,
    execExploreResult,
    execPeekData,

    -- * Pieces
    runExplore,
    ExploreOp (..),
    parseExploreOp,
    guidedOutcome,
    typeConstructors,
    recordDecl,
    instanceClasses,
    withBackend,
) where

import Control.Exception (try)
import Control.Exception.Base (IOException)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Char (isUpper)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath)
import System.FilePath (normalise, splitDirectories, (</>))

import Sabela.AI.Capabilities.Query.Explore (
    ExploreOp (..),
    execExploreResult,
    parseExploreOp,
    runExplore,
 )
import Sabela.AI.Capabilities.Util (fieldInt, fieldText)
import Sabela.AI.HoleFits (holeFitsJson)
import Sabela.AI.LeakShape (leakyLine)
import Sabela.AI.PeekData (peekData, peekResultJSON)
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.AI.VerifierDistill (answerVerdict, distillInfo, distillTypeAnswer)
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
            pure
                ( guidedOutcome
                    [ "expr" .= expr
                    , "via" .= via
                    , "verdict" .= answerVerdict result
                    ]
                    result
                )

{- | Every answer routes through 'distillTypeAnswer' at this emitting seam
(R3.10): the signature stays, trailing leak-shaped output never crosses.
-}
dispatchCheckType :: SessionBackend -> Text -> IO (Text, Text)
dispatchCheckType backend expr
    | length (T.words expr) > 1 =
        (,) "type" . distillTypeAnswer <$> sbQueryType backend expr
    | otherwise = do
        ty <- distillTypeAnswer <$> sbQueryType backend expr
        if looksResolved ty
            then do
                struct <- typeStructure backend ty
                pure ("type", if T.null struct then ty else ty <> "\n\n" <> struct)
            else do
                raw <- sbQueryInfo backend expr
                pure ("info", withInstances raw (distillTypeAnswer raw))

{- | Append the type's instances to an @:info@ answer. Without them the caller
sees a type's shape but not what it can DO — the live_test20 failure, where
@Picture@'s @Semigroup@ instance was the answer and never surfaced.
-}
withInstances :: Text -> Text -> Text
withInstances raw answer = case instanceClasses raw of
    [] -> answer
    cs -> answer <> "\n\ninstances: " <> T.intercalate ", " cs

{- | True when @:type@ actually resolved the query, so we keep its answer rather
than falling back to @:info@. Matches GHC's "didn't resolve" forms
case-insensitively: a bare type/class name fails @:type@ with "not in scope" or
"Illegal term-level use of the type constructor", and we want @:info@ then.
-}
looksResolved :: Text -> Bool
looksResolved t =
    let lt = T.toLower t
     in not (T.null (T.strip t))
            && not ("not in scope" `T.isInfixOf` lt)
            && not ("error:" `T.isInfixOf` lt)
            && not ("illegal term-level" `T.isInfixOf` lt)

{- | When @check_type@ resolves a value to a record type, surface that type's
constructors and FIELD names, so the model can record-update the value in one
go instead of looping to recover the fields (the observed weak-model failure).
Best-effort: @:info@ the type constructors in the resolved type and return the
first that is an ADT with record fields; @""@ when none qualifies.
-}
typeStructure :: SessionBackend -> Text -> IO Text
typeStructure backend = go . take 4 . candidates
  where
    go [] = pure ""
    go (c : cs) = do
        decl <- recordDecl . distillInfo <$> sbQueryInfo backend c
        maybe (go cs) pure decl
    candidates = concatMap variants . typeConstructors
    variants t =
        let bare = lastSeg t
         in if bare == t then [t] else [t, bare]

{- | The classes a @:info@ dump says the type belongs to, in declaration order.
A type's instances are its composition vocabulary — @Semigroup@ is how the
caller learns that @<>@ joins two of these — and that is the one part of
@:info@ 'recordDecl' deliberately drops.
-}
instanceClasses :: Text -> [Text]
instanceClasses info =
    nubKeep
        [ cls
        | l <- map T.strip (T.lines info)
        , Just rest <- [T.stripPrefix "instance " l]
        , cls : _ <- [T.words (dropContext (fst (T.breakOn "--" rest)))]
        , maybe False (isUpper . fst) (T.uncons cls)
        ]
  where
    dropContext r = case T.breakOn "=>" r of
        (_, m) | not (T.null m) -> T.strip (T.drop 2 m)
        _ -> r
    nubKeep = foldr (\x acc -> x : filter (/= x) acc) []

-- | Last dot-separated segment of a (possibly qualified) name.
lastSeg :: Text -> Text
lastSeg = last . T.splitOn "."

{- | Type-constructor atoms (uppercase-headed, qualifier allowed) on the RHS of
a @name :: ty@ result (or a bare type), in order, deduped. The value name before
@::@ is dropped so its qualifier is never mistaken for a constructor.
-}
typeConstructors :: Text -> [Text]
typeConstructors s = nubKeep (filter isCtorAtom atoms)
  where
    rhs = case T.breakOn "::" s of
        (_, r) | not (T.null r) -> T.drop 2 r
        _ -> s
    atoms =
        filter (not . T.null) $
            T.split (`elem` (" \t\n[]()->,!{}=|" :: String)) rhs
    isCtorAtom t = maybe False (isUpper . fst) (T.uncons (lastSeg t))
    nubKeep = foldr (\x acc -> x : filter (/= x) acc) []

{- | Filter a GHCi @:info@ dump to just the data/newtype declaration — its
constructors and record fields — dropping instance lines and @-- Defined in@
provenance. @Nothing@ unless the result is an ADT declaration carrying fields.
-}
recordDecl :: Text -> Maybe Text
recordDecl info
    | hasAdt && hasField = Just kept
    | otherwise = Nothing
  where
    kept = T.intercalate "\n" (filter keep (T.lines info))
    keep l =
        let t = T.strip l
         in not (T.null t)
                && not (leakyLine t)
                && not ("instance " `T.isPrefixOf` t)
                && not ("-- Defined in" `T.isInfixOf` t)
    hasAdt = "data " `T.isInfixOf` kept || "newtype " `T.isInfixOf` kept
    hasField = "{" `T.isInfixOf` kept

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
            let fits = holeFitsJson findByTypeCap result
            pure
                ( okOutcome
                    ( object
                        [ "goal" .= goal
                        , "fits" .= fits
                        , "shown" .= length fits
                        , "probe" .= ("typecheck-only; nothing was committed" :: Text)
                        ]
                    )
                )

{- | How many fits @find_by_type@ ships. The probe is a SEARCH, so its answer
is a short ranked list of names and where they come from — never GHC's raw
hole-fit blob, whose instantiation and source-span clutter both misleads the
reader and spends its context (live_test20).
-}
findByTypeCap :: Int
findByTypeCap = 8

-- | @describe_function@: the haddock documentation for a name (@:doc@ prose).
execDescribeFunction :: App -> Value -> IO ToolOutcome
execDescribeFunction app input = do
    let name = T.strip (fieldText "name" input)
    if T.null name
        then pure (errOutcome (errorJson "name required"))
        else withBackend app $ \backend -> do
            result <- sbQueryDoc backend name
            pure (guidedOutcome ["name" .= name] result)

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
