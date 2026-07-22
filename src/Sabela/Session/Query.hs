{-# LANGUAGE OverloadedStrings #-}

{- | Cheap GHCi introspection (:type, :info, …) over the live session.
Queries are bounded; an abandoned query's stale output self-heals via
the numbered-marker discard in the next drain.
-}
module Sabela.Session.Query (
    queryComplete,
    queryType,
    queryInfo,
    queryKind,
    queryBrowse,
    queryDoc,
    queryHoleFits,
    queryBindings,
    TypecheckInput (..),
    TypecheckResult (..),
    classifyTypecheckInput,
    typecheckValueWith,
    typecheckLetDeclarations,
    captureBindingsBaseline,
    scrubBindings,
    groupEntries,
) where

import Control.Concurrent (withMVar)
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import Sabela.Session (
    Session (..),
    getMarker,
    markerText,
    placeMarker,
    readErrorBuffer,
    resetErrorBuffer,
    sendRaw,
    sessLines,
 )
import Sabela.Session.Drain (drainResultText, drainUntilMarker)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

queryTimeoutUs :: Int
queryTimeoutUs = 10 * 1000000

{- | GHCi @:complete repl@ defaults to ~250 matches returned ALPHABETICALLY, so
for the bare @import @ prefix the discovery-relevant namespaces (@DataFrame.*@,
@Granite.*@) sort past the cap and never appear — leaving @find_function@ unable
to discover a re-exported verb like @fit@. With @SABELA_INSTANCE_SURFACING@ set we
ask for the full list (high count); unset keeps the default cap (the baseline the
A/B's OFF arm measures against). No module names are hardcoded.
-}
queryComplete :: Session -> Text -> IO [Text]
queryComplete sess prefix = do
    surfacing <- isJust <$> lookupEnv "SABELA_INSTANCE_SURFACING"
    let countArg = if surfacing then "1000000 " else ""
    res <- runQueryCommand sess (QueryComplete (countArg <> "\"" <> prefix <> "\""))
    pure (concatMap parseCompletionLine (T.lines res))

parseCompletionLine :: Text -> [Text]
parseCompletionLine line =
    let stripped = T.strip line
     in case T.stripPrefix "\"" stripped of
            Just rest -> case T.stripSuffix "\"" rest of
                Just inner -> [inner]
                Nothing -> []
            Nothing -> []

queryType :: Session -> Text -> IO Text
queryType sess name = runQueryCommand sess (QueryType name)

queryInfo :: Session -> Text -> IO Text
queryInfo sess name = runQueryCommand sess (QueryInfo name)

queryKind :: Session -> Text -> IO Text
queryKind sess name = runQueryCommand sess (QueryKind name)

queryBrowse :: Session -> Text -> IO Text
queryBrowse sess mname = runQueryCommand sess (QueryBrowse mname)

queryDoc :: Session -> Text -> IO Text
queryDoc sess name = runQueryCommand sess (QueryDoc name)

{- | Ask GHC which in-scope names fit a concrete goal type. The goal must be a
typed hole expression, e.g. @_ :: [Int] -> Int@; GHC reports the fits in its
error blob, which 'Sabela.AI.Capabilities.Query.parseHoleFits' parses.
-}
queryHoleFits :: Session -> Text -> IO Text
queryHoleFits sess goal = runQueryCommand sess (QueryHoleFits goal)

{- | List the session's interactive bindings, scrubbed of the prelude-injected
ones, so only what the notebook defined remains (@:show bindings@).
-}
queryBindings :: Session -> IO Text
queryBindings sess = do
    raw <- runQueryCommand sess QueryBindings
    baseline <- readIORef (sessBaselineBindings sess)
    pure (scrubBindings baseline raw)

{- | Result from the deliberately limited Path-2 feasibility prototype.
This is not the general primitive: GHCi's @:type@ accepts only expression-
local value bindings, not imports or type/class/instance declarations.
-}
data TypecheckResult = TypecheckResult
    { tcSucceeded :: Bool
    , tcDiagnostics :: Text
    }
    deriving (Eq, Show)

-- | The honest routing boundary of GHCi's no-add @:type@ mechanism.
data TypecheckInput = ValueExpression | ValueBindings | OutsideValueSubset
    deriving (Eq, Show)

classifyTypecheckInput :: Text -> TypecheckInput
classifyTypecheckInput source
    | any outside (meaningfulLines source) = OutsideValueSubset
    | any isBinding (meaningfulLines source) = ValueBindings
    | otherwise = ValueExpression
  where
    outside line = any (`T.isPrefixOf` lower line) excluded
    excluded =
        [ "data "
        , "newtype "
        , "type "
        , "class "
        , "instance "
        , "import "
        , "foreign "
        , "default "
        , "infix"
        , "{-#"
        , "-- cabal:"
        ]
    lower = T.map toLower . T.stripStart
    isBinding line =
        let lhs = T.takeWhile (/= '=') line
         in "=" `T.isInfixOf` line
                && not (T.null (T.strip lhs))
                && T.all validLhs lhs
    validLhs c = isAlphaNum c || isSpace c || c `elem` ("_'(),[]" :: String)

meaningfulLines :: Text -> [Text]
meaningfulLines = filter (not . T.null) . map T.strip . T.lines

{- | Type-check a value-binding group against the live interactive scope
without committing it. Disabled unless @SABELA_TYPECHECK_PRIMITIVE@ is set.
Newlines are rendered as explicit-let semicolons because GHCi commands occupy
one protocol line. The caller must not use this subset as a whole-cell gate.
-}
typecheckLetDeclarations :: Session -> Text -> IO TypecheckResult
typecheckLetDeclarations sess =
    typecheckValueWith
        (runQueryCommand sess . QueryType)
        (runQueryCommand sess QueryBindings)

{- | Run the proven Path-2 subset through any live-session backend. It snapshots
bindings itself: every query both enforces and reports the no-pollution
invariant instead of relying on callers to remember the check.
-}
typecheckValueWith :: (Text -> IO Text) -> IO Text -> Text -> IO TypecheckResult
typecheckValueWith askType askBindings source = do
    started <- getMonotonicTimeNSec
    enabled <- primitiveEnabled
    case (enabled, classifyTypecheckInput source) of
        (False, _) -> finish started "disabled" True "type-check primitive disabled" True
        (_, OutsideValueSubset) ->
            finish started "not-in-value-subset" False "not in the Path-2 value subset" True
        _ -> do
            before <- askBindings
            output <- askType (wrapped source)
            after <- askBindings
            let failed = any (`T.isInfixOf` output) failureSignals
                ok = not failed && expectedSuffix source `T.isInfixOf` output
            finish started (if ok then "ok" else "diagnostic") ok output (before == after)
  where
    failureSignals = ["error:", "Found hole:", "parse error"]
    finish started verdict ok diagnostics unchanged = do
        finished <- getMonotonicTimeNSec
        hPutStrLn stderr $
            "sabela_typecheck mode=path2-value verdict="
                <> verdict
                <> " no_pollution="
                <> map toLower (show unchanged)
                <> " latency_us="
                <> show ((finished - started) `div` 1000)
        pure
            ( TypecheckResult
                (ok && unchanged)
                ( if unchanged
                    then diagnostics
                    else diagnostics <> "\nPath-2 polluted live bindings"
                )
            )
    wrapped s = case classifyTypecheckInput s of
        ValueBindings ->
            ("(let { " <>) . (<> " } in ())") . T.intercalate "; " $ meaningfulLines s
        _ -> s
    expectedSuffix s = case classifyTypecheckInput s of
        ValueBindings -> ":: ()"
        _ -> "::"

primitiveEnabled :: IO Bool
primitiveEnabled = do
    value <- lookupEnv "SABELA_TYPECHECK_PRIMITIVE"
    pure $ maybe True ((`notElem` ["0", "off", "false", "no"]) . map toLower) value

{- | Snapshot the prelude-injected bindings as the baseline scrubbed from later
'queryBindings' results. Call once, right after prelude injection.
-}
captureBindingsBaseline :: Session -> IO ()
captureBindingsBaseline sess = do
    raw <- runQueryCommand sess QueryBindings
    writeIORef (sessBaselineBindings sess) (groupEntries raw)

{- | Drop the prelude-injected bindings (those in the baseline), GHCi's @it@, and
the internal @_sabela*@ refs from @:show bindings@ output, leaving the notebook's
own bindings.
-}
scrubBindings :: [Text] -> Text -> Text
scrubBindings baseline current =
    T.intercalate "\n" (filter keep (groupEntries current))
  where
    keep e = e `notElem` baseline && not (isHidden (T.strip e))
    -- @instance@ lines carry GHCi's @GhciN.@ counter, which bumps as cells run,
    -- so they drift from the baseline; drop them (they are not value bindings).
    isHidden s =
        any
            (`T.isPrefixOf` s)
            ["it ::", "it =", "_sab", "instance "]

{- | Split @:show bindings@ output into entries: an entry starts on a
non-indented line and absorbs the indented continuation lines that follow it.
-}
groupEntries :: Text -> [Text]
groupEntries = map (T.intercalate "\n") . collect . T.lines
  where
    collect [] = []
    collect (l : ls)
        | starts l = let (cont, rest) = span continues ls in (l : cont) : collect rest
        | otherwise = collect ls
    starts x = not (T.null x) && not (isSpace (T.head x))
    continues x = not (T.null x) && isSpace (T.head x)

{- | Serialise on 'sessQueryLock', not the cell run-lock: a query then
never blocks at the lock level behind a draining cell (the busy gate stays
admission control). The query lock still prevents query/query interleave,
which would corrupt GHCi's shared stdin/stdout stream (case 20).
-}
runQueryCommand :: Session -> QueryCommand -> IO Text
runQueryCommand sess cmd = withMVar (sessQueryLock sess) $ \_ -> do
    resetErrorBuffer sess
    mk <- getMarker sess
    sendRaw sess $ T.unpack $ toText cmd
    placeMarker sess mk
    mRes <-
        timeout queryTimeoutUs $
            drainUntilMarker (sessLines sess) (markerText mk) (\_ -> pure ())
    case mRes of
        Nothing -> pure "*** query timed out ***"
        Just dr -> do
            let out = T.strip (drainResultText dr)
            errLines <- readErrorBuffer sess
            pure $ if T.null out then errLines else out

data QueryCommand
    = QueryType Text
    | QueryInfo Text
    | QueryKind Text
    | QueryBrowse Text
    | QueryDoc Text
    | QueryHoleFits Text
    | QueryComplete Text
    | QueryBindings

toText :: QueryCommand -> Text
toText (QueryType t) = ":type " <> t
toText (QueryInfo t) = ":info " <> t
toText (QueryKind t) = ":kind " <> t
toText (QueryBrowse t) = ":browse " <> t
toText (QueryDoc t) = ":doc " <> t
toText (QueryHoleFits t) =
    ":set -fno-max-valid-hole-fits -frefinement-level-hole-fits=2\
    \ -fsort-by-subsumption-hole-fits\n"
        <> t
toText (QueryComplete t) = ":complete repl " <> t
toText QueryBindings = ":show bindings"
