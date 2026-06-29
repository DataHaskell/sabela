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
    captureBindingsBaseline,
    scrubBindings,
    groupEntries,
) where

import Control.Concurrent (withMVar)
import Data.Char (isSpace)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
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
