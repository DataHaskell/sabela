{-# LANGUAGE OverloadedStrings #-}

{- | The GHCi meta-commands a query is allowed to send, and the one place that
sends one: marker-framed, under the query locks, with a timeout.
-}
module Sabela.Session.Query.Command (
    QueryCommand (..),
    toText,
    runQueryCommand,
    queryTimeoutUs,
) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)

import Sabela.Session (
    Session (..),
    getMarker,
    markerText,
    placeMarker,
    readErrorBuffer,
    resetErrorBuffer,
    sendRaw,
    sessLines,
    withQueryLocks,
 )
import Sabela.Session.Drain (drainResultText, drainUntilMarker)

queryTimeoutUs :: Int
queryTimeoutUs = 10 * 1000000

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

runQueryCommand :: Session -> QueryCommand -> IO Text
runQueryCommand sess cmd = withQueryLocks sess $ do
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
