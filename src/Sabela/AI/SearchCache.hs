{-# LANGUAGE OverloadedStrings #-}

{- | What the local search cache actually resolves to, reported at startup.

Every hoogle-backed lookup degrades silently: 'Sabela.AI.HoogleClient.runHoogle'
swallows a missing binary, and 'hoogleDbArgSets' drops a configured database
whose file is absent. A resolver running with no local database therefore looks
exactly like one that found nothing, and @data\/hoogle-local.hoo@ went
ungenerated for months without a word. This turns each of those into a line the
operator sees.
-}
module Sabela.AI.SearchCache (
    SearchSource (..),
    searchCacheStatus,
    searchCacheReport,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist, findExecutable)
import System.Environment (lookupEnv)

{- | One configured search input and whether it resolves. 'sourceUsable' is the
question the resolver silently answers for itself.
-}
data SearchSource = SearchSource
    { sourceLabel :: Text
    , sourceDetail :: Text
    , sourceUsable :: Bool
    }
    deriving (Eq, Show)

-- | Resolve every local search input: the binary, then each database.
searchCacheStatus :: IO [SearchSource]
searchCacheStatus = do
    bin <- fromMaybe "hoogle" <$> lookupEnv "SABELA_HOOGLE_BIN"
    found <- findExecutable bin
    mainDb <- lookupEnv "SABELA_HOOGLE_DB"
    localDb <- lookupEnv "SABELA_HOOGLE_LOCAL_DB"
    mainStatus <- dbSource "SABELA_HOOGLE_DB" hoogleDefaultNote mainDb
    localStatus <- dbSource "SABELA_HOOGLE_LOCAL_DB" localUnsetNote localDb
    pure
        [ SearchSource
            "hoogle binary"
            (maybe (T.pack bin <> " NOT FOUND on PATH") T.pack found)
            (not (null found))
        , mainStatus
        , localStatus
        ]
  where
    hoogleDefaultNote = "unset — using hoogle's own default database"
    localUnsetNote = "unset — installed-package symbols are unindexed"

{- | A database input's status. An unset variable is reported with the caller's
note and counts as usable, because the resolver has a defined behaviour without
it; a SET variable pointing at a missing file does not.
-}
dbSource :: Text -> Text -> Maybe String -> IO SearchSource
dbSource label unsetNote mPath = case mPath of
    Nothing -> pure (SearchSource label unsetNote True)
    Just path -> do
        ok <- doesFileExist path
        pure
            ( SearchSource
                label
                (T.pack path <> if ok then "" else " MISSING")
                ok
            )

{- | The startup lines, one per input. Every source is named whether or not it
resolves: a silent success and a silent failure must not look alike.
-}
searchCacheReport :: IO [Text]
searchCacheReport = map render <$> searchCacheStatus
  where
    render s =
        "  search cache: "
            <> sourceLabel s
            <> " = "
            <> sourceDetail s
            <> if sourceUsable s then "" else "  [degraded]"
