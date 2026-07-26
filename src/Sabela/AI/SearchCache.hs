{-# LANGUAGE OverloadedStrings #-}

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

data SearchSource = SearchSource
    { sourceLabel :: Text
    , sourceDetail :: Text
    , sourceUsable :: Bool
    }
    deriving (Eq, Show)

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

searchCacheReport :: IO [Text]
searchCacheReport = map render <$> searchCacheStatus
  where
    render s =
        "  search cache: "
            <> sourceLabel s
            <> " = "
            <> sourceDetail s
            <> if sourceUsable s then "" else "  [degraded]"
