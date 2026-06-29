{-# LANGUAGE OverloadedStrings #-}

{- | The SHIP capability-search helper: shell out to the committed cabal-script
retriever (@tools/capability_search.hs@) and parse its JSON. The helper runs
the proven high-recall pipeline (gpt-oss query expansion + nomic embedding +
BM25 + RRF + popularity rerank) against the offline index in @data/@.

This is the lever 'Sabela.AI.Capabilities.CapabilitySearch' prefers; it falls
back to the local-hoogle lexical query when the helper is absent, errors, or
returns no hits. The helper itself degrades to an empty list (never crashes)
when ollama is down or the index files are missing, so the fallback fires
cleanly. The path is configurable via @SABELA_CAPABILITY_HELPER@ (an executable
script) and the data dir via @SABELA_CAPABILITY_DATA_DIR@; defaults shell to
@cabal run -v0 tools/capability_search.hs@.

This adds an ollama runtime dependency for the high-recall path; it is optional
— without it the tool still works via the hoogle fallback. The first call after
a checkout compiles the cabal script (point @SABELA_CAPABILITY_HELPER@ at a
prebuilt binary to avoid that one-off cost).
-}
module Sabela.AI.Capabilities.CapabilityHelper (
    HelperHit (..),
    parseHelperHits,
    helperToHoogleHits,
    runCapabilityHelper,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON (..), eitherDecodeStrict', withObject, (.:?))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode)

import Sabela.AI.HoogleResolve (HoogleHit (..))

{- | One helper result row: a package name, its synopsis, and the SHIP rerank
score. Mirrors the JSON the Python helper prints:
@[{"package":..,"synopsis":..,"score":..}]@.
-}
data HelperHit = HelperHit
    { hePackage :: Text
    , heSynopsis :: Text
    , heScore :: Double
    }
    deriving (Eq, Show)

instance FromJSON HelperHit where
    parseJSON = withObject "HelperHit" $ \o -> do
        pkg <- o .:? "package"
        syn <- o .:? "synopsis"
        sc <- o .:? "score"
        pure
            HelperHit
                { hePackage = fromMaybe "" pkg
                , heSynopsis = fromMaybe "" syn
                , heScore = fromMaybe 0 sc
                }

{- | Parse the helper's stdout JSON array into hits. A leading @[]@ (the helper's
graceful-degrade output) yields no hits; malformed output yields no hits too, so
the caller falls back. Rows missing a non-empty @package@ are dropped.
-}
parseHelperHits :: Text -> [HelperHit]
parseHelperHits blob =
    case eitherDecodeStrict' (BS.pack (T.unpack (T.strip blob))) of
        Right hits -> filter (not . T.null . hePackage) hits
        Left _ -> tryLines
  where
    tryLines =
        [ h
        | ln <- T.lines blob
        , Just h <- [parseLine ln]
        , not (T.null (hePackage h))
        ]
    parseLine ln =
        case eitherDecodeStrict' (BS.pack (T.unpack (T.strip ln))) of
            Right v -> parseMaybe parseJSON v
            Left _ -> Nothing

{- | Map helper hits onto 'HoogleHit' so the existing @capabilityOutcome@ shaping
applies unchanged: the package name lands in both @name@ and @package@, the
synopsis in @docs@; @type@/@module@ are empty (the helper is package-level, not
symbol-level).
-}
helperToHoogleHits :: [HelperHit] -> [HoogleHit]
helperToHoogleHits = map toHit
  where
    toHit h =
        HoogleHit
            { hhName = hePackage h
            , hhPackage = hePackage h
            , hhModule = ""
            , hhType = ""
            , hhDocs = heSynopsis h
            }

{- | Run the SHIP helper for a query, returning its hits (or @[]@ on any failure:
helper absent, non-zero exit, malformed JSON, or thrown exception). The helper
path comes from @SABELA_CAPABILITY_HELPER@ (run directly) or defaults to
@cabal run -v0 tools/capability_search.hs@. @SABELA_CAPABILITY_DATA_DIR@, if set,
is passed through as @--data-dir@.
-}
runCapabilityHelper :: Int -> Text -> IO [HelperHit]
runCapabilityHelper k query
    | T.null (T.strip query) = pure []
    | otherwise = do
        mHelper <- lookupEnv "SABELA_CAPABILITY_HELPER"
        mDataDir <- lookupEnv "SABELA_CAPABILITY_DATA_DIR"
        let (cmd, baseArgs) = case mHelper of
                Just h -> (h, [])
                Nothing -> ("cabal", ["run", "-v0", "tools/capability_search.hs", "--"])
            dataArgs = maybe [] (\d -> ["--data-dir", d]) mDataDir
            args = baseArgs ++ [T.unpack query, "--top-k", show k] ++ dataArgs
        r <-
            try (readCreateProcessWithExitCode (proc cmd args) "") ::
                IO (Either SomeException (ExitCode, String, String))
        pure $ case r of
            Right (ExitSuccess, out, _) -> parseHelperHits (T.pack out)
            _ -> []
