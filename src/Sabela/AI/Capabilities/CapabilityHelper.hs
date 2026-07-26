{-# LANGUAGE OverloadedStrings #-}

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
