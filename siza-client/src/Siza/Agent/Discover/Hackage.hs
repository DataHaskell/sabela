{-# LANGUAGE ScopedTypeVariables #-}

{- | The @hackage@ source (docs/discover/search-api.md section 2): a
membership lookup over the sorted package-name list @data/hackage-packages.txt@
(built by @make search-cache@). A missing file is source-unavailability, never
absence — the distinction the envelope discloses per source.
-}
module Siza.Agent.Discover.Hackage (
    hackageNamesPath,
    loadHackageNames,
    hackageInfoFor,
    hackageMatching,
) where

import Control.Exception (SomeException, try)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import Siza.Agent.Discover.Types (HackageInfo (..))

-- | @SABELA_HACKAGE_NAMES@ overrides the default repo-relative path.
hackageNamesPath :: IO FilePath
hackageNamesPath =
    fromMaybe ("data" </> "hackage-packages.txt")
        <$> lookupEnv "SABELA_HACKAGE_NAMES"

-- | The package-name set, or Nothing when the cache is unavailable.
loadHackageNames :: IO (Maybe (S.Set Text))
loadHackageNames = do
    path <- hackageNamesPath
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            r <- try (TIO.readFile path)
            pure $ case r of
                Left (_ :: SomeException) -> Nothing
                Right t ->
                    Just
                        ( S.fromList
                            (filter (not . T.null) (map T.strip (T.lines t)))
                        )

-- | Which of the candidate package names exist upstream, plus availability.
hackageInfoFor :: [Text] -> IO HackageInfo
hackageInfoFor candidates = do
    mNames <- loadHackageNames
    pure $ case mNames of
        Nothing -> HackageInfo False []
        Just names ->
            HackageInfo True (filter (`S.member` names) candidates)

{- | Package names lexically matching any topic token (name substring, tokens
under 3 chars ignored), capped — the inventory mode's upstream candidates.
-}
hackageMatching :: Int -> [Text] -> IO [Text]
hackageMatching cap tokens = do
    mNames <- loadHackageNames
    pure $ case mNames of
        Nothing -> []
        Just names ->
            take cap [n | n <- S.toAscList names, any (`T.isInfixOf` n) usable]
  where
    usable = [T.toLower t | t <- tokens, T.length t >= 3]
