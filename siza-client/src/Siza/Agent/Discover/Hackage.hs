{-# LANGUAGE ScopedTypeVariables #-}

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

hackageNamesPath :: IO FilePath
hackageNamesPath =
    fromMaybe ("data" </> "hackage-packages.txt")
        <$> lookupEnv "SABELA_HACKAGE_NAMES"

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

hackageInfoFor :: [Text] -> IO HackageInfo
hackageInfoFor candidates = do
    mNames <- loadHackageNames
    pure $ case mNames of
        Nothing -> HackageInfo False []
        Just names -> HackageInfo True (concatMap (canonical names) candidates)
  where
    canonical names c = case [n | n <- S.toAscList names, eqIgnoreCase n c] of
        (n : _) -> [n]
        [] -> []
    eqIgnoreCase a b = T.toLower a == T.toLower b

hackageMatching :: Int -> [Text] -> IO [Text]
hackageMatching cap tokens = do
    mNames <- loadHackageNames
    pure $ case mNames of
        Nothing -> []
        Just names ->
            take
                cap
                [n | n <- S.toAscList names, any (`T.isInfixOf` T.toLower n) usable]
  where
    usable = [T.toLower t | t <- tokens, T.length t >= 3]
