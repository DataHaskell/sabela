{-# LANGUAGE OverloadedStrings #-}

module Eval.GateMetrics (renderGateMetrics) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Eval.Bench (tshow)
import Eval.Episode (voidPair)
import Eval.GateResult (GateResult (..), SearchMode (..), modeText)

renderGateMetrics :: FilePath -> [GateResult] -> IO Text
renderGateMetrics dir rs = do
    let pairs = nub [(grTask g, grSeed g) | g <- rs]
    effects <- catMaybes <$> mapM (leverEffect dir) pairs
    (emptyFinds, totalFinds, heals) <- discoveryEmpty dir rs
    let changed = length (filter id effects)
        nPairs = length effects
        inert = nPairs > 0 && changed == 0
    pure $
        T.unlines
            [ "Transcript tripwires:"
            , "  lever-effect: "
                <> tshow changed
                <> "/"
                <> tshow nPairs
                <> " task/seed pairs changed off→on"
                <> if inert
                    then "  ⚠ LEVER INERT — the pass-rate delta is sampling noise"
                    else ""
            , "  discovery-empty: "
                <> tshow emptyFinds
                <> "/"
                <> tshow totalFinds
                <> " discovery calls returned nothing"
            , "  self_heal-named: "
                <> tshow heals
                <> " repair announcements in tool responses"
            ]

leverEffect :: FilePath -> (Text, Int) -> IO (Maybe Bool)
leverEffect dir (task, seed) = do
    off <- readMaybe (transcriptPath dir task seed SearchOff)
    on <- readMaybe (transcriptPath dir task seed SearchOn)
    pure (differ <$> off <*> on)
  where
    differ a b = not (voidPair (TE.decodeUtf8 a) (TE.decodeUtf8 b))

discoveryEmpty :: FilePath -> [GateResult] -> IO (Int, Int, Int)
discoveryEmpty dir rs = do
    let paths = nub [transcriptPath dir (grTask g) (grSeed g) (grMode g) | g <- rs]
    texts <- catMaybes <$> mapM (fmap (fmap TE.decodeUtf8) . readMaybe) paths
    let total = sum (map (countAny findHeaders) texts)
        empty = sum (map (countAny emptyMarkers) texts)
        heals = sum (map (countAny ["\"self_heal\""]) texts)
    pure (empty, total, heals)
  where
    findHeaders =
        ["tool (find_package)", "tool (find_function)", "tool (discover)"]
    emptyMarkers =
        ["\"suggestions\":[]", "\"matches\":[]", "\"candidates\":[]"]
    countAny needles t = sum [T.count n t | n <- needles]

transcriptPath :: FilePath -> Text -> Int -> SearchMode -> FilePath
transcriptPath dir task seed mode =
    dir </> T.unpack (task <> "-s" <> tshow seed <> "-" <> modeText mode <> ".md")

readMaybe :: FilePath -> IO (Maybe BS.ByteString)
readMaybe p = do
    exists <- doesFileExist p
    if not exists
        then pure Nothing
        else either (const Nothing) Just <$> tryRead
  where
    tryRead = try (BS.readFile p) :: IO (Either SomeException BS.ByteString)
