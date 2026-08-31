{-# LANGUAGE OverloadedStrings #-}

module Eval.GateResult (
    GateResult (..),
    ContextMetric (..),
    SearchMode (..),
    modeText,
    gateKey,
    gateKeysForMetric,
    readGateResults,
    appendGateResult,
    isDone,
    renderGateResults,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    eitherDecodeStrict,
    encode,
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
 )
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.List (nub)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.IO (IOMode (AppendMode), hClose, hFlush, openFile)

import Eval.Bench (
    ArmCost (..),
    ArmResult (..),
    Comparison (..),
    RunStat (..),
    armCost,
    noiseCaveat,
    round1,
    tshow,
    twoProportionZ,
 )

data SearchMode = SearchOff | SearchOn
    deriving (Eq, Ord, Show)

data ContextMetric = LegacyTranscriptChars | EncodedRequestBodyBytes
    deriving (Eq, Show)

metricText :: ContextMetric -> Text
metricText LegacyTranscriptChars = "transcript_chars"
metricText EncodedRequestBodyBytes = "request_body_bytes"

metricFromText :: Text -> Maybe ContextMetric
metricFromText "transcript_chars" = Just LegacyTranscriptChars
metricFromText "request_body_bytes" = Just EncodedRequestBodyBytes
metricFromText _ = Nothing

modeText :: SearchMode -> Text
modeText SearchOff = "off"
modeText SearchOn = "on"

modeFromText :: Text -> Maybe SearchMode
modeFromText "off" = Just SearchOff
modeFromText "on" = Just SearchOn
modeFromText _ = Nothing

data GateResult = GateResult
    { grTask :: Text
    , grSeed :: Int
    , grMode :: SearchMode
    , grPass :: Bool
    , grTurns :: Int
    , grCalls :: Int
    , grStopped :: Text
    , grCtxChars :: Int
    -- ^ Legacy field name; 'grContextMetric' supplies its unit.
    , grContextMetric :: ContextMetric
    }
    deriving (Eq, Show)

instance ToJSON GateResult where
    toJSON g =
        object
            [ "grTask" .= grTask g
            , "grSeed" .= grSeed g
            , "grMode" .= modeText (grMode g)
            , "grPass" .= grPass g
            , "grTurns" .= grTurns g
            , "grCalls" .= grCalls g
            , "grStopped" .= grStopped g
            , "grCtxChars" .= grCtxChars g
            , "grCtxMetric" .= metricText (grContextMetric g)
            ]

instance FromJSON GateResult where
    parseJSON = withObject "GateResult" $ \o -> do
        m <- o .: "grMode"
        mode <- maybe (fail ("bad grMode: " <> T.unpack m)) pure (modeFromText m)
        metricTag <- o .:? "grCtxMetric" .!= "transcript_chars"
        metric <-
            maybe
                (fail ("bad grCtxMetric: " <> T.unpack metricTag))
                pure
                (metricFromText metricTag)
        GateResult
            <$> o .: "grTask"
            <*> o .: "grSeed"
            <*> pure mode
            <*> o .: "grPass"
            <*> o .: "grTurns"
            <*> o .: "grCalls"
            <*> o .:? "grStopped" .!= ""
            <*> o .:? "grCtxChars" .!= 0
            <*> pure metric

gateKey :: GateResult -> (Text, Int, SearchMode)
gateKey g = (grTask g, grSeed g, grMode g)

gateKeysForMetric ::
    ContextMetric -> [GateResult] -> Set.Set (Text, Int, SearchMode)
gateKeysForMetric metric =
    Set.fromList . map gateKey . filter ((== metric) . grContextMetric)

readGateResults :: FilePath -> IO [GateResult]
readGateResults path = do
    exists <- doesFileExist path
    if not exists
        then pure []
        else do
            er <- try (BS8.readFile path)
            pure $ case er of
                Left (_ :: SomeException) -> []
                Right bs -> mapMaybe parseLine (BS8.lines bs)
  where
    parseLine l
        | BS8.null (BS8.dropWhile (== ' ') l) = Nothing
        | otherwise = either (const Nothing) Just (eitherDecodeStrict l)

appendGateResult :: FilePath -> GateResult -> IO ()
appendGateResult path g = do
    h <- openFile path AppendMode
    LBS8.hPutStrLn h (encode g)
    hFlush h
    hClose h

isDone :: Set.Set (Text, Int, SearchMode) -> Text -> Int -> SearchMode -> Bool
isDone done task seed mode = Set.member (task, seed, mode) done

renderGateResults :: [GateResult] -> Text
renderGateResults rs =
    T.unlines
        ("Per task (A=SearchOff, B=SearchOn):" : map taskRow (byTaskComparison measured))
        <> "\nOverall:\n"
        <> renderComparison (summarise measured)
        <> "\n"
        <> renderCost measured
        <> renderCtx rs
        <> infraNote measured
  where
    measured = [g | g <- rs, grContextMetric g == EncodedRequestBodyBytes]
    taskRow (tid, Comparison a b _ _) =
        "  " <> tid <> ":  A " <> rate a <> "   B " <> rate b
    rate r = tshow (arPasses r) <> "/" <> tshow (arRuns r)

summarise :: [GateResult] -> Comparison
summarise rs =
    Comparison a b (rate b - rate a) (twoProportionZ a b)
  where
    a = tally SearchOff
    b = tally SearchOn
    rate (ArmResult p n) = if n == 0 then 0 else fromIntegral p / fromIntegral n
    tally mode =
        ArmResult
            (length [() | g <- rs, grMode g == mode, grPass g])
            (length [() | g <- rs, grMode g == mode])

byTaskComparison :: [GateResult] -> [(Text, Comparison)]
byTaskComparison rs =
    [(tid, summarise [g | g <- rs, grTask g == tid]) | tid <- nub (map grTask rs)]

renderComparison :: Comparison -> Text
renderComparison (Comparison a b diff z) =
    T.unlines
        ( [ "Arm A (SearchOff): " <> rate a
          , "Arm B (SearchOn):  " <> rate b
          , "B - A: " <> tshow (round3 diff) <> "   z = " <> tshow (round3 z)
          , "significant at 5%: " <> tshow (abs z > 1.96)
          ]
            <> noiseCaveat z (arRuns a)
        )
  where
    rate r = tshow (arPasses r) <> "/" <> tshow (arRuns r)

renderCost :: [GateResult] -> Text
renderCost rs =
    T.unlines
        ( "Cost to pass (mean tool calls / turns over passing runs, A=Off B=On):"
            : map row (costByTask rs)
        )
        <> "Overall:  A "
        <> cell (armCost (statsFor SearchOff))
        <> "   B "
        <> cell (armCost (statsFor SearchOn))
        <> "\n"
  where
    statsFor mode = [stat g | g <- rs, grMode g == mode]
    row (tid, (a, b)) = "  " <> tid <> ":  A " <> cell a <> "   B " <> cell b
    cell (ArmCost n c t) =
        tshow (round1 c) <> "c/" <> tshow (round1 t) <> "t (" <> tshow n <> ")"

costByTask :: [GateResult] -> [(Text, (ArmCost, ArmCost))]
costByTask rs =
    [ (tid, (statsFor tid SearchOff, statsFor tid SearchOn))
    | tid <- nub (map grTask rs)
    ]
  where
    statsFor tid mode =
        armCost [stat g | g <- rs, grTask g == tid, grMode g == mode]

renderCtx :: [GateResult] -> Text
renderCtx rs
    | null measured = legacyNote
    | otherwise =
        T.unlines
            ( "Model request payload/task (mean cumulative bytes, A=Off B=On):"
                : [ "  " <> tid <> ":  A " <> mean tid SearchOff <> "   B " <> mean tid SearchOn
                  | tid <- nub (map grTask measured)
                  ]
            )
            <> "Overall:  A "
            <> meanAll SearchOff
            <> "   B "
            <> meanAll SearchOn
            <> "\n"
            <> legacyNote
  where
    measured = [g | g <- rs, grContextMetric g == EncodedRequestBodyBytes]
    legacyN = length rs - length measured
    mean tid mode = fmt [grCtxChars g | g <- measured, grTask g == tid, grMode g == mode]
    meanAll mode = fmt [grCtxChars g | g <- measured, grMode g == mode]
    fmt [] = "-"
    fmt xs =
        tshow (round1 (fromIntegral (sum xs) / (1000 * fromIntegral (length xs))))
            <> "k"
    legacyNote
        | legacyN == 0 = ""
        | otherwise =
            "Excluded legacy context rows: "
                <> tshow legacyN
                <> " (final-transcript chars; not mixed with request bytes).\n"

infraNote :: [GateResult] -> Text
infraNote rs
    | n == 0 = ""
    | otherwise = "infra-errored episodes: " <> tshow n <> "\n"
  where
    n = length [() | g <- rs, grStopped g == "error"]

stat :: GateResult -> RunStat
stat g = RunStat (grPass g) (grTurns g) (grCalls g)

round3 :: Double -> Double
round3 x = fromIntegral (round (x * 1000) :: Int) / 1000
