{- | @siza retro@: read a session's append-only JSONL provenance log and fold
it into the adaptation-loop metrics (redesign 7.5).

The metrics fall straight out of the log because each line is a typed
'SessionEvent': per-tool call counts, the error rate, the security-scan hit
rate, and the count of pre-flight blocks. This is the read side of Part 7 — the
write side is 'Siza.Provenance'. It computes; the CLI ('Siza.Cli.Retro') reads
the file and prints.
-}
module Siza.Retro (
    RetroMetrics (..),
    computeMetrics,
    decodeSession,
    metricsValue,
) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Map.Strict as M
import Sabela.AI.Capabilities.ToolName (ToolName, toolWireName)
import Sabela.AI.Types (ToolOutcome (ToolErr))
import Siza.Provenance (
    Preflight (..),
    SessionEvent (..),
 )

{- | The session metrics, derived from one JSONL log. @rmErrors@ /
@rmTotal@ is the error rate; @rmScanHits@ / @rmTotal@ is the security-scan hit
rate; @rmBlocks@ counts pre-flight blocks (an unvetted mutation).
-}
data RetroMetrics = RetroMetrics
    { rmTotal :: Int
    , rmPerTool :: [(ToolName, Int)]
    , rmErrors :: Int
    , rmScanHits :: Int
    , rmBlocks :: Int
    }
    deriving (Eq, Show)

{- | Decode a JSONL log into its events, skipping blank lines. A line that
fails to decode is dropped, so a partially written tail never aborts the read.
-}
decodeSession :: LBS8.ByteString -> [SessionEvent]
decodeSession raw =
    [ ev
    | ln <- LBS8.lines raw
    , not (LBS8.null (LBS8.dropWhile (== ' ') ln))
    , Just ev <- [A.decode ln]
    ]

-- | Fold a session's events into the metric counts.
computeMetrics :: [SessionEvent] -> RetroMetrics
computeMetrics evs =
    RetroMetrics
        { rmTotal = length evs
        , rmPerTool = perTool evs
        , rmErrors = count isError evs
        , rmScanHits = count hasFinding evs
        , rmBlocks = count isBlocked evs
        }
  where
    count p = length . filter p
    isError e = case seOutcome e of ToolErr _ -> True; _ -> False
    hasFinding e = maybe False (not . null . pfFindings) (sePreflight e)
    isBlocked e = maybe False (not . pfVetted) (sePreflight e)

{- | Per-tool call counts, keyed on the wire name (the only 'Ord' handle on a
'ToolName'), recovering the typed 'ToolName' from each tool's first event.
-}
perTool :: [SessionEvent] -> [(ToolName, Int)]
perTool evs =
    [ (t, M.findWithDefault 0 (toolWireName t) counts)
    | t <- nubByWire (map seCall evs)
    ]
  where
    counts = M.fromListWith (+) [(toolWireName (seCall e), 1 :: Int) | e <- evs]
    nubByWire = go []
      where
        go _ [] = []
        go seen (t : ts)
            | w `elem` seen = go seen ts
            | otherwise = t : go (w : seen) ts
          where
            w = toolWireName t

-- | A small JSON summary for the CLI to print.
metricsValue :: RetroMetrics -> Value
metricsValue m =
    object
        [ "total" .= rmTotal m
        , "errors" .= rmErrors m
        , "errorRate" .= rate (rmErrors m)
        , "scanHits" .= rmScanHits m
        , "scanHitRate" .= rate (rmScanHits m)
        , "preflightBlocks" .= rmBlocks m
        , "perTool" .= object [keyOf t .= n | (t, n) <- rmPerTool m]
        ]
  where
    rate :: Int -> Double
    rate n = if rmTotal m == 0 then 0 else fromIntegral n / fromIntegral (rmTotal m)
    keyOf :: ToolName -> AK.Key
    keyOf = AK.fromText . toolWireName
