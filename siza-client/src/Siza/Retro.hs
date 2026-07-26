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

data RetroMetrics = RetroMetrics
    { rmTotal :: Int
    , rmPerTool :: [(ToolName, Int)]
    , rmErrors :: Int
    , rmScanHits :: Int
    , rmBlocks :: Int
    }
    deriving (Eq, Show)

decodeSession :: LBS8.ByteString -> [SessionEvent]
decodeSession raw =
    [ ev
    | ln <- LBS8.lines raw
    , not (LBS8.null (LBS8.dropWhile (== ' ') ln))
    , Just ev <- [A.decode ln]
    ]

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
