{-# LANGUAGE OverloadedStrings #-}

{- | The unit a synthetic episode is planned in.
A beat carries both the messages it renders to and the metrics it is worth,
so the expected numbers are counted off the plan, never off the parser.
-}
module Test.RetroBeat (
    Beat (..),
    Truth (..),
    assistantMsg,
    beatMessages,
    episodeTruth,
    toolMsg,
) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Map.Strict as M
import Data.Text (Text)

{- | Everything a plan asserts about the metrics of the episode it renders.
Counts only; the order-sensitive numbers are pinned by their own properties.
-}
data Truth = Truth
    { trTurns :: Int
    , trCalls :: M.Map Text Int
    , trAttempted :: Int
    , trCommitted :: Int
    , trRejected :: Int
    , trRepeats :: Int
    , trUnchangedDiags :: Int
    , trUnknownDiags :: Int
    , trDuplicates :: Int
    , trElided :: Int
    , trElidedFull :: Int
    , trPayload :: M.Map Text Int
    , trThinking :: Int
    , trPrompt :: Int
    }
    deriving (Eq, Show)

instance Semigroup Truth where
    a <> b =
        Truth
            { trTurns = trTurns a + trTurns b
            , trCalls = M.unionWith (+) (trCalls a) (trCalls b)
            , trAttempted = trAttempted a + trAttempted b
            , trCommitted = trCommitted a + trCommitted b
            , trRejected = trRejected a + trRejected b
            , trRepeats = trRepeats a + trRepeats b
            , trUnchangedDiags = trUnchangedDiags a + trUnchangedDiags b
            , trUnknownDiags = trUnknownDiags a + trUnknownDiags b
            , trDuplicates = trDuplicates a + trDuplicates b
            , trElided = trElided a + trElided b
            , trElidedFull = trElidedFull a + trElidedFull b
            , trPayload = M.unionWith (+) (trPayload a) (trPayload b)
            , trThinking = trThinking a + trThinking b
            , trPrompt = trPrompt a + trPrompt b
            }

instance Monoid Truth where
    mempty = Truth 0 M.empty 0 0 0 0 0 0 0 0 0 M.empty 0 0

-- | One planned move of an episode, with the messages it renders to.
data Beat = Beat
    { beatMsgs :: [Value]
    , beatTruth :: Truth
    }

instance Show Beat where
    show = show . beatMsgs

beatMessages :: [Beat] -> [Value]
beatMessages = concatMap beatMsgs

episodeTruth :: [Beat] -> Truth
episodeTruth = mconcat . map beatTruth

assistantMsg :: Text -> Text -> [(Text, Value)] -> Value
assistantMsg think content calls =
    object $
        [ "role" .= ("assistant" :: Text)
        , "thinking" .= think
        , "content" .= content
        ]
            <> [ "tool_calls"
                    .= [ object ["function" .= object ["name" .= n, "arguments" .= a]]
                       | (n, a) <- calls
                       ]
               | not (null calls)
               ]

toolMsg :: Text -> Text -> Value
toolMsg name content =
    object
        [ "role" .= ("tool" :: Text)
        , "tool_name" .= name
        , "content" .= content
        ]
