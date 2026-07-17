module Siza.Agent.Transcript (renderTranscript, renderMessage) where

import Data.Aeson (Value (..), encode)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

renderTranscript :: Text -> [Value] -> Text
renderTranscript taskId msgs =
    T.intercalate "\n" $
        ["# Session: " <> taskId, ""]
            ++ concat (zipWith section [1 :: Int ..] msgs)

-- | One numbered message block, the unit a live debug trace emits as it streams.
renderMessage :: Int -> Value -> Text
renderMessage i = T.intercalate "\n" . section i

section :: Int -> Value -> [Text]
section i m =
    ["## " <> tshow i <> ". " <> roleLine m, ""]
        ++ thinkingBlock (strAt "thinking" m)
        ++ contentBlock (strAt "content" m)
        ++ callLines (lookupField "tool_calls" m)
        ++ [""]

thinkingBlock :: Text -> [Text]
thinkingBlock "" = []
thinkingBlock t = ["*thinking:*", "```", t, "```", ""]

roleLine :: Value -> Text
roleLine m = case strAt "tool_name" m of
    "" -> strAt "role" m
    tn -> strAt "role" m <> " (" <> tn <> ")"

contentBlock :: Text -> [Text]
contentBlock "" = []
contentBlock c = ["```", c, "```", ""]

callLines :: Maybe Value -> [Text]
callLines (Just (Array a)) = "**tool calls:**" : map callLine (toList a)
callLines _ = []

callLine :: Value -> Text
callLine v = case lookupField "function" v of
    Just f -> "- `" <> strAt "name" f <> "` " <> enc (lookupField "arguments" f)
    Nothing -> "- (unparsed call)"

strAt :: Text -> Value -> Text
strAt k (Object o) = case KM.lookup (textKey k) o of
    Just (String s) -> s
    _ -> ""
strAt _ _ = ""

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KM.lookup (textKey k) o
lookupField _ _ = Nothing

textKey :: Text -> K.Key
textKey = K.fromText

enc :: Maybe Value -> Text
enc Nothing = ""
enc (Just v) = TE.decodeUtf8 (LBS.toStrict (encode v))

tshow :: (Show a) => a -> Text
tshow = T.pack . show
