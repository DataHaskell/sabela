{- | The JSON-RPC envelope the MCP server speaks: request decoding, the
notification rule (no id, no response), and the result shapes.
-}
module Siza.Mcp.Rpc (
    Rpc (..),
    decodeRpc,
    routeResponse,
    successResp,
    errorResp,
    toolResult,
    textBlock,
    encodeText,
    stripCR,
    paramName,
    paramArgs,
    paramUri,
    sourceField,
) where

import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

data Rpc = Rpc
    { rpcId :: Maybe Value
    , rpcMethod :: Text
    , rpcParams :: Value
    }
    deriving (Eq, Show)

decodeRpc :: BS.ByteString -> Either String Rpc
decodeRpc bs = do
    v <- eitherDecodeStrict bs
    case v of
        Object o ->
            Right
                Rpc
                    { rpcId = KM.lookup "id" o
                    , rpcMethod = case KM.lookup "method" o of
                        Just (String m) -> m
                        _ -> ""
                    , rpcParams = fromMaybe (object []) (KM.lookup "params" o)
                    }
        _ -> Left "not a JSON-RPC object"

-- | A notification (no id) gets no response, ever.
routeResponse :: Rpc -> Value -> Maybe Value
routeResponse rpc result = fmap (`successResp` result) (rpcId rpc)

successResp :: Value -> Value -> Value
successResp i result =
    object ["jsonrpc" .= ("2.0" :: Text), "id" .= i, "result" .= result]

errorResp :: Value -> Int -> Text -> Value
errorResp i code msg =
    object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= i
        , "error" .= object ["code" .= code, "message" .= msg]
        ]

toolResult :: Bool -> Text -> Value
toolResult isErr txt =
    object ["content" .= [textBlock txt], "isError" .= isErr]

textBlock :: Text -> Value
textBlock txt = object ["type" .= ("text" :: Text), "text" .= txt]

encodeText :: Value -> Text
encodeText (String s) = s
encodeText v = TE.decodeUtf8 (LBS.toStrict (encode v))

stripCR :: BS.ByteString -> BS.ByteString
stripCR bs
    | not (BS.null bs) && BS.last bs == 13 = BS.init bs
    | otherwise = bs

paramName :: Value -> Text
paramName (Object o) = case KM.lookup "name" o of
    Just (String s) -> s
    _ -> ""
paramName _ = ""

paramArgs :: Value -> Value
paramArgs (Object o) = fromMaybe (object []) (KM.lookup "arguments" o)
paramArgs _ = object []

paramUri :: Value -> Text
paramUri (Object o) = case KM.lookup "uri" o of
    Just (String s) -> s
    _ -> ""
paramUri _ = ""

sourceField :: Value -> Maybe Text
sourceField (Object o) = case KM.lookup "source" o of
    Just (String s) -> Just s
    _ -> Nothing
sourceField _ = Nothing
