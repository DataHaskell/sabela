module Siza.Mcp (
    runMcp,
    Rpc (..),
    decodeRpc,
    initializeResult,
    toMcpTool,
    toolResult,
    successResp,
    errorResp,
    routeResponse,
    gateForMcp,
) where

import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Sabela.AI.Capabilities.ToolName (ToolName (..), parseToolName)
import Sabela.AI.Types (ToolOutcome, toolOutcomeIsError, toolOutcomeValue)
import Sabela.LLM.Ollama.Client (ToolCall (..))
import qualified Siza.Agent.Loop as AgentLoop
import qualified Siza.Agent.Tools as AgentTools
import Siza.Language (Diagnostic, renderDiagnostic)
import Siza.Preflight (preflight)
import Siza.Security (Policy, advisoryPolicy)
import Siza.Transport (Conn, callTool)
import System.IO (
    BufferMode (LineBuffering),
    hPutStrLn,
    hSetBuffering,
    isEOF,
    stderr,
    stdout,
 )

defaultProtocolVersion :: Text
defaultProtocolVersion = "2025-06-18"

runMcp :: Conn -> Text -> IO ()
runMcp conn base = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    catalogue <- loadCatalogue conn base
    loop catalogue
  where
    loop catalogue = do
        eof <- isEOF
        if eof
            then pure ()
            else do
                line <- BS8.getLine
                resp <- handleLine conn base catalogue (stripCR line)
                mapM_ emit resp
                loop catalogue

emit :: Value -> IO ()
emit = LBS8.putStrLn . encode

stripCR :: BS.ByteString -> BS.ByteString
stripCR bs
    | not (BS.null bs) && BS.last bs == 13 = BS.init bs
    | otherwise = bs

loadCatalogue :: Conn -> Text -> IO [Value]
loadCatalogue _ _ = do
    let cat = map agentToMcpTool (AgentTools.catalogueWith False)
    hPutStrLn stderr ("siza mcp: serving " <> show (length cat) <> " tools")
    pure cat

agentToMcpTool :: Value -> Value
agentToMcpTool (Object o)
    | Just (Object f) <- KM.lookup "function" o =
        object
            [ "name" .= fromMaybe Null (KM.lookup "name" f)
            , "description" .= fromMaybe Null (KM.lookup "description" f)
            , "inputSchema" .= fromMaybe (object []) (KM.lookup "parameters" f)
            ]
agentToMcpTool v = v

toMcpTool :: Value -> Value
toMcpTool (Object o) =
    Object
        . KM.insert "inputSchema" (fromMaybe (object []) (KM.lookup "input_schema" o))
        . KM.delete "input_schema"
        $ KM.delete "cache_control" o
toMcpTool v = v

handleLine :: Conn -> Text -> [Value] -> BS.ByteString -> IO (Maybe Value)
handleLine conn base catalogue line =
    case decodeRpc line of
        Left _ -> pure (Just (errorResp Null (-32700) "parse error"))
        Right rpc -> dispatch conn base catalogue rpc

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

dispatch :: Conn -> Text -> [Value] -> Rpc -> IO (Maybe Value)
dispatch conn base catalogue rpc = case rpcMethod rpc of
    "initialize" -> pure (routeResponse rpc (initializeResult (rpcParams rpc)))
    "notifications/initialized" -> pure Nothing
    "ping" -> pure (routeResponse rpc (object []))
    "tools/list" -> pure (routeResponse rpc (object ["tools" .= catalogue]))
    "tools/call" -> do
        result <- toolsCall conn base (rpcParams rpc)
        pure (routeResponse rpc result)
    other ->
        pure
            (fmap (\i -> errorResp i (-32601) ("method not found: " <> other)) (rpcId rpc))

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

initializeResult :: Value -> Value
initializeResult params =
    object
        [ "protocolVersion" .= negotiatedVersion params
        , "capabilities" .= object ["tools" .= object []]
        , "serverInfo"
            .= object ["name" .= ("siza" :: Text), "version" .= ("0.3.0" :: Text)]
        , "instructions" .= AgentLoop.mcpInstructions
        ]

negotiatedVersion :: Value -> Text
negotiatedVersion (Object o) = case KM.lookup "protocolVersion" o of
    Just (String v) -> v
    _ -> defaultProtocolVersion
negotiatedVersion _ = defaultProtocolVersion

toolsCall :: Conn -> Text -> Value -> IO Value
toolsCall conn base params =
    case parseToolName name of
        Nothing -> do
            res <- AgentTools.dispatch conn base (ToolCall name args)
            pure (outcomeResult res)
        Just tn -> do
            gate <- gateForMcp advisoryPolicy tn args
            case gate of
                Left ds -> pure (toolResult True (renderDiags ds))
                Right () -> do
                    res <- AgentTools.dispatch conn base (ToolCall name args)
                    pure (outcomeResult res)
  where
    name = paramName params
    args = paramArgs params

outcomeResult :: Either Text ToolOutcome -> Value
outcomeResult (Left e) = toolResult True e
outcomeResult (Right o) =
    toolResult (toolOutcomeIsError o) (encodeText (toolOutcomeValue o))

gateForMcp :: Policy -> ToolName -> Value -> IO (Either [Diagnostic] ())
gateForMcp policy name input
    | name `elem` [ReplaceCellSource, InsertCell, ProposeEdit]
    , Just src <- sourceField input =
        either Left (const (Right ())) <$> preflight policy src
    | otherwise = pure (Right ())

sourceField :: Value -> Maybe Text
sourceField (Object o) = case KM.lookup "source" o of
    Just (String s) -> Just s
    _ -> Nothing
sourceField _ = Nothing

paramName :: Value -> Text
paramName (Object o) = case KM.lookup "name" o of
    Just (String s) -> s
    _ -> ""
paramName _ = ""

paramArgs :: Value -> Value
paramArgs (Object o) = fromMaybe (object []) (KM.lookup "arguments" o)
paramArgs _ = object []

toolResult :: Bool -> Text -> Value
toolResult isErr txt =
    object
        [ "content" .= [object ["type" .= ("text" :: Text), "text" .= txt]]
        , "isError" .= isErr
        ]

renderDiags :: [Diagnostic] -> Text
renderDiags = T.intercalate "\n" . map renderDiagnostic

encodeText :: Value -> Text
encodeText (String s) = s
encodeText v = TE.decodeUtf8 (LBS.toStrict (encode v))
