{- | An MCP (Model Context Protocol) server over stdio for the Sabela AI tool
surface.

Speaks newline-delimited JSON-RPC 2.0 on stdin/stdout, so any MCP client
(goose, Claude Desktop, opencode) drives the notebook with structured tool
arguments. The model fills typed fields rather than building a shell command
string. Tool schemas are fetched at runtime from the live server's
@GET /api/ai/tools@ (the same @chatTools@ the chat loop uses), and @tools/call@
forwards through 'Siza.Transport.callTool', reusing the client's
auth/discovery/hub-token path.

Mutations (@insert_cell@/@replace_cell_source@/@propose_edit@) pass through the
same client-side pre-flight gate ('Siza.Preflight.preflight') the @siza tool@
command enforces. Here a block becomes an MCP tool error the model can recover
from rather than a process exit.

STDOUT PURITY: the @emit@ chokepoint is the only writer to stdout; every
diagnostic goes to stderr. A stray stdout write corrupts the JSON-RPC stream.
-}
module Siza.Mcp (
    runMcp,

    -- * Exposed for testing
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
import Siza.Language (Diagnostic, renderDiagnostic)
import Siza.Preflight (preflight)
import Siza.Security (Policy, advisoryPolicy)
import Siza.Transport (Conn, callTool, getTools)
import System.IO (
    BufferMode (LineBuffering),
    hPutStrLn,
    hSetBuffering,
    isEOF,
    stderr,
    stdout,
 )

-- | The MCP protocol version we advertise when the client does not pin one.
defaultProtocolVersion :: Text
defaultProtocolVersion = "2025-06-18"

{- | Serve MCP over stdio against the resolved server @base@ until stdin EOF.
The tool catalogue is fetched once at startup.
-}
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

-- | The ONLY writer to stdout. Everything else goes to stderr.
emit :: Value -> IO ()
emit = LBS8.putStrLn . encode

stripCR :: BS.ByteString -> BS.ByteString
stripCR bs
    | not (BS.null bs) && BS.last bs == 13 = BS.init bs
    | otherwise = bs

-- | Fetch @/api/ai/tools@ and project each entry to MCP's tool shape.
loadCatalogue :: Conn -> Text -> IO [Value]
loadCatalogue conn base = do
    mt <- getTools conn base
    case mt of
        Just (Array arr) -> do
            let cat = map toMcpTool (toList arr)
            hPutStrLn stderr ("siza mcp: serving " <> show (length cat) <> " tools")
            pure cat
        _ -> do
            hPutStrLn
                stderr
                "siza mcp: could not fetch /api/ai/tools; serving empty catalogue"
            pure []

{- | Project one server @ToolDef@ JSON to an MCP tool: rename @input_schema@ →
@inputSchema@ (the schema body is raw JSON Schema, passed through verbatim) and
drop Anthropic's @cache_control@. Other fields (name, description) are kept.
-}
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

-- | A parsed JSON-RPC request. A missing @id@ marks a notification.
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

{- | Wrap a result as a success response, but only when the request carried an
@id@. A notification (no @id@) gets no response. That is the JSON-RPC rule, and
the classic hand-rolled bug if forgotten.
-}
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
        ]

negotiatedVersion :: Value -> Text
negotiatedVersion (Object o) = case KM.lookup "protocolVersion" o of
    Just (String v) -> v
    _ -> defaultProtocolVersion
negotiatedVersion _ = defaultProtocolVersion

{- | Handle @tools/call@: gate mutations, forward via 'callTool', map the
@{isError,result}@ outcome onto an MCP tool result. Unknown tool names and gate
blocks become tool-level errors (@isError:true@) rather than protocol errors, so
the model gets feedback it can act on.
-}
toolsCall :: Conn -> Text -> Value -> IO Value
toolsCall conn base params =
    case parseToolName name of
        Nothing -> pure (toolResult True ("unknown tool: " <> name))
        Just tn -> do
            gate <- gateForMcp advisoryPolicy tn args
            case gate of
                Left ds -> pure (toolResult True (renderDiags ds))
                Right () -> do
                    res <- callTool conn base tn args
                    pure (outcomeResult res)
  where
    name = paramName params
    args = paramArgs params

outcomeResult :: Either Text ToolOutcome -> Value
outcomeResult (Left e) = toolResult True e
outcomeResult (Right o) =
    toolResult (toolOutcomeIsError o) (encodeText (toolOutcomeValue o))

{- | The pre-flight gate for MCP: block on a parse failure (always) or a denied
capability (under a strict 'Policy'); otherwise pass. Reuses
'Siza.Preflight.preflight', the same gate @siza tool@ runs, but returns a
verdict instead of exiting, since this server is long-lived.
-}
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
