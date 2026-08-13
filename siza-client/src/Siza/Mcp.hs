module Siza.Mcp (
    runMcp,
    McpEnv,
    mcpEnvOver,
    mcpStackFor,
    mcpSession,
    mcpCatalogue,
    handleLine,
    toolsCall,
    module Siza.Mcp.Rpc,
    initializeResult,
    toMcpTool,
    gateForMcp,
    gateForCall,
) where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..), toolOutcomeIsError, toolOutcomeValue)
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Sabela.AI.WriteAck (refusalAck)
import Sabela.Api (errorJsonWith)
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.GrammarCards (GrammarMode (..))
import qualified Siza.Agent.Loop as AgentLoop
import Siza.Agent.OutcomeDistill (distillOutcome)
import Siza.Agent.Stack (
    Dispatch,
    StackSession,
    Surface (..),
    SurfacePolicy (..),
    newSessionFor,
    sessionPolicy,
    stackDispatch,
 )
import Siza.Agent.Stack.Call (
    CallResult (..),
    foldNotes,
    noteCharBudget,
    runToolCall,
 )
import Siza.Agent.ToolRoute (Route (..), routeCallWith)
import qualified Siza.Agent.Tools as AgentTools
import Siza.Language (Diagnostic, renderDiagnostic)
import Siza.Mcp.Rpc
import Siza.Mcp.Surface (
    mcpCatalogue,
    notebookUri,
    promptGet,
    promptsList,
    resourcesList,
 )
import Siza.Preflight (preflight)
import Siza.Security (Policy, advisoryPolicy)
import Siza.Transport (Conn)
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

{- | One driving session: the stack-wrapped dispatch, the state it
accumulates, and the catalogue served to the client.
-}
data McpEnv = McpEnv
    { meDispatch :: Dispatch
    , meSession :: StackSession
    , meCatalogue :: [Value]
    }

{- | The stack the MCP server wraps every tool call in. Named so the parity
spec can drive the same layers the chat loop drives.
-}
mcpStackFor :: StackSession -> Dispatch -> Dispatch
mcpStackFor = stackDispatch

mcpEnvOver :: StackSession -> Dispatch -> [Value] -> McpEnv
mcpEnvOver ss inner = McpEnv (mcpStackFor ss (gateDispatch ss inner)) ss

{- | The session an MCP server runs with. What it does differently from the
chat loop is declared once, in 'Siza.Agent.Stack.surfacePolicy'.
-}
mcpSession :: IO StackSession
mcpSession = newSessionFor McpSurface GrammarOn ""

{- | The client-side gate, as a dispatch layer: a refusal is an outcome like
any other, so the stack's ownership, futility and note layers all see it.
-}
gateDispatch :: StackSession -> Dispatch -> Dispatch
gateDispatch ss inner call
    | not (spPreflights (sessionPolicy ss)) = inner call
    | otherwise = do
        gate <- gateForCall advisoryPolicy call
        case gate of
            Left ds -> pure (preflightRefusal (gatedSource call) ds)
            Right () -> inner call

{- | A refusal built by the constructor the server uses for one, so a write
stopped here reports what a write stopped there reports.
-}
preflightRefusal :: Text -> [Diagnostic] -> Either Text ToolOutcome
preflightRefusal src ds =
    Right (ToolErr (refusalAck "preflight" Nothing (errorJsonWith message pairs)))
  where
    message =
        "This write was refused before it reached the notebook: the source \
        \does not parse, or is not permitted. Nothing was committed."
    pairs =
        [ "verdict" .= verdictTag VerdictDiagnostic
        , "diagnostic" .= renderDiags ds
        , "source" .= src
        ]

-- | The source a gated write submitted, as the router resolved it.
gatedSource :: ToolCall -> Text
gatedSource call = case routeCallWith AgentTools.offeredArgKeys call of
    RouteTool tn args | tn `elem` gatedTools -> fromMaybe "" (sourceField args)
    _ -> ""

gatedTools :: [ToolName]
gatedTools = [ReplaceCellSource, InsertCell, ProposeEdit]

runMcp :: Conn -> Text -> IO ()
runMcp conn base = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    catalogue <- loadCatalogue conn base
    ss <- mcpSession
    loop (mcpEnvOver ss (AgentTools.dispatch conn base) catalogue)
  where
    loop env = do
        eof <- isEOF
        if eof
            then pure ()
            else do
                line <- BS8.getLine
                resp <- handleLine env (stripCR line)
                mapM_ emit resp
                loop env

emit :: Value -> IO ()
emit = LBS8.putStrLn . encode

loadCatalogue :: Conn -> Text -> IO [Value]
loadCatalogue _ _ = do
    hPutStrLn
        stderr
        ("siza mcp: serving " <> show (length mcpCatalogue) <> " tools")
    pure mcpCatalogue

toMcpTool :: Value -> Value
toMcpTool (Object o) =
    Object
        . KM.insert "inputSchema" (fromMaybe (object []) (KM.lookup "input_schema" o))
        . KM.delete "input_schema"
        $ KM.delete "cache_control" o
toMcpTool v = v

handleLine :: McpEnv -> BS.ByteString -> IO (Maybe Value)
handleLine env line =
    case decodeRpc line of
        Left _ -> pure (Just (errorResp Null (-32700) "parse error"))
        Right rpc -> dispatch env rpc

dispatch :: McpEnv -> Rpc -> IO (Maybe Value)
dispatch env rpc = case rpcMethod rpc of
    "initialize" -> pure (routeResponse rpc (initializeResult (rpcParams rpc)))
    "notifications/initialized" -> pure Nothing
    "ping" -> pure (routeResponse rpc (object []))
    "tools/list" -> pure (routeResponse rpc (object ["tools" .= meCatalogue env]))
    "tools/call" -> do
        result <- toolsCall env (rpcParams rpc)
        pure (routeResponse rpc result)
    "prompts/list" -> pure (routeResponse rpc promptsList)
    "prompts/get" -> case promptGet (paramName (rpcParams rpc)) of
        Just p -> pure (routeResponse rpc p)
        Nothing ->
            pure
                ( fmap
                    (\i -> errorResp i (-32602) "unknown prompt")
                    (rpcId rpc)
                )
    "resources/list" -> pure (routeResponse rpc resourcesList)
    "resources/read" -> do
        contents <- readResource env (paramUri (rpcParams rpc))
        pure (routeResponse rpc contents)
    other ->
        pure
            (fmap (\i -> errorResp i (-32601) ("method not found: " <> other)) (rpcId rpc))

initializeResult :: Value -> Value
initializeResult params =
    object
        [ "protocolVersion" .= negotiatedVersion params
        , "capabilities"
            .= object
                [ "tools" .= object []
                , "prompts" .= object []
                , "resources" .= object []
                ]
        , "serverInfo"
            .= object ["name" .= ("siza" :: Text), "version" .= ("0.3.0" :: Text)]
        , "instructions" .= AgentLoop.mcpInstructions
        ]

negotiatedVersion :: Value -> Text
negotiatedVersion (Object o) = case KM.lookup "protocolVersion" o of
    Just (String v) -> v
    _ -> defaultProtocolVersion
negotiatedVersion _ = defaultProtocolVersion

toolsCall :: McpEnv -> Value -> IO Value
toolsCall env params =
    stackResult <$> runToolCall (meSession env) (meDispatch env) call
  where
    call = ToolCall (paramName params) (paramArgs params)

{- | The tool's own result first, anything the stack did behind the caller's
back in a second block, so a client reading one block still reads clean output.
-}
stackResult :: CallResult -> Value
stackResult cr =
    object
        [ "content" .= (textBlock (renderForMcp (crOutcome cr)) : noteBlocks)
        , "isError" .= either (const True) toolOutcomeIsError (crOutcome cr)
        ]
  where
    noteBlocks =
        [textBlock n | Just n <- [foldNotes noteCharBudget (crNotes cr)]]

{- | Structured JSON, distilled: a client parses this, so the chat labels stay
on the chat path and only the output shedding is shared.
-}
renderForMcp :: Either Text ToolOutcome -> Text
renderForMcp (Left e) = e
renderForMcp (Right o) = encodeText (distillOutcome (toolOutcomeValue o))

{- | Gate on the ROUTE, not on the raw name: a client-side tool whose name
never parses would otherwise skip the gate entirely.
-}
gateForCall :: Policy -> ToolCall -> IO (Either [Diagnostic] ())
gateForCall policy call = case routeCallWith AgentTools.offeredArgKeys call of
    RouteTool tn args -> gateForMcp policy tn args
    _ -> pure (Right ())

gateForMcp :: Policy -> ToolName -> Value -> IO (Either [Diagnostic] ())
gateForMcp policy name input
    | name `elem` gatedTools
    , Just src <- sourceField input =
        either Left (const (Right ())) <$> preflight policy src
    | otherwise = pure (Right ())

{- | The notebook, read live. A resource nobody can read is advertising, so
this goes through the same dispatch a tool call would.
-}
readResource :: McpEnv -> Text -> IO Value
readResource env uri
    | uri /= notebookUri =
        pure (object ["contents" .= ([] :: [Value])])
    | otherwise = do
        out <- meDispatch env (ToolCall "list_cells" (object ["full" .= True]))
        pure
            ( object
                [ "contents"
                    .= [ object
                            [ "uri" .= notebookUri
                            , "mimeType" .= ("application/json" :: Text)
                            , "text" .= renderForMcp out
                            ]
                       ]
                ]
            )

renderDiags :: [Diagnostic] -> Text
renderDiags = T.intercalate "\n" . map renderDiagnostic
