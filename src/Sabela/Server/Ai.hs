{-# LANGUAGE OverloadedStrings #-}

{- | The AI surface served over HTTP: chat lifecycle (message / cancel /
clear / accept-edit / revert-edit), config get/set, and the REST bridge
external CLI clients (Siza) use to invoke individual tools.
-}
module Sabela.Server.Ai (
    -- * Chat
    chatMessageH,
    chatCancelH,
    chatClearH,
    chatAcceptEditH,
    chatRevertEditH,

    -- * Config
    getAIConfigH,
    setAIConfigH,

    -- * REST bridge
    aiHealthH,
    aiToolsH,
    aiNotebookH,
    aiToolH,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Servant (Handler, NoContent (..))

import Sabela.AI.Capabilities (acceptEdit, chatTools, executeTool, revertEdit)
import Sabela.AI.Doc (defaultDocOpts, renderNotebookDoc)
import Sabela.AI.Orchestrator (
    handleCancelTurn,
    handleChatMessage,
    handleClearChat,
 )
import Sabela.AI.Store (getAIConfig)
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (EditId (..), toolOutcomeIsError, toolOutcomeValue)
import Sabela.Anthropic.Types (AnthropicConfig (..), ToolDef, newCancelToken)
import Sabela.Api
import Sabela.Handlers (ReactiveNotebook)
import Sabela.Handlers.Shared (broadcast)
import Sabela.Model
import Sabela.State (
    AIConfigUpdate (..),
    App (..),
    getAIStore,
    resolveCliHandleStore,
    setAIStore,
    updateAIConfig,
 )
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (readNotebook)

------------------------------------------------------------------------
-- AI Config handlers
------------------------------------------------------------------------

getAIConfigH :: App -> Handler Value
getAIConfigH app = liftIO $ do
    mStore <- getAIStore app
    case mStore of
        Nothing ->
            pure $
                object
                    [ "configured" .= False
                    , "model" .= (Nothing :: Maybe Text)
                    , "models" .= knownModels
                    ]
        Just store -> do
            cfg <- getAIConfig store
            pure $
                object
                    [ "configured" .= True
                    , "model" .= acModel cfg
                    , "models" .= knownModels
                    ]

-- | Suggested model IDs shown in the picker. Custom values are also accepted.
knownModels :: [Value]
knownModels =
    [ modelEntry
        "claude-haiku-4-5-20251001"
        "Haiku 4.5"
        "Fast + cheap; best for high-frequency iteration"
    , modelEntry
        "claude-sonnet-4-6"
        "Sonnet 4.6"
        "Recommended balance of speed and capability"
    , modelEntry
        "claude-opus-4-7"
        "Opus 4.7"
        "Most capable; slower; use for hard reasoning"
    , modelEntry "claude-sonnet-4-20250514" "Sonnet 4 (legacy)" "Original default"
    ]
  where
    modelEntry :: Text -> Text -> Text -> Value
    modelEntry mid label desc =
        object ["id" .= mid, "label" .= label, "description" .= desc]

setAIConfigH :: App -> Value -> Handler Value
setAIConfigH app (Object o) = liftIO $ do
    let mKey = case KM.lookup (Key.fromText "apiKey") o of
            Just (String s) | not (T.null s) -> Just s
            _ -> Nothing
        mModel = case KM.lookup (Key.fromText "model") o of
            Just (String s) | not (T.null s) -> Just s
            _ -> Nothing
    if isNothing mKey && isNothing mModel
        then pure $ errorJson "apiKey or model is required"
        else do
            result <-
                updateAIConfig
                    app
                    AIConfigUpdate{aicuApiKey = mKey, aicuModel = mModel}
            case result of
                Right () -> do
                    mStore <- getAIStore app
                    currentModel <- case mStore of
                        Just store -> Just . acModel <$> getAIConfig store
                        Nothing -> pure Nothing
                    pure $
                        object
                            [ "configured" .= True
                            , "model" .= currentModel
                            ]
                Left err -> pure $ errorJson err
setAIConfigH _ _ = pure $ errorJson "Invalid request body"

------------------------------------------------------------------------
-- Chat (AI assistant) handlers
------------------------------------------------------------------------

chatMessageH :: App -> ReactiveNotebook -> ChatRequest -> Handler NoContent
chatMessageH app rn (ChatRequest msg) = liftIO $ do
    mStore <- getAIStore app
    case mStore of
        Nothing ->
            broadcast
                app
                (EvChatError Nothing "AI not configured. Open the Chat panel to set your API key.")
        Just store ->
            handleChatMessage app store rn msg
    pure NoContent

chatCancelH :: App -> Handler NoContent
chatCancelH app = liftIO $ do
    mStore <- getAIStore app
    for_ mStore (handleCancelTurn app)
    pure NoContent

chatClearH :: App -> Handler NoContent
chatClearH app = liftIO $ do
    mStore <- getAIStore app
    for_ mStore (handleClearChat app)
    pure NoContent

chatAcceptEditH :: App -> ReactiveNotebook -> Int -> Handler (Maybe Cell)
chatAcceptEditH app rn editIdInt = liftIO $ do
    mStore <- getAIStore app
    case mStore of
        Nothing -> pure Nothing
        Just store -> acceptEdit app store rn (EditId editIdInt)

chatRevertEditH :: App -> Int -> Handler NoContent
chatRevertEditH app editIdInt = liftIO $ do
    mStore <- getAIStore app
    case mStore of
        Nothing -> pure ()
        Just store -> revertEdit app store (EditId editIdInt)
    pure NoContent

------------------------------------------------------------------------
-- AI REST bridge (for external CLI skills — e.g. Siza)
------------------------------------------------------------------------

aiHealthH :: App -> Handler Value
aiHealthH app =
    pure $
        object
            [ "ok" .= True
            , "workDir" .= envWorkDir (appEnv app)
            , "authRequired" .= isJust (appAiToken app)
            ]

aiToolsH :: Handler [ToolDef]
aiToolsH = pure chatTools

aiNotebookH :: App -> Handler Value
aiNotebookH app = liftIO $ do
    nb <- readNotebook (appNotebook app)
    pure (renderNotebookDoc defaultDocOpts nb)

{- | Invoke a single AI tool from an external CLI client. Body: @{ name, input }@.
An optional @X-Sabela-Session@ header isolates @explore_result@ handles
between concurrent clients while they still see the same notebook.
-}
aiToolH ::
    App ->
    ReactiveNotebook ->
    Maybe Text ->
    Value ->
    Handler Value
aiToolH app rn mSession body = liftIO $ do
    let name = fromMaybe "" (stringField "name" body)
        input = fromMaybe (object []) (valueField "input" body)
    mStore <- ensureAIStoreForTools app
    case mStore of
        Nothing ->
            pure $
                object
                    [ "isError" .= True
                    , "result"
                        .= object
                            [ "error"
                                .= ( "Cannot execute tools: no HTTP manager available. Start sabela normally via cabal run." ::
                                        Text
                                   )
                            ]
                    ]
        Just store -> do
            storeForCall <- case mSession of
                Nothing -> pure store
                Just sid -> do
                    hs <- resolveCliHandleStore app sid
                    pure store{AIStore.aiHandles = hs}
            cancelTok <- newCancelToken
            outcome <- executeTool app storeForCall rn cancelTok name input
            pure $
                object
                    [ "isError" .= toolOutcomeIsError outcome
                    , "result" .= toolOutcomeValue outcome
                    ]

{- | Ensure an AIStore exists. The browser path requires a real API key for
Anthropic access, but the REST bridge only uses the store as a plumbing
object — handles, scratchpad, pending edits. If no store exists yet, we
build one with a placeholder config so read-only/tool-only flows work even
before the user configures a key.
-}
ensureAIStoreForTools :: App -> IO (Maybe AIStore.AIStore)
ensureAIStoreForTools app = do
    mStore <- getAIStore app
    case mStore of
        Just s -> pure (Just s)
        Nothing -> case appHttpMgr app of
            Nothing -> pure Nothing
            Just mgr -> do
                let cfg =
                        AnthropicConfig
                            { acApiKey = ""
                            , acModel = "placeholder"
                            , acBaseUrl = "https://api.anthropic.com"
                            }
                store <- AIStore.newAIStore cfg mgr
                setAIStore app (Just store)
                pure (Just store)

stringField :: Text -> Value -> Maybe Text
stringField k v = case valueField k v of
    Just (String s) -> Just s
    _ -> Nothing

valueField :: Text -> Value -> Maybe Value
valueField k (Object o) = KM.lookup (Key.fromText k) o
valueField _ _ = Nothing
