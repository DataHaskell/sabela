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
    knownModels,

    -- * REST bridge
    aiHealthH,
    aiToolsH,
    aiNotebookH,
    aiToolH,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Result (..), Value (..), fromJSON, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Servant (Handler, NoContent (..))

import Sabela.AI.Capabilities (acceptEdit, chatTools, executeTool, revertEdit)
import Sabela.AI.Capabilities.Kernel (kernelStateBefore)
import Sabela.AI.Doc (defaultDocOpts, renderNotebookDoc)
import Sabela.AI.Orchestrator (
    handleCancelTurn,
    handleChatMessage,
    handleClearChat,
 )
import Sabela.AI.Provenance (Actor (..), recordToolCall)
import Sabela.AI.Store (getAIConfig, getAIProvider)
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
    getAINumCtx,
    getAIStore,
    getAIToolLimit,
    providerNameOf,
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
    numCtx <- getAINumCtx app
    toolLimit <- getAIToolLimit app
    let knobs = ["numCtx" .= numCtx, "toolLimit" .= toolLimit]
    mStore <- getAIStore app
    case mStore of
        Nothing ->
            pure $
                object $
                    [ "configured" .= False
                    , "model" .= (Nothing :: Maybe Text)
                    , "models" .= knownModels
                    ]
                        ++ knobs
        Just store -> do
            cfg <- getAIConfig store
            provider <- providerNameOf <$> getAIProvider store
            pure $
                object $
                    [ "configured" .= True
                    , "provider" .= provider
                    , "model" .= acModel cfg
                    , "models" .= knownModels
                    ]
                        ++ knobs

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
    let strField k = case KM.lookup (Key.fromText k) o of
            Just (String s) | not (T.null s) -> Just s
            _ -> Nothing
        intField k = case KM.lookup (Key.fromText k) o of
            Just v -> jsonInt v
            _ -> Nothing
        mKey = strField "apiKey"
        mModel = strField "model"
        mProvider = strField "provider"
        mNumCtx = intField "numCtx"
        mToolLimit = intField "toolLimit"
    if all isNothing [mKey, mModel, mProvider]
        && isNothing mNumCtx
        && isNothing mToolLimit
        then
            pure $ errorJson "apiKey, model, provider, numCtx, or toolLimit is required"
        else do
            result <-
                updateAIConfig
                    app
                    AIConfigUpdate
                        { aicuApiKey = mKey
                        , aicuModel = mModel
                        , aicuProvider = mProvider
                        , aicuNumCtx = mNumCtx
                        , aicuToolLimit = mToolLimit
                        }
            case result of
                Right () -> do
                    numCtx <- getAINumCtx app
                    toolLimit <- getAIToolLimit app
                    mStore <- getAIStore app
                    (curModel, curProvider) <- case mStore of
                        Just store -> do
                            m <- acModel <$> getAIConfig store
                            p <- providerNameOf <$> getAIProvider store
                            pure (Just m, Just p)
                        Nothing -> pure (Nothing, Nothing)
                    pure $
                        object
                            [ "configured" .= True
                            , "model" .= curModel
                            , "provider" .= curProvider
                            , "numCtx" .= numCtx
                            , "toolLimit" .= toolLimit
                            ]
                Left err -> pure $ errorJson err
setAIConfigH _ _ = pure $ errorJson "Invalid request body"

-- | A JSON value as an @Int@ (via aeson's own decoder); 'Nothing' if not integral.
jsonInt :: Value -> Maybe Int
jsonInt v = case fromJSON v of
    Success i -> Just i
    Error _ -> Nothing

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
                ( EvChatError
                    Nothing
                    "AI not configured. Open the Chat panel to set your API key."
                )
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
            (kBefore, gen) <- kernelStateBefore app
            outcome <- executeTool app storeForCall rn cancelTok name input
            recordToolCall
                (envWorkDir (appEnv app))
                mSession
                Agent
                name
                input
                outcome
                kBefore
                gen
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
