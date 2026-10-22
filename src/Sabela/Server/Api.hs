{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Sabela.Server.Api (
    JsonAPI,
    FullAPI,
    fullProxy,
) where

import Data.Aeson (Value)
import Data.Text (Text)
import Servant

import Sabela.Anthropic.Types (ToolDef)
import Sabela.Api
import Sabela.Api.Bindings (BindingsResult)
import Sabela.Api.Components (
    ComponentBundle,
    ComponentInfo,
    ExtractAccept,
    ExtractPreview,
    ExtractRequest,
    InstallRequest,
 )
import Sabela.Api.FrameQuery (FrameQueryRequest, FrameQueryResult)
import Sabela.Api.Viz (VizSourceRequest, VizSourceResult)
import Sabela.Api.Viz.Columns (VizColumns)
import Sabela.Model
import Sabela.Server.ImportPath (ImportPathRequest, ImportPathResult)
import qualified Sabela.SessionTypes as ST

type JsonAPI =
    "api" :> "notebook" :> Get '[JSON] Notebook
        :<|> "api" :> "load" :> ReqBody '[JSON] LoadRequest :> Post '[JSON] Notebook
        :<|> "api" :> "save" :> ReqBody '[JSON] SaveRequest :> Post '[JSON] Notebook
        :<|> "api"
            :> "cell"
            :> Capture "id" Int
            :> Header "X-Sabela-Session" Text
            :> ReqBody '[JSON] UpdateCell
            :> Put '[JSON] Cell
        :<|> "api"
            :> "cell"
            :> Capture "id" Int
            :> "source"
            :> Header "X-Sabela-Session" Text
            :> ReqBody '[JSON] UpdateCell
            :> Put '[JSON] Cell
        :<|> "api" :> "cell" :> ReqBody '[JSON] InsertCell :> Post '[JSON] Cell
        :<|> "api"
            :> "cell"
            :> Capture "id" Int
            :> Delete '[JSON] Notebook
        :<|> "api" :> "run" :> Capture "id" Int :> Post '[JSON] RunResult
        :<|> "api" :> "run-all" :> Post '[JSON] RunAllResult
        :<|> "api" :> "reset" :> Post '[JSON] Notebook
        :<|> "api" :> "restart-kernel" :> Post '[JSON] NoContent
        :<|> "api" :> "restart-run-all" :> Post '[JSON] NoContent
        :<|> "api" :> "interrupt" :> Post '[JSON] NoContent
        :<|> "api" :> "kernel" :> Get '[JSON] Value
        :<|> "api" :> "mode" :> Get '[JSON] RunModeUpdate
        :<|> "api" :> "mode" :> ReqBody '[JSON] RunModeUpdate :> Put '[JSON] RunModeUpdate
        :<|> "api" :> "clear" :> Capture "id" Int :> Post '[JSON] NoContent
        :<|> "api"
            :> "files"
            :> QueryParam "path" Text
            :> Get '[JSON] [FileEntry]
        :<|> "api" :> "file" :> QueryParam "path" Text :> Get '[JSON] Text
        :<|> "api"
            :> "file"
            :> "preview"
            :> QueryParam "path" Text
            :> QueryParam "offset" Int
            :> QueryParam "limit" Int
            :> Get '[JSON] FilePreview
        :<|> "api"
            :> "dataset"
            :> "preview"
            :> QueryParam "path" Text
            :> QueryParam "rows" Int
            :> Get '[JSON] DatasetPreview
        :<|> "api"
            :> "file"
            :> "create"
            :> ReqBody '[JSON] CreateFileRequest
            :> Post '[JSON] FileEntry
        :<|> "api"
            :> "file"
            :> "write"
            :> ReqBody '[JSON] WriteFileRequest
            :> Post '[JSON] Text
        :<|> "api"
            :> "file"
            :> "delete"
            :> ReqBody '[JSON] DeleteFileRequest
            :> Post '[JSON] NoContent
        :<|> "api"
            :> "file"
            :> "rename"
            :> ReqBody '[JSON] RenameFileRequest
            :> Post '[JSON] NoContent
        :<|> "api"
            :> "complete"
            :> ReqBody '[JSON] CompleteRequest
            :> Post '[JSON] CompleteResult
        :<|> "api"
            :> "info"
            :> ReqBody '[JSON] InfoRequest
            :> Post '[JSON] InfoResult
        :<|> "api" :> "bindings" :> Get '[JSON] BindingsResult
        :<|> "api"
            :> "frame"
            :> "query"
            :> ReqBody '[JSON] FrameQueryRequest
            :> Post '[JSON] FrameQueryResult
        :<|> "api"
            :> "viz"
            :> "source"
            :> ReqBody '[JSON] VizSourceRequest
            :> Post '[JSON] VizSourceResult
        :<|> "api"
            :> "viz"
            :> "columns"
            :> QueryParam "binding" Text
            :> Get '[JSON] VizColumns
        :<|> "api"
            :> "import-path"
            :> ReqBody '[JSON] ImportPathRequest
            :> Post '[JSON] ImportPathResult
        :<|> "api" :> "components" :> Get '[JSON] [ComponentInfo]
        :<|> "api"
            :> "components"
            :> "extract"
            :> ReqBody '[JSON] ExtractRequest
            :> Post '[JSON] ExtractPreview
        :<|> "api"
            :> "components"
            :> "accept"
            :> ReqBody '[JSON] ExtractAccept
            :> Post '[JSON] Notebook
        :<|> "api"
            :> "components"
            :> "bundle"
            :> QueryParam "module" Text
            :> Get '[JSON] ComponentBundle
        :<|> "api"
            :> "components"
            :> "install"
            :> ReqBody '[JSON] InstallRequest
            :> Post '[JSON] ComponentInfo
        :<|> "api" :> "examples" :> Get '[JSON] [Example]
        :<|> "api"
            :> "cell"
            :> Capture "id" Int
            :> "lang"
            :> ReqBody '[JSON] ST.CellLang
            :> Put '[JSON] Cell
        :<|> "api" :> "widget" :> ReqBody '[JSON] WidgetUpdate :> Post '[JSON] NoContent
        :<|> "api" :> "config" :> "ai" :> Get '[JSON] Value
        :<|> "api" :> "config" :> "ai" :> ReqBody '[JSON] Value :> Post '[JSON] Value
        :<|> "api" :> "chat" :> ReqBody '[JSON] ChatRequest :> Post '[JSON] NoContent
        :<|> "api" :> "chat" :> "cancel" :> Post '[JSON] NoContent
        :<|> "api" :> "chat" :> "clear" :> Post '[JSON] NoContent
        :<|> "api"
            :> "chat"
            :> "edit"
            :> Capture "editId" Int
            :> "accept"
            :> Post '[JSON] (Maybe Cell)
        :<|> "api"
            :> "chat"
            :> "edit"
            :> Capture "editId" Int
            :> "revert"
            :> Post '[JSON] NoContent
        :<|> "api" :> "ai" :> "health" :> Get '[JSON] Value
        :<|> "api" :> "ai" :> "tools" :> Get '[JSON] [ToolDef]
        :<|> "api" :> "ai" :> "notebook" :> Get '[JSON] Value
        :<|> "api"
            :> "ai"
            :> "tool"
            :> Header "X-Sabela-Session" Text
            :> ReqBody '[JSON] Value
            :> Post '[JSON] Value

type FullAPI =
    JsonAPI
        :<|> "api" :> "events" :> Raw
        :<|> "api" :> "export" :> "dashboard" :> Raw
        :<|> "api" :> "export" :> "slideshow" :> Raw
        :<|> "api" :> "export" :> "notebook" :> Raw
        :<|> "api" :> "export" :> "markdown" :> Raw
        :<|> "api" :> "export" :> "haskell" :> Raw
        :<|> "api" :> "export" :> "lhs" :> Raw
        :<|> "api" :> "export" :> "reactive" :> Raw
        :<|> "dashboard" :> Raw
        :<|> "slideshow" :> Raw
        :<|> "api" :> "asset" :> Raw
        :<|> "api" :> "upload" :> Raw
        :<|> "api" :> "import-url" :> Raw
        :<|> "vendor" :> Raw
        :<|> Raw

fullProxy :: Proxy FullAPI
fullProxy = Proxy
