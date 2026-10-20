{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | The table view's runtime: the JavaScript a rendered table carries with
it, so an exported notebook or a shared page shows the same grid the editor
does with nothing to fetch.
-}
module Sabela.Output.Grid (gridDefs, gridWidgetJs) where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Sabela.Output.Fence (wrapRuntime)

gridModelJs :: Text
gridModelJs =
    TE.decodeUtf8
        $(makeRelativeToProject "static/src/widgets/grid-model.js" >>= embedFile)

gridViewJs :: Text
gridViewJs =
    TE.decodeUtf8
        $(makeRelativeToProject "static/src/widgets/grid-view.js" >>= embedFile)

gridQueryJs :: Text
gridQueryJs =
    TE.decodeUtf8
        $(makeRelativeToProject "static/src/widgets/grid-query.js" >>= embedFile)

gridBodyJs :: Text
gridBodyJs =
    TE.decodeUtf8
        $(makeRelativeToProject "static/src/widgets/grid-body.js" >>= embedFile)

{- | The runtime in dependency order: typing and statistics, the chrome drawn
around them, the expression box, then the body that mounts it all.
-}
gridWidgetJs :: Text
gridWidgetJs = T.intercalate "\n" [gridModelJs, gridViewJs, gridQueryJs, gridBodyJs]

{- | The runtime as a binding the injected prelude can splice into the markup
it prints.
-}
gridDefs :: Text
gridDefs =
    T.unlines
        [ "_sabelaGridJs :: SabelaBase.String"
        , "_sabelaGridJs = "
            <> T.pack (show (T.unpack (wrapRuntime "grid" gridWidgetJs)))
        ]
