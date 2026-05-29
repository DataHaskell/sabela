{-# LANGUAGE OverloadedStrings #-}

-- | Tool schemas for live GHCi introspection and result drill-down.
-- Sibling of "Sabela.AI.Capabilities.Tools.Notebook".
module Sabela.AI.Capabilities.Tools.Query (queryTools) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Sabela.AI.Capabilities.ToolName (ToolName (..), mkTool)
import Sabela.Anthropic.Types (ToolDef)

queryTools :: [ToolDef]
queryTools =
    [ mkTool
        GhciQuery
        "Lightweight GHCi introspection against the live Haskell session: type, info, kind, doc, or browse a module. Much cheaper than execute_cell for syntax discovery. Use `browse` with a module name (e.g. \"DataFrame\") to list exports."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "op"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "enum" .= (["type", "info", "kind", "browse", "doc"] :: [Text])
                            , "description"
                                .= ("Which GHCi command to run (:type, :info, :kind, :browse, :doc)." :: Text)
                            ]
                    , "arg"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description"
                                .= ( "For type/info/kind/doc: an expression, name, or type. For browse: a module name like \"DataFrame\"." ::
                                        Text
                                   )
                            ]
                    ]
            , "required" .= (["op", "arg"] :: [Text])
            ]
        )
    , mkTool
        ApiReference
        "Fetch signatures for DataFrame, DataFrame.Functions, DataFrame.Display.Web.Plot, Granite.Svg (legacy one-shot charts), or the grammar-of-graphics API (Granite.Spec, Granite.Render.Pipeline). Pass a module name (substring match on the section header) to get that module's section, or omit/empty to get all. Output is cleaned :browse output. Use this before writing dataframe or granite code if you're uncertain of a signature."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "module"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description"
                                .= ( "Substring of the module name, e.g. \"DataFrame\", \"Functions\", \"Plot\", \"Granite.Svg\", \"Granite.Spec\", \"Pipeline\". Empty for all sections." ::
                                        Text
                                   )
                            ]
                    ]
            ]
        )
    , mkTool
        ExploreResult
        "Drill into a large result returned by a previous tool call. Use this when a tool returned a handleId instead of inlining the full payload."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "handle_id"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("The handleId from a prior tool result." :: Text)
                            ]
                    , "op"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "enum" .= (["head", "tail", "slice", "grep"] :: [Text])
                            ]
                    , "n"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("For head/tail: number of lines." :: Text)
                            ]
                    , "from"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("For slice: 1-based start line (inclusive)." :: Text)
                            ]
                    , "to"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("For slice: 1-based end line (inclusive)." :: Text)
                            ]
                    , "pattern"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("For grep: substring to search for." :: Text)
                            ]
                    ]
            , "required" .= (["handle_id", "op"] :: [Text])
            ]
        )
    ]
