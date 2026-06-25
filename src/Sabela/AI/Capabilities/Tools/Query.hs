{-# LANGUAGE OverloadedStrings #-}

{- | Tool schemas for live GHCi introspection and result drill-down.
Sibling of "Sabela.AI.Capabilities.Tools.Notebook".
-}
module Sabela.AI.Capabilities.Tools.Query (queryTools) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Sabela.AI.Capabilities.ToolName (ToolName (..), mkTool)
import Sabela.Anthropic.Types (ToolDef)

queryTools :: [ToolDef]
queryTools =
    [ mkTool
        GhciQuery
        "Lightweight GHCi introspection against the live Haskell session: type, info, kind, doc, browse a module, holefits, or bindings. Much cheaper than execute_cell for syntax discovery. Use `browse` with a module name (e.g. \"DataFrame\") to list exports. Use `holefits` with a concrete goal type (e.g. \"_ :: [Int] -> Int\") to list in-scope names that fit it, so you pick a real name instead of inventing one. Use `bindings` to list every variable currently bound in the session with its type."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "op"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "enum"
                                .= (["type", "info", "kind", "browse", "doc", "holefits", "bindings"] :: [Text])
                            , "description"
                                .= ( "Which GHCi command to run (:type, :info, :kind, :browse, :doc, holefits, or bindings)." ::
                                        Text
                                   )
                            ]
                    , "arg"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description"
                                .= ( "For type/info/kind/doc: an expression, name, or type. For browse: a module name like \"DataFrame\". For holefits: a concrete typed hole like \"_ :: [Int] -> Int\". For bindings: ignored (pass an empty string)." ::
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
    , mkTool
        KernelStatus
        "Lock-free kernel status. Always answers, even while a cell holds the run-lock, so you can tell \"busy\" (a slow cell) from \"wedged\" (the server is unresponsive) without blocking. Returns a typed `state` tag (cold | idle | executing | building) plus `ksGen` (the restart counter) and `ebGeneration` (the edit/run fence)."
        noArgs
    , mkTool
        Interrupt
        "Abort the cell the Haskell kernel is currently running (group SIGINT). No-op when the kernel is idle. Use this when a cell is stuck before reaching for kernel_restart."
        noArgs
    , mkTool
        KernelRestart
        "Restart the Haskell kernel asynchronously. Returns immediately; poll kernel_status until the kernel is alive and idle again. Use when the kernel is wedged and interrupt did not free it."
        noArgs
    , mkTool
        AwaitIdle
        "Block until the running cascade finishes (a bounded ~45s long-poll on the execution-done fence, not a running==false sample). Returns immediately when the kernel is already idle. The reply carries a `waited` tag (settled | idle | timedOut | kernelDead) and a fresh kernel `status`; re-call while `waited` is timedOut and re-check the kernel when it is kernelDead. Cheaper than spinning on kernel_status."
        noArgs
    , mkTool
        ExportNotebook
        "Return every cell's id, position, type, language, and source in one call, so a full notebook sync is a single request rather than N read_cell round-trips."
        noArgs
    , mkTool
        PeekData
        "Peek at a delimited data file (CSV/TSV) in the work directory before loading it: returns the inferred delimiter, whether row one is a header, the first N rows, and a guessed type (Int|Double|Bool|Text) per column. Use this to shape a DataFrame read instead of guessing the schema."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "path"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description"
                                .= ("Path to the file, relative to the work directory." :: Text)
                            ]
                    , "n"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("Number of data rows to return (default 10)." :: Text)
                            ]
                    ]
            , "required" .= (["path"] :: [Text])
            ]
        )
    ]

-- | Schema for a tool that takes no arguments.
noArgs :: Value
noArgs = object ["type" .= ("object" :: Text), "properties" .= object []]
