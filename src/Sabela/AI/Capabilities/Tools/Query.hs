{-# LANGUAGE OverloadedStrings #-}

{- | Tool schemas for live GHCi introspection and result drill-down.
Sibling of "Sabela.AI.Capabilities.Tools.Notebook".
-}
module Sabela.AI.Capabilities.Tools.Query (queryTools) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import Sabela.AI.Capabilities.ToolName (ToolName (..), mkTool)
import Sabela.Anthropic.Types (ToolDef)

queryTools :: [ToolDef]
queryTools =
    [ mkTool
        ListBindings
        "List the values, functions, and types already defined in the notebook session, each with its type. Use BEFORE writing a cell that builds on earlier work, to reuse an existing binding (a dataset, a model, a helper) instead of recomputing it. find_function searches library APIs, not your bindings."
        noArgs
    , mkTool
        CheckType
        "Get the type of an expression, or the kind/definition of a type or class you ALREADY know, without running anything. Pass an expression (\"map fst\"), a value name, or a type/class name. When a value's type is a record, this ALSO returns that type's constructors and FIELD names — so to discover a config's fields for a record update (e.g. `cfg{maxTreeDepth=10}`), just check_type the value. To find a name you do not know, use find_function or find_by_type."
        (oneArg "expr" "An expression, value name, or type/class name.")
    , mkTool
        FindByType
        "Find an installed function whose TYPE matches a goal type. Pass a type like \"[Int] -> Int\" (or a hole \"_ :: [Int] -> Int\"). Use when you know the type you need but not the name. Differs from find_function, which searches by name or keyword."
        (oneArg "goal" "A goal type, e.g. \"[Int] -> Int\" or \"_ :: [Int] -> Int\".")
    , mkTool
        DescribeFunction
        "Show the haddock documentation (the prose explanation) for an installed function or type, by name. Use when you know the name and want to understand what it DOES, beyond its type. For the type alone use check_type; to find a name use find_function."
        (oneArg "name" "The function or type name.")
    , mkTool
        ApiReference
        "Fetch the curated signature card for the dataframe and granite plotting APIs (DataFrame, DataFrame.Functions, Granite.Spec, Granite.Svg). Works even before any cell has run — no live session needed. Pass a module substring, or omit for the whole card. For a live search of installed modules use find_function."
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
        "Hard-reset the Haskell kernel: force-kill the kernel process (even a wedged one that ignores interrupt) and respawn it clean — reusing the installed packages without rebuilding, and WITHOUT re-running any cells. Returns immediately; poll kernel_status until idle. This is how you recover a stuck or wedged kernel."
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
    , mkTool
        FindPackage
        "Find which Haskell package provides a capability you have not installed yet. Query a task in words (e.g. \"linear regression\", \"read csv\", \"plot a bar chart\"); returns ranked packages, each with the `-- cabal: build-depends:` line for a cell's first line and the key modules. This is the FIRST step for a new capability; once installed, use find_function to find the function inside it."
        ( queryArg
            "A keyword or task, e.g. \"linear regression\", \"read csv\", \"plotting\"."
        )
    , mkTool
        FindExampleCell
        "Search runnable example cells for a cell-shape idiom (e.g. \"read csv\", \"typed column\"); returns the title and full source to paste and adapt. Covers loading data and typed column access. To find which package or function does a task, use find_package or find_function."
        (queryArg "A shape idiom, e.g. \"read csv\" or \"typed column\".")
    , mkTool
        FindFunction
        "Find a function by NAME or KEYWORD in the packages already installed in the session, or list a module's exports by passing a module name (\"DataFrame\", \"Granite.Svg\"). Covers the dataframe and granite plotting APIs. Returns the best-matching functions with their module and signature, ranked; nothing on a true miss. To find a function by its TYPE use find_by_type; for a value you already defined use list_bindings; for a capability you have not installed use find_package."
        ( queryArg
            "A keyword (\"animate\") or a module name (\"DataFrame\", \"Granite.Svg\")."
        )
    , mkTool
        SearchCapability
        "Search ALL of Hackage by plain-language description, type signature, or keyword to DISCOVER packages you don't know. e.g. \"generate a QR code from text\", or a type \"Text -> Text -> Int\", or a name. Returns ranked candidate PACKAGES, each enriched with what you need to actually CALL it: its `-- cabal: build-depends:` line (paste as a cell's first line), the module(s) to import, and its KEY functions with type signatures (ranked to your query, so an action query surfaces the right function, not data accessors). Use this to find AND wire up the right library before importing it."
        (queryArg "A description, a type signature, or a name.")
    ]

-- | Schema for a tool that takes no arguments.
noArgs :: Value
noArgs = object ["type" .= ("object" :: Text), "properties" .= object []]

-- | Schema for a tool taking one required free-text field of the given name.
oneArg :: Text -> Text -> Value
oneArg name desc =
    object
        [ "type" .= ("object" :: Text)
        , "properties"
            .= object
                [ Key.fromText name
                    .= object ["type" .= ("string" :: Text), "description" .= desc]
                ]
        , "required" .= ([name] :: [Text])
        ]

-- | Schema for a tool that takes one required free-text @query@ string.
queryArg :: Text -> Value
queryArg = oneArg "query"
