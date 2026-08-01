{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.PromptCore (
    sharedPromptCore,
    sharedPromptCoreWith,
    sabelaBuiltins,
    builtinNames,
    builtinModules,
    drawingBuiltins,
    toolSurfaceBlock,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ToolName (toolWireName)
import Sabela.AI.Grammar (grammarPromptBlock)
import Sabela.LLM.Tool (ToolSpec (..))

sharedPromptCore :: Text
sharedPromptCore = sharedPromptCoreWith grammarPromptBlock

sharedPromptCoreWith :: Text -> Text
sharedPromptCoreWith searchBlock =
    T.unlines
        [ "## Working rules"
        , ""
        , "- Try, then commit: use the single `try` tool for speculative code before calling insert_cell or replace_cell."
        , "  It accepts imports and ordinary declarations, plus at most one final expression."
        , "  It can use live bindings and candidate-only dependencies."
        , "  It refuses GHCi meta-commands and compile-time escapes, and it"
        , "  type-checks unrestricted IO without running it."
        , "  Notebook cells are the durable home for owned effects; commit only"
        , "  after a useful result."
        , "- Look signatures up, don't recall them: use the search tools in"
        , "  your tool list to find real names and types. A search miss is"
        , "  weaker evidence than a compile: when they disagree, trust the"
        , "  compiler's verdict."
        , "- One small definition at a time; a write auto-runs, so read the"
        , "  result and fix any error before moving on."
        , "- Reuse what earlier cells defined (list_bindings) rather than"
        , "  recomputing."
        , "- Report back in one short sentence after each tool call, no silent"
        , "  flurries. Stop when the ask is satisfied with a one-line summary."
        , ""
        , sabelaBuiltins
        , searchBlock
        ]

sabelaBuiltins :: Text
sabelaBuiltins =
    T.unlines
        [ "## Sabela's own library (installed, nothing to add)"
        , ""
        , "- Display and interactive widgets are in scope at session start."
        , "- Drawing, charts, animation and FRP live in `Sabela.Notebook`."
        , "  Bring them into scope with: import Sabela.Notebook"
        , "  (the whole module, not a selective list)."
        , ""
        , "Find the entry points the same way you find anything else: search"
        , "for what you want to DO in your own words, browse the module, and"
        , "check_type before calling. These are an internal library, so"
        , "Hackage will not have them — your session search will."
        ]

builtinNames :: [Text]
builtinNames =
    displayBuiltins
        ++ widgetBuiltins
        ++ drawingBuiltins
        ++ ["mkWidget", "currentValue"]

drawingBuiltins :: [Text]
drawingBuiltins =
    [ "displayPicture"
    , "plot"
    , "lineChart"
    , "animate"
    , "animateWith"
    , "circle"
    , "rectangle"
    , "polyline"
    , "fill"
    , "translate"
    , "group"
    ]

displayBuiltins :: [Text]
displayBuiltins =
    [ "displayHtml"
    , "displayMarkdown"
    , "displaySvg"
    , "displayLatex"
    , "displayJson"
    , "displayImage"
    ]

widgetBuiltins :: [Text]
widgetBuiltins = ["slider", "dropdown", "checkbox", "textInput", "button"]

builtinModules :: [Text]
builtinModules =
    [ "Sabela.Notebook"
    , "Sabela.Notebook.Picture"
    , "Sabela.Notebook.Anim"
    , "Sabela.Notebook.Frp"
    ]

toolSurfaceBlock :: [ToolSpec] -> Text
toolSurfaceBlock specs =
    T.unlines (["## Tools available", ""] ++ map line specs)
  where
    line s =
        "- " <> toolWireName (toolName s) <> ": " <> firstLine (toolDescription s)
    firstLine = T.takeWhile (/= '\n') . T.strip
