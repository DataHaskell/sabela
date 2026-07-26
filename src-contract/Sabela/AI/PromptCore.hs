{-# LANGUAGE OverloadedStrings #-}

{- | The prompt substrate shared by the product chat and the eval harness: the
search-driven working rules and the generated tool-surface block. Both compose
this so the two experiences stay in step and the tool list never names a tool
the schema does not carry.
-}
module Sabela.AI.PromptCore (
    sharedPromptCore,
    sharedPromptCoreWith,
    sabelaBuiltins,
    builtinNames,
    builtinModules,
    toolSurfaceBlock,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ToolName (toolWireName)
import Sabela.AI.Grammar (grammarPromptBlock)
import Sabela.LLM.Tool (ToolSpec (..))

{- | The working rules both agents share: the live notebook is the verifier
(write, then read the compiler's verdict — R6.8). The caller passes its own
search cheat-sheet via 'sharedPromptCoreWith' so no phantom tool is named.
-}
sharedPromptCore :: Text
sharedPromptCore = sharedPromptCoreWith grammarPromptBlock

-- | 'sharedPromptCore' with the caller's own search cheat-sheet block.
sharedPromptCoreWith :: Text -> Text
sharedPromptCoreWith searchBlock =
    T.unlines
        [ "## Working rules"
        , ""
        , "- Try, then commit: use the single `try` tool for speculative code before calling insert_cell or replace_cell."
        , "  It accepts imports and ordinary declarations, plus at most one final expression."
        , "  It can use live bindings and candidate-only dependencies."
        , "  It refuses GHCi meta-commands, compile-time escapes, and unrestricted IO."
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

{- | Where Sabela's own capabilities LIVE — not what they are called. The
prompt names the modules and points at the search tools; the model finds the
functions the same way it finds anything else.

Enumerating them here was a deliberate lever, added when the session index
could not surface `Sabela.*` by keyword at all. That is fixed (see
'Test.BuiltinSearchLiveSpec': @find_function "chart"@ reaches @lineChart@,
@"Picture"@ reaches @displayPicture@), so listing the names now only teaches
the answer to the probes that grade this surface — the sine and animate
canaries were passing on prompt recall rather than on discovery.
-}
sabelaBuiltins :: Text
sabelaBuiltins =
    T.unlines
        [ "## Sabela's own library (installed, nothing to add)"
        , ""
        , "- Display and interactive widgets are in scope at session start."
        , "- Drawing, charts, animation and FRP live in `Sabela.Notebook`"
        , "  and its submodules; import it to bring them into scope."
        , ""
        , "Find the entry points the same way you find anything else: search"
        , "for what you want to DO in your own words, browse the module, and"
        , "check_type before calling. These are an internal library, so"
        , "Hackage will not have them — your session search will."
        ]

{- | The builtin value names the prompt documents, as a machine-readable seed:
discover's environment layer derives from this SAME list, so a documented
builtin can never be denied (R1.5 — one source of truth, no drift).
-}
builtinNames :: [Text]
builtinNames =
    displayBuiltins
        ++ widgetBuiltins
        ++ drawingBuiltins
        ++ ["display", "currentValue"]

{- | The drawing entry points. Named, not merely alluded to by module: the
model reaches for what the prompt names — it used displaySvg happily and
hand-rolled SVG in four consecutive live rounds while `plot` went unqueried.
-}
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

-- | The always-shipped notebook modules the prompt documents.
builtinModules :: [Text]
builtinModules =
    [ "Sabela.Notebook"
    , "Sabela.Notebook.Picture"
    , "Sabela.Notebook.Anim"
    , "Sabela.Notebook.Frp"
    ]

{- | The available-tools block, generated from the typed tool subset so the
prompt names exactly the tools the schema carries. Each line is the wire name
and the first line of the tool's description.
-}
toolSurfaceBlock :: [ToolSpec] -> Text
toolSurfaceBlock specs =
    T.unlines (["## Tools available", ""] ++ map line specs)
  where
    line s =
        "- " <> toolWireName (toolName s) <> ": " <> firstLine (toolDescription s)
    firstLine = T.takeWhile (/= '\n') . T.strip
