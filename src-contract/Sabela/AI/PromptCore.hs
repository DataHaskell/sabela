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
        , "- Propose, then observe: the live notebook is the compile check."
        , "  Write the cell and read the compiler's verdict — a red cell can"
        , "  be fixed, replaced, or deleted. scratchpad suits only snippets"
        , "  that are self-contained (it sees no notebook bindings or deps)."
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

{- | Sabela's own capabilities — the display/widget API and the drawing/animation/
FRP toolkit @import Sabela.Notebook@ brings. Always in the prompt because Hackage
search cannot surface them; everything else is discovered with the tools.
-}
sabelaBuiltins :: Text
sabelaBuiltins =
    T.unlines
        [ "## Sabela builtins (always available, nothing to install)"
        , ""
        , "- Display helpers, in scope after session start: "
            <> T.intercalate ", " displayBuiltins
            <> "."
        , "- Interactive widgets, in scope: "
            <> T.intercalate ", " widgetBuiltins
        , "  (each builds an `Input a`; display / currentValue read one)."
        , "- Drawing, animation, and FRP via `import Sabela.Notebook` (the Picture,"
        , "  Anim, and Frp modules — they ship with every notebook)."
        , ""
        , "These are an internal library. Look the exact signatures up with"
        , "check_type or your search tools before calling, and let the types"
        , "guide you — same as you would for dataframe, granite, or any package."
        ]

{- | The builtin value names the prompt documents, as a machine-readable seed:
discover's environment layer derives from this SAME list, so a documented
builtin can never be denied (R1.5 — one source of truth, no drift).
-}
builtinNames :: [Text]
builtinNames = displayBuiltins ++ widgetBuiltins ++ ["display", "currentValue"]

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
