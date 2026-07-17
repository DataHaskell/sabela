{-# LANGUAGE OverloadedStrings #-}

{- | The prompt substrate shared by the product chat and the eval harness: the
search-driven working rules and the generated tool-surface block. Both compose
this so the two experiences stay in step and the tool list never names a tool
the schema does not carry.
-}
module Sabela.AI.PromptCore (
    sharedPromptCore,
    sabelaBuiltins,
    toolSurfaceBlock,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ToolName (toolWireName)
import Sabela.AI.Grammar (grammarPromptBlock)
import Sabela.LLM.Tool (ToolSpec (..))

{- | The working rules both agents share: compile-check before committing, look
signatures up with the search tools rather than recalling them, edit in small
auto-running steps, reuse bindings, narrate briefly, and stop when done.
-}
sharedPromptCore :: Text
sharedPromptCore =
    T.unlines
        [ "## Working rules"
        , ""
        , "- Compiler first: before inserting or editing a cell, compile-check it"
        , "  with a query tool or the scratchpad. Shrink on failure, don't guess."
        , "- Look signatures up, don't recall them: use the search tools"
        , "  (find_function, describe_function, find_by_type, api_reference,"
        , "  search_capability) to find real names and types."
        , "- One small definition at a time; a write auto-runs, so read the"
        , "  result and fix any error before moving on."
        , "- Reuse what earlier cells defined (list_bindings) rather than"
        , "  recomputing. scratchpad runs an isolated snippet that cannot see"
        , "  notebook bindings."
        , "- Report back in one short sentence after each tool call, no silent"
        , "  flurries. Stop when the ask is satisfied with a one-line summary."
        , ""
        , sabelaBuiltins
        , grammarPromptBlock
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
        , "- Display helpers, in scope after session start: displayHtml,"
        , "  displayMarkdown, displaySvg, displayLatex, displayJson, displayImage."
        , "- Interactive widgets, in scope: slider, dropdown, checkbox, textInput,"
        , "  button (each builds an `Input a`; display / currentValue read one)."
        , "- Drawing, animation, and FRP via `import Sabela.Notebook` (the Picture,"
        , "  Anim, and Frp modules — they ship with every notebook)."
        , ""
        , "These are an internal library. Look the exact signatures up with"
        , "describe_function / api_reference before calling, and let the types"
        , "guide you — same as you would for dataframe, granite, or any package."
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
