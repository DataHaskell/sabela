{-# LANGUAGE OverloadedStrings #-}

-- | The system prompt each surface opens with.
module Siza.Agent.Loop.Prompt (
    systemPrompt,
    mcpInstructions,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.PromptCore (sabelaBuiltins)
import Siza.Agent.Tools (toolSurfacePrompt)

systemPrompt :: Text
systemPrompt = introBlock <> toolSurfacePrompt <> examplesBlock <> sabelaBuiltins

mcpInstructions :: Text
mcpInstructions = introBlock <> examplesBlock <> sabelaBuiltins

introBlock :: Text
introBlock =
    T.unlines
        [ "Pair on a live Sabela reactive Haskell notebook through tools."
        , "Editing or running a cell re-runs every cell downstream of it."
        , "insert_cell and replace_cell_source only commit code that compiles;"
            <> " a rejection carries the compiler's diagnostic so you can fix it and retry."
        , ""
        ]

examplesBlock :: Text
examplesBlock =
    T.unlines
        [ "Examples:"
        , ""
        , "* \"what is already here?\" -> list_cells, then read_cell on the one you care about"
        , "* \"which cell defines the counter?\" -> discover {query: \"counter\"}"
        , "* \"is there a priority queue?\" -> discover {query: \"priority queue\"}"
        , "* \"what is in Data.Map?\" -> discover {module: \"Data.Map\"}"
        , "* \"how do I merge two maps?\""
            <> " -> discover {query: \"Map k v -> Map k v -> Map k v\"}"
        , "* \"how do I thread state?\" -> discover {query: \"StateT\"}"
        , "* \"what is on disk?\" -> list_files, before writing any cell that reads a file"
        , "* \"what is in that file?\""
            <> " -> read_file {path: \"...\"}, so the names you write are names you have seen"
        , "* \"what arguments does mapAccumL take?\" -> check_type {expr: \"mapAccumL\"}"
        , "* \"how is foldl' implemented?\""
            <> " -> read_source {module: \"Data.List\", name: \"foldl'\"}"
        , "* \"will this compile?\" -> try {code: \"...\"}, then insert_cell once it runs"
        , "* \"is it really done?\" -> verify {check: \"...\"}, before saying so"
        , "* \"the kernel says busy\" -> await_idle"
        , ""
        ]
