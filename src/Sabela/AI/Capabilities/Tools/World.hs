{-# LANGUAGE OverloadedStrings #-}

-- | Tools that look outside the notebook: the work directory and GitHub.
module Sabela.AI.Capabilities.Tools.World (worldTools) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Sabela.AI.Capabilities.ToolName (ToolName (..), mkTool)
import Sabela.AI.ToolDoc (
    readFileDescription,
    readFilePathArg,
    readSourceDescription,
    readSourceModuleArg,
    readSourceNameArg,
    readSourcePackageArg,
    readSourceVersionArg,
 )
import Sabela.Anthropic.Types (ToolDef)

worldTools :: [ToolDef]
worldTools =
    [ mkTool
        ListFiles
        "List the data files that actually exist, so you never guess a path. With no `repo`, lists the notebook's work directory (this is the ONLY way to see what data is on disk — do it BEFORE writing any cell that reads a file, and whenever a write is refused for a missing path). With `repo` set to \"owner/name\", lists that GitHub repository's file tree instead. `path` narrows the listing to one subtree. Paths returned here are exactly the paths a cell may read."
        (fileArgs "Subtree to list, e.g. \"examples/data\". Omit for everything.")
    , mkTool
        ReadFile
        readFileDescription
        (fileArgs readFilePathArg)
    , mkTool ReadSource readSourceDescription readSourceArgs
    ]

readSourceArgs :: Value
readSourceArgs =
    object
        [ "type" .= ("object" :: Text)
        , "properties"
            .= object
                [ "module" .= stringArg readSourceModuleArg
                , "name" .= stringArg readSourceNameArg
                , "package" .= stringArg readSourcePackageArg
                , "version" .= stringArg readSourceVersionArg
                ]
        , "required" .= (["module"] :: [Text])
        ]

fileArgs :: Text -> Value
fileArgs pathDesc =
    object
        [ "type" .= ("object" :: Text)
        , "properties"
            .= object
                [ "path" .= stringArg pathDesc
                , "repo"
                    .= stringArg
                        "GitHub repository as \"owner/name\", e.g. \"haskell/containers\". Omit to use the notebook's work directory."
                , "ref"
                    .= stringArg
                        "Branch, tag, or commit to read the repository at. Omit for its default branch."
                ]
        ]

stringArg :: Text -> Value
stringArg desc = object ["type" .= ("string" :: Text), "description" .= desc]
