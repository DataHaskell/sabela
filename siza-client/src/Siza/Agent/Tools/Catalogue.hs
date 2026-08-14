module Siza.Agent.Tools.Catalogue (baseCatalogue) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import Data.Text (Text)
import Sabela.AI.ToolDoc (
    checkTypeImportsDoc,
    readFileDescription,
    readFilePathArg,
    readSourceDescription,
    readSourceModuleArg,
    readSourceNameArg,
    readSourcePackageArg,
    readSourceVersionArg,
    tryDescription,
 )
import Siza.Agent.Discover.Request (requestProperties, requestRequired)
import Siza.Agent.DiscoverTool (discoverToolDescription)
import Siza.Agent.VerifyTool (
    verifyDescription,
    verifyProperties,
    verifyRequired,
    verifyToolName,
 )

fn :: Text -> Text -> Value -> Value
fn name desc params =
    object
        [ "type" .= ("function" :: Text)
        , "function"
            .= object
                ["name" .= name, "description" .= desc, "parameters" .= params]
        ]

prop :: Text -> Value
prop d = object ["type" .= ("string" :: Text), "description" .= d]

intProp :: Text -> Value
intProp d = object ["type" .= ("integer" :: Text), "description" .= d]

boolProp :: Text -> Value
boolProp d = object ["type" .= ("boolean" :: Text), "description" .= d]

listProp :: Text -> Value
listProp d =
    object
        [ "type" .= ("array" :: Text)
        , "items" .= object ["type" .= ("string" :: Text)]
        , "description" .= d
        ]

{- | Optional on every write: what the cell is FOR. Remembered for the rest of
the session and used to decide whether a result needs displaying.
-}
goalProp :: Value
goalProp =
    prop
        "Optional. What this cell is for, in one line (\"show a chart of the \
        \residuals\"). Remembered for the session."

props :: [(Text, Value)] -> [Text] -> Value
props ps required =
    object
        [ "type" .= ("object" :: Text)
        , "properties" .= object [(K.fromText k, v) | (k, v) <- ps]
        , "required" .= required
        ]

baseCatalogue :: [Value]
baseCatalogue =
    [ fn
        "list_cells"
        "Map of EVERY cell in the notebook (the whole notebook in one call): each cell's id, position, type, language, the bindings it `defines`, and whether it errored. By default each cell shows only its first line; pass `full: true` to include each cell's source. To find which cell defines or uses a name, scan `defines` here or search for it with discover, which reports notebook cells alongside library results."
        ( props
            [
                ( "full"
                , boolProp "Include each cell's source (default false: first-line preview)."
                )
            ]
            []
        )
    , fn
        "discover"
        discoverToolDescription
        (props requestProperties requestRequired)
    , fn
        "read_cell"
        "Read ONE cell's full SOURCE and error by id. Its outputs (often large rendered HTML/SVG) are omitted by default — a `hasOutputs` flag signals them; pass `full: true` to include outputs."
        ( props
            [ ("cell_id", intProp "Cell id from list_cells.")
            , ("full", boolProp "Include the cell's outputs (default false).")
            ]
            ["cell_id"]
        )
    , fn
        "insert_cell"
        "Append a new Haskell cell and run it. Put the cell's full Haskell source in the `source` argument. Use this to add code."
        ( props
            [ ("source", prop "The full Haskell source for the new cell.")
            , ("goal", goalProp)
            ]
            ["source"]
        )
    , fn
        "replace_cell_source"
        "Replace a cell's entire source and re-run it. Pass the cell_id and the replacement Haskell source. Use to fix an existing cell."
        ( props
            [ ("cell_id", intProp "Cell to replace.")
            , ("new_source", prop "The replacement Haskell source.")
            , ("goal", goalProp)
            ]
            ["cell_id", "new_source"]
        )
    , fn
        "execute_cell"
        "Run one cell by id; returns its outputs and any errors."
        (props [("cell_id", intProp "Cell to run.")] ["cell_id"])
    , fn
        "list_bindings"
        "List every value, function, and type already defined in the notebook session, with its type. Use BEFORE building on earlier work, to reuse an existing binding instead of recomputing it."
        (props [] [])
    , fn
        "check_type"
        "Look a NAME up in the local Haskell index, or type-check an EXPRESSION. A bare or qualified name (\"nub\", \"Q.insert\", \"Maybe\") is answered from the index with no session needed: where it is defined, its package, its signature, and separately what this notebook shows about it. An expression is answered by the compiler. To find a name you cannot spell, use discover."
        ( props
            [ ("expr", prop "An expression, value name, or type/class name.")
            , ("imports", listProp checkTypeImportsDoc)
            ]
            ["expr"]
        )
    , fn
        "list_files"
        "List the data files that actually exist, so you never guess a path. With no `repo`, lists the notebook's work directory — this is the ONLY way to see what data is on disk, so call it BEFORE writing any cell that reads a file, and whenever a write is refused for a missing path. With `repo` set to \"owner/name\", lists that GitHub repository's file tree instead. Paths returned here are exactly the paths a cell may read."
        ( props
            [ ("path", prop "Subtree to list, e.g. \"examples/data\". Omit for everything.")
            ,
                ( "repo"
                , prop "GitHub repository as \"owner/name\". Omit for the work directory."
                )
            , ("ref", prop "Branch, tag, or commit. Omit for the default branch.")
            ]
            []
        )
    , fn
        "read_file"
        readFileDescription
        ( props
            [ ("path", prop readFilePathArg)
            ,
                ( "repo"
                , prop "GitHub repository as \"owner/name\". Omit for the work directory."
                )
            , ("ref", prop "Branch, tag, or commit. Omit for the default branch.")
            ]
            ["path"]
        )
    , fn
        "read_source"
        readSourceDescription
        ( props
            [ ("module", prop readSourceModuleArg)
            , ("name", prop readSourceNameArg)
            , ("package", prop readSourcePackageArg)
            , ("version", prop readSourceVersionArg)
            ]
            ["module"]
        )
    , fn
        "try"
        tryDescription
        ( props
            [ ("code", prop "The code to try.")
            ,
                ( "language"
                , object
                    [ "type" .= ("string" :: Text)
                    , "enum" .= (["Haskell", "Python"] :: [Text])
                    , "description" .= ("Language. Default: Haskell." :: Text)
                    ]
                )
            ]
            ["code"]
        )
    , fn
        verifyToolName
        verifyDescription
        (props verifyProperties verifyRequired)
    , fn
        "delete_cell"
        "Delete a cell from the notebook. Use this to remove a cell you cannot fix in place — e.g. a failing cell that is blocking you from inserting a new one."
        (props [("cell_id", intProp "The id of the cell to delete.")] ["cell_id"])
    , fn
        "kernel_status"
        "Lock-free kernel status. Always answers, even while a cell holds the run-lock, so you can tell \"busy\" (a slow cell) from \"wedged\" (unresponsive) without blocking. Returns a typed `state` tag (cold | idle | executing | building) plus `buildingMs` while compiling, so a long-but-progressing build reads differently from a wedge."
        (props [] [])
    , fn
        "await_idle"
        "Block until the running cell or build finishes (a bounded ~45s long-poll), then return the fresh kernel status. `waited` is one of: idle | settled | timedOut | kernelDead. Call this when a tool says the kernel is busy — do NOT re-run the cell. Re-call while it reports timedOut (the kernel is still working); if the reply carries a `resource` line the cell looks non-terminating — interrupt, shrink the work, rewrite. kernelDead means the kernel died: kernel_restart."
        (props [] [])
    , fn
        "interrupt"
        "Abort the cell the Haskell kernel is currently running (group SIGINT). No-op when the kernel is idle. Use this when a cell is stuck before reaching for kernel_restart."
        (props [] [])
    , fn
        "kernel_restart"
        "Hard-reset the Haskell kernel: force-kill the kernel process (even a wedged one that ignores interrupt) and respawn it clean — reusing the installed packages without rebuilding, and WITHOUT re-running any cells. Returns immediately; poll kernel_status until idle. This is how you recover a stuck or wedged kernel."
        (props [] [])
    ]
