{-# LANGUAGE OverloadedStrings #-}

{- | The repair-tier counter log: one append-only JSONL record per speculative
repair round, so a measured null result can be read.

Without it, "the tier did not help" and "the tier never fired" look identical —
which is how an earlier hole-fit run was scored inert when its trigger simply
never matched. The counters separate the two: no candidates means a TRIGGER bug,
candidates but no winner means a SELECTOR bug, and a winner that still fails the
task means the idea is genuinely dead.

Server-side and best-effort: the eval harness sends the server's stdout to
/dev/null but keeps its work dir, so a file is the only channel that survives a
bench run. A write failure can never fail a repair.
-}
module Sabela.AI.RepairTrace (
    RepairEvent (..),
    repairEventJSON,
    repairTracePath,
    recordRepair,
) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

{- | One speculative-repair round. 'reCounts' is per candidate source, so a tier
that produced nothing is distinguishable from one whose candidates all failed the
type check. 'reWinner' names the source whose candidate was selected, if any.
-}
data RepairEvent = RepairEvent
    { reCellId :: Int
    , reCounts :: [(Text, Int)]
    , reWinner :: Maybe Text
    }
    deriving (Eq, Show)

-- | The JSONL line shape, pinned by @Test.RepairTraceWireSpec@.
repairEventJSON :: RepairEvent -> Value
repairEventJSON ev =
    object
        [ "cellId" .= reCellId ev
        , "counts" .= object [K.fromText k .= v | (k, v) <- reCounts ev]
        , "winner" .= reWinner ev
        ]

-- | @<workDir>/.sabela/repair-trace.jsonl@.
repairTracePath :: FilePath -> FilePath
repairTracePath workDir = workDir </> ".sabela" </> "repair-trace.jsonl"

{- | Append one record, creating the directory tree on demand. Best-effort: any
IO failure is swallowed, mirroring 'Sabela.AI.Provenance.recordEvent', so tracing
can never fail the repair it is measuring.
-}
recordRepair :: FilePath -> RepairEvent -> IO ()
recordRepair workDir ev = void (try go :: IO (Either SomeException ()))
  where
    path = repairTracePath workDir
    go = do
        createDirectoryIfMissing True (takeDirectory path)
        appendFile path (LBS.unpack (encode (repairEventJSON ev)) ++ "\n")
