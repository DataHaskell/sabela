{-# LANGUAGE OverloadedStrings #-}

module Sabela.Handlers.Shared (
    bumpGeneration,
    isCurrentGen,
    whenCurrentGen,
    broadcast,
    updateAndBroadcast,
    broadcastCellError,
    broadcastCellErrorWith,
    debugLog,
    isMimeLine,
    mkStreamingCallback,
    partitionExports,
    applyResult,
    insertCellAt,
    DefConflict (..),
    insertCellChecked,
    setCellSourceChecked,
    NotebookViolation (..),
    pendingError,
    checkedAppend,
    find,
) where

import Control.Monad (unless)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Api (InsertAt (..), RunResult (..))
import Sabela.Model (
    Cell (..),
    CellError (..),
    CellType (..),
    Notebook (..),
    NotebookEvent (..),
    bareCellError,
 )
import Sabela.SessionTypes (CellLang (..))
import Sabela.State (App (..))
import qualified Sabela.State.EventBus as EB
import Sabela.State.NotebookStore (modifyNotebookIO)
import Sabela.Topo (buildDefMap, cellNames)

bumpGeneration :: App -> IO Int
bumpGeneration = EB.bumpGeneration . appEvents

broadcast :: App -> NotebookEvent -> IO ()
broadcast = EB.broadcast . appEvents

updateAndBroadcast :: App -> (Notebook -> Notebook) -> NotebookEvent -> IO ()
updateAndBroadcast app f ev = modifyNotebookIO (appNotebook app) $ \nb -> do
    let nb' = f nb
    broadcast app ev
    pure nb'

debugLog :: App -> Text -> IO ()
debugLog app = EB.debugLog (appEnv app)

isCurrentGen :: App -> Int -> IO Bool
isCurrentGen = EB.isCurrentGen . appEvents

whenCurrentGen :: App -> Int -> IO () -> IO ()
whenCurrentGen = EB.whenCurrentGen . appEvents

{- | True for either the new @<!-- MIME:... -->@ or legacy
@---MIME:...---@ marker. Such lines are buffered, not broadcast as
partial output, so the block body stays whole.
-}
isMimeLine :: Text -> Bool
isMimeLine line =
    "<!-- MIME:" `T.isPrefixOf` line
        || "---MIME:" `T.isPrefixOf` line

mkStreamingCallback :: App -> Int -> IO (Text -> IO ())
mkStreamingCallback app cid = do
    inMimeBlock <- newIORef False
    pure $ \line ->
        if isMimeLine line
            then writeIORef inMimeBlock ("---MIME:text/plain---" /= line)
            else do
                isMime <- readIORef inMimeBlock
                unless isMime $
                    broadcast app (EvCellPartialOutput cid line)

partitionExports :: [(Text, Text)] -> ([(Text, Text)], [(Text, Text)])
partitionExports = foldr classify ([], [])
  where
    classify (mime, body) (exps, normals) =
        case T.stripPrefix "EXPORT:" mime of
            Just name -> ((name, body) : exps, normals)
            Nothing -> (exps, (mime, body) : normals)

applyResult :: RunResult -> Cell -> Cell
applyResult r c
    | cellId c == rrCellId r =
        c
            { cellOutputs = rrOutputs r
            , cellError = rrError r
            , cellDirty = False
            }
    | otherwise = c

broadcastCellError :: App -> Int -> Text -> IO ()
broadcastCellError app cid msg =
    broadcastCellErrorWith app cid msg [bareCellError Nothing Nothing msg]

-- | Like 'broadcastCellError', with explicit (line-carrying) 'CellError's.
broadcastCellErrorWith :: App -> Int -> Text -> [CellError] -> IO ()
broadcastCellErrorWith app cid msg errs =
    updateAndBroadcast
        app
        (\nb -> nb{nbCells = map (clearCellOutputs cid msg) (nbCells nb)})
        (EvCellResult cid [] (Just msg) errs [])

clearCellOutputs :: Int -> Text -> Cell -> Cell
clearCellOutputs targetCid errMsg c
    | cellId c == targetCid =
        c{cellOutputs = [], cellError = Just errMsg, cellDirty = False}
    | otherwise = c

{- | Insert @c@ at the position described by 'InsertAt'. 'AtBeginning'
prepends; 'After' inserts after the matching cell, or appends if no such
cell exists. Shared by 'insertCellH' (REST) and 'execInsertCell' (AI
tool) so both call sites stay in sync.
-}
insertCellAt :: InsertAt -> Cell -> [Cell] -> [Cell]
insertCellAt AtBeginning c cs = c : cs
insertCellAt (After _) c [] = [c]
insertCellAt (After aid) c (x : xs)
    | cellId x == aid = x : c : xs
    | otherwise = x : insertCellAt (After aid) c xs

{- | A rejected mutation: top-level binding @dcName@ is already defined by the
cell with id @dcOwnerId@. The checked mutations return this instead of
committing a duplicate; the caller turns it into actionable guidance.
-}
data DefConflict = DefConflict
    { dcName :: Text
    , dcOwnerId :: Int
    }
    deriving (Eq, Show)

-- | Only Haskell code cells contribute top-level bindings.
isHaskellCode :: Cell -> Bool
isHaskellCode c = cellType c == CodeCell && cellLang c == Haskell

{- | The first binding @candidate@ defines that another Haskell code cell
already owns, if any. Order-independent: the conflict is reported against the
owning cell whether @candidate@ sits before or after it.
-}
defConflict :: Notebook -> Cell -> Maybe DefConflict
defConflict nb candidate
    | not (isHaskellCode candidate) = Nothing
    | otherwise =
        let others =
                [c | c <- nbCells nb, cellId c /= cellId candidate, isHaskellCode c]
            (defMap, _) = buildDefMap others
            (defs, _) = cellNames (cellSource candidate)
         in listToMaybe
                [ DefConflict n owner
                | n <- S.toAscList defs
                , Just owner <- [M.lookup n defMap]
                ]

{- | Insert @cell@ at @at@, refusing if it would define a binding another cell
already owns. The success branch is the only one that yields a 'Notebook', so a
duplicate cannot be committed through this path.
-}
insertCellChecked :: InsertAt -> Cell -> Notebook -> Either DefConflict Notebook
insertCellChecked at cell nb =
    case defConflict nb cell of
        Just conflict -> Left conflict
        Nothing -> Right nb{nbCells = insertCellAt at cell (nbCells nb)}

{- | Replace @oldCell@'s source with @newSrc@ (marking it dirty and clearing its
stale outputs/error), refusing if the new source would redefine a binding
another cell owns. Editing the cell's own definitions never conflicts.
-}
setCellSourceChecked ::
    Cell -> Text -> Notebook -> Either DefConflict (Notebook, Cell)
setCellSourceChecked oldCell newSrc nb =
    case defConflict nb candidate of
        Just conflict -> Left conflict
        Nothing -> Right (nb{nbCells = map upd (nbCells nb)}, candidate)
  where
    candidate =
        oldCell
            { cellSource = newSrc
            , cellDirty = True
            , cellOutputs = []
            , cellError = Nothing
            }
    upd c = if cellId c == cellId oldCell then candidate else c

{- | A rejected AI insert: either the candidate duplicates a definition
('VDuplicateDef'), or the notebook already has a cell with a settled error
('VPendingError') — a new cell cannot be started atop a broken one; fix or delete
it first. The checked insert returns this instead of committing.
-}
data NotebookViolation
    = VDuplicateDef DefConflict
    | VPendingError Int Text
    deriving (Eq, Show)

{- | The first Haskell code cell carrying a settled error, if any. The error-gate
reads this so a new cell cannot be appended while the notebook is broken.
-}
pendingError :: Notebook -> Maybe (Int, Text)
pendingError nb =
    listToMaybe
        [ (cellId c, msg)
        | c <- nbCells nb
        , isHaskellCode c
        , Just msg <- [cellError c]
        ]

{- | The AI insert contract: append @cell@ at the end, refusing if the notebook
has a cell with a settled error or @cell@ would duplicate a binding another cell
owns. Append (not insert-at-position) because execution order is dependency-driven
— cell position is cosmetic. A pending error is reported before a duplicate:
resolve the broken cell first.
-}
checkedAppend :: Cell -> Notebook -> Either NotebookViolation Notebook
checkedAppend cell nb =
    case pendingError nb of
        Just (cid, msg) -> Left (VPendingError cid msg)
        Nothing ->
            case defConflict nb cell of
                Just conflict -> Left (VDuplicateDef conflict)
                Nothing -> Right nb{nbCells = nbCells nb ++ [cell]}
