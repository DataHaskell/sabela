{- | One tool call, all the way through: dispatch, unblock, display-contract
repair, ownership, and the repair cascade over the cells the session owns.

Everything here needs a 'Dispatch' and nothing else, which is what lets the
MCP server run the same pipeline the chat loop runs.
-}
module Siza.Agent.Stack.Call (
    StackNote (..),
    CallResult (..),
    runToolCall,
    healOwnedReds,
    foldDiscovery,
    foldNotes,
    notesMessage,
    noteToolName,
    noteCharBudget,
) where

import Control.Monad (filterM)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.CellResult (CellId)
import Sabela.AI.Types (ToolOutcome)
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover (runDiscoverOutcomes)
import Siza.Agent.Loop.Route (blockingCell, discloseRoute, routedRetryNote)
import Siza.Agent.Loop.Support (replaceCall, writeSource)
import Siza.Agent.Owned (ocDiagnostic, ownedCellOutcome)
import Siza.Agent.RenderContract (repairDisplayContract)
import Siza.Agent.Repair (repairRedCells)
import Siza.Agent.Repair.Blocking (repairBlockingCell)
import Siza.Agent.Stack (
    Dispatch,
    StackSession (..),
    SurfacePolicy (..),
    freshNote,
    ownedCells,
    ownedReds,
    recordCall,
    sessionGoal,
    sessionPolicy,
    ssHealReds,
 )

-- | Something the pipeline did that the caller did not ask for, and would be
-- wrong to discover by reading the notebook later.
data StackNote = StackNote
    { snKind :: !Text
    , snText :: !Text
    }
    deriving (Eq, Show)

data CallResult = CallResult
    { crCall :: ToolCall
    , crOutcome :: Either Text ToolOutcome
    , crNotes :: [StackNote]
    }

{- | Dispatch a call and carry it through the repair layers. @disp@ must
already be wrapped in 'Siza.Agent.Stack.stackDispatch'.
-}
runToolCall :: StackSession -> Dispatch -> ToolCall -> IO CallResult
runToolCall ss disp call = do
    outcome <- disp call
    (call', out', routeNotes) <- unblock ss disp call outcome
    (call'', out'') <- surfaceDisplay ss disp call' out'
    recordCall ss (call'', out'')
    healNotes <- healAfter ss disp call'' out''
    discNotes <- discoverAfter ss disp call'' out''
    pure (CallResult call'' out'' (routeNotes ++ healNotes ++ discNotes))

{- | The discovery a call implicates, for a surface with no later turn to
inject it in. The chat loop has one, and injects it as messages instead.
-}
discoverAfter ::
    StackSession ->
    Dispatch ->
    ToolCall ->
    Either Text ToolOutcome ->
    IO [StackNote]
discoverAfter ss disp call outcome
    | not (spFoldsDiscovery (sessionPolicy ss)) = pure []
    | Right o <- outcome = foldDiscovery ss disp [(call, o)]
    | otherwise = pure []

{- | A write blocked by another red cell is not a failure of the write: repair
the blocker, or re-aim the same source at it.
-}
unblock ::
    StackSession ->
    Dispatch ->
    ToolCall ->
    Either Text ToolOutcome ->
    IO (ToolCall, Either Text ToolOutcome, [StackNote])
unblock _ disp call outcome = case blockingCell outcome of
    Just n -> do
        healed <- repairBlockingCell disp n
        case healed of
            Just (c, o) -> pure (c, o, [])
            Nothing
                | tcName call == "insert_cell"
                , Just src <- writeSource call -> do
                    let rc = replaceCall n src
                    o2 <- disp rc
                    pure
                        ( rc
                        , fmap (discloseRoute n) o2
                        , [StackNote "routed_retry" (routedRetryNote n)]
                        )
            _ -> pure (call, outcome, [])
    _ -> pure (call, outcome, [])

surfaceDisplay ::
    StackSession ->
    Dispatch ->
    ToolCall ->
    Either Text ToolOutcome ->
    IO (ToolCall, Either Text ToolOutcome)
surfaceDisplay ss disp call outcome = do
    goal <- sessionGoal ss
    repaired <- repairDisplayContract goal disp call outcome
    pure $ case repaired of
        Just (c, o) -> (c, o)
        Nothing -> (call, outcome)

{- | Repair the session's red cells after a call that left its own cell red.
Cells the session never wrote are never touched.
-}
healAfter ::
    StackSession ->
    Dispatch ->
    ToolCall ->
    Either Text ToolOutcome ->
    IO [StackNote]
healAfter ss disp call outcome
    | not (ssHealReds ss) = pure []
    | Just (_, False) <- ownedCellOutcome call outcome = do
        reds <- map fst <$> ownedReds ss
        fixes <- healOwnedReds ss disp reds
        pure [StackNote "repair" (repairNote (length fixes)) | not (null fixes)]
    | otherwise = pure []

repairNote :: Int -> Text
repairNote n =
    "Repaired "
        <> T.pack (show n)
        <> " red cell(s) this session wrote. Read them back before building on them."

{- | Run the repair cascade over the named cells, keeping the owned map in
step with whatever it changed.
-}
healOwnedReds ::
    StackSession -> Dispatch -> [CellId] -> IO [(ToolCall, Either Text ToolOutcome)]
healOwnedReds ss disp reds = do
    owned <- ownedCells ss
    let targets =
            [ (cid, ocDiagnostic oc)
            | cid <- reds
            , Just oc <- [lookupOwned cid owned]
            ]
    fixes <- repairRedCells disp targets
    mapM_ (recordCall ss) fixes
    pure fixes
  where
    lookupOwned = Map.lookup

-- | Discovery for a finished batch of calls, as notes rather than messages.
foldDiscovery ::
    StackSession -> Dispatch -> [(ToolCall, ToolOutcome)] -> IO [StackNote]
foldDiscovery ss disp steps = do
    vals <- runDiscoverOutcomes (ssGrammar ss) disp steps
    fresh <- filterM (freshNote ss) (concatMap discoverText vals)
    pure [StackNote "discover" t | t <- fresh]

-- | The prose of a chat-shaped injection, so it can travel as a note instead.
discoverText :: Value -> [Text]
discoverText (Object o) = case KM.lookup "content" o of
    Just (String t) | not (T.null (T.strip t)) -> [T.strip t]
    _ -> []
discoverText _ = []

{- | Fold notes into one block under a character budget, most load-bearing
first. What does not fit is dropped, and the block says so.
-}
foldNotes :: Int -> [StackNote] -> Maybe Text
foldNotes budget notes
    | null kept = Nothing
    | otherwise = Just (T.intercalate "\n\n" (map snText kept) <> omitted)
  where
    ranked = sortOn (notePriority . snKind) notes
    kept = takeUnder budget ranked
    dropped = length ranked - length kept
    omitted
        | dropped <= 0 = ""
        | otherwise =
            "\n\n[" <> T.pack (show dropped) <> " further note(s) omitted for length]"

takeUnder :: Int -> [StackNote] -> [StackNote]
takeUnder _ [] = []
takeUnder left (n : ns)
    | cost > left = []
    | otherwise = n : takeUnder (left - cost) ns
  where
    cost = T.length (snText n)

{- | The notes a finished batch of calls produced, as one message. The MCP
server folds the same notes into a text block; this is the chat surface's
delivery of that fold, so neither surface drops what the shared stack computed.
-}
notesMessage :: [CallResult] -> Maybe Value
notesMessage results = msg <$> foldNotes noteCharBudget (concatMap crNotes results)
  where
    msg t =
        object
            [ "role" .= ("tool" :: Text)
            , "tool_name" .= noteToolName
            , "content" .= t
            ]

noteToolName :: Text
noteToolName = "stack_note"

notePriority :: Text -> Int
notePriority "routed_retry" = 0
notePriority "repair" = 1
notePriority "red_cells" = 2
notePriority _ = 3

noteCharBudget :: Int
noteCharBudget = 2500
