{-# LANGUAGE OverloadedStrings #-}

{- | Synthetic episodes with known ground truth, for the retro round trip.
Each beat below renders the messages one harness move produces and declares
what that move is worth, so the expected metrics are a fold over the plan.
-}
module Test.RetroEpisodeGen (
    commit,
    deleteOf,
    genEpisode,
    genRepeatBeat,
    identifiers,
    rejectRun,
    renameIdentifiers,
    reroutedCommit,
    verdictOk,
) where

import Data.Aeson (object, toJSON, (.=))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

import Sabela.AI.Verdict (VerdictClass (VerdictOk), verdictMarker)
import Siza.Agent.Compact (resultStubFor)
import Test.RetroBeat (
    Beat (..),
    Truth (..),
    assistantMsg,
    toolMsg,
 )

-- | A user prompt: prompt characters and nothing else.
promptBeat :: Text -> Beat
promptBeat t =
    Beat
        [object ["role" .= ("user" :: Text), "content" .= t]]
        mempty{trPrompt = T.length t}

-- | An assistant turn that says something and calls nothing.
proseBeat :: Text -> Text -> Beat
proseBeat think content =
    Beat
        [assistantMsg think content []]
        mempty{trTurns = 1, trThinking = T.length think}

{- | A read-only call and its result, optionally elided to a stub.
The stub is built with the harness's own 'resultStubFor', so the parser is
pinned to the real elision format rather than to a copy of it.
-}
queryBeat :: Text -> Text -> Text -> Text -> Bool -> Bool -> Beat
queryBeat tool think args result dup elide =
    Beat
        [ assistantMsg think "" [(tool, toJSON args)]
        , toolMsg tool shown
        ]
        mempty
            { trTurns = 1
            , trThinking = T.length think
            , trCalls = M.singleton tool 1
            , trDuplicates = if dup then 1 else 0
            , trElided = if elide then 1 else 0
            , trElidedFull = if elide then T.length full else 0
            , trPayload = M.singleton tool (T.length shown)
            }
  where
    full =
        if dup
            then
                "{\"ref\":\"miss 4\",\"state\":\"duplicate\",\"summary\":\"" <> result <> "\"}"
            else "{\"summary\":\"" <> result <> "\"}"
    shown = if elide then resultStubFor 1 tool full else full

-- | An accepted write: the ack the harness returns for a committed cell.
commitBeat :: Text -> Int -> Text -> Beat
commitBeat tool cellId source =
    Beat
        [ assistantMsg "" "" [(tool, object ["source" .= source])]
        , toolMsg tool ack
        ]
        mempty
            { trTurns = 1
            , trCalls = M.singleton tool 1
            , trAttempted = 1
            , trCommitted = 1
            , trPayload = M.singleton tool (T.length ack)
            }
  where
    ack =
        "{\"cellId\":"
            <> T.pack (show cellId)
            <> ",\"hash\":\"p1\",\"status\":\"completed\"}"

commit :: Int -> Beat
commit cellId = commitBeat "insert_cell" cellId "x = 1"

-- | A delete that the harness carried out, retiring a committed cell.
deleteOf :: Int -> Beat
deleteOf cellId =
    Beat
        [ assistantMsg "" "" [("delete_cell", object ["cell_id" .= cellId])]
        , toolMsg "delete_cell" ack
        ]
        mempty
            { trTurns = 1
            , trCalls = M.singleton "delete_cell" 1
            , trAttempted = 1
            , trCommitted = 1
            , trPayload = M.singleton "delete_cell" (T.length ack)
            }
  where
    ack = "{\"cellId\":" <> T.pack (show cellId) <> ",\"deleted\":true}"

{- | A write the harness rerouted: the ack comes back under a tool name the
model never called.
-}
reroutedCommit :: Int -> Beat
reroutedCommit cellId =
    Beat
        [ assistantMsg "" "" [("insert_cell", object ["source" .= ("x = 1" :: Text)])]
        , toolMsg "replace_cell_source" ack
        ]
        mempty
            { trTurns = 1
            , trCalls = M.singleton "insert_cell" 1
            , trAttempted = 1
            , trCommitted = 1
            , trPayload = M.singleton "replace_cell_source" (T.length ack)
            }
  where
    ack = "{\"cellId\":" <> T.pack (show cellId) <> ",\"status\":\"completed\"}"

-- | The harness's done signal, which answers no call of the model's.
verdictOk :: Beat
verdictOk =
    Beat
        [toolMsg "verify" content]
        mempty{trPayload = M.singleton "verify" (T.length content)}
  where
    content = verdictMarker VerdictOk <> " Deliverable confirmed."

{- | A run of rejected submissions sharing one diagnostic.
The sources are submitted in order; a run of length n scores n-1 repeats
where consecutive sources are whitespace-equal, and n-1 unchanged
diagnostics where they are not.
-}
rejectRunWith :: Bool -> Text -> Text -> [Text] -> Beat
rejectRunWith elide tool diagnostic sources =
    Beat
        (concatMap submission sources)
        mempty
            { trTurns = n
            , trCalls = M.singleton tool n
            , trAttempted = n
            , trRejected = n
            , trRepeats = if elide then 0 else same
            , trUnchangedDiags = if elide then 0 else (n - 1) - same
            , trUnknownDiags = if elide then n else 0
            , trElided = if elide then n else 0
            , trElidedFull = if elide then n * T.length payload else 0
            , trPayload = M.singleton tool (n * T.length shown)
            }
  where
    n = length sources
    same = length [() | (a, b) <- zip sources (drop 1 sources), squash a == squash b]
    squash = T.unwords . T.words
    payload =
        "CODE ISSUE: {\"diagnostic\":\""
            <> diagnostic
            <> "\",\"notCommitted\":\"compile-gate\",\"verdict\":\"diagnostic\"}"
    shown = if elide then resultStubFor 1 tool payload else payload
    submission src =
        [ assistantMsg "" "" [(tool, object ["source" .= src])]
        , toolMsg tool shown
        ]

rejectRun :: Text -> Text -> [Text] -> Beat
rejectRun = rejectRunWith False

{- | Identifiers the episodes are written in: several packages plus invented
names, so no metric can depend on the library the baseline episode used.
-}
identifiers :: [Text]
identifiers =
    [ "insertWith"
    , "Data.Map.alter"
    , "decodeStrict"
    , "Data.Aeson.encode"
    , "splitOn"
    , "Data.Text.replace"
    , "Data.Set.union"
    , "frobnicate"
    , "wibbleCount"
    , "quuxify"
    , "Containers.Zed.merge"
    , "unfoldrM"
    ]

-- | Read-only tool names, disjoint from every notebook-write tool name.
queryTools :: [Text]
queryTools =
    ["discover", "check_type", "list_cells", "wobble_tool", "find_thing"]

writeTools :: [Text]
writeTools = ["insert_cell", "replace_cell_source", "execute_cell"]

genProse :: Gen Text
genProse = T.unwords <$> listOf1 (elements identifiers)

genSource :: Gen Text
genSource = T.unwords <$> vectorOf 3 (elements identifiers)

genBeat :: Gen Beat
genBeat =
    oneof
        [ promptBeat <$> genProse
        , proseBeat <$> genProse <*> genProse
        , queryBeat
            <$> elements queryTools
            <*> genProse
            <*> genProse
            <*> genProse
            <*> arbitrary
            <*> arbitrary
        , commitBeat <$> elements writeTools <*> choose (1, 40) <*> genSource
        , reroutedCommit <$> choose (1, 40)
        , genRepeatBeat
        ]

{- | A run of rejections whose diagnostic is unique to the run, so two runs
never merge across a beat boundary and each run scores only its own.
-}
genRepeatBeat :: Gen Beat
genRepeatBeat = do
    tool <- elements writeTools
    tag <- elements identifiers
    salt <- choose (1, 10 ^ (12 :: Int) :: Integer)
    base <- genSource
    n <- choose (2, 4)
    sources <- vectorOf n (variantOf base)
    elide <- arbitrary
    pure (rejectRunWith elide tool (diagnosticOf tag salt) sources)
  where
    {- Long enough that a stub preview always cuts it off, so an elided run
    is unreadable by construction and salted so two runs never match. -}
    diagnosticOf tag salt =
        "error in " <> tag <> " #" <> T.pack (show salt) <> T.replicate 80 "x"
    variantOf base =
        oneof
            [ pure base
            , pure (base <> "  ")
            , pure (T.replace " " "\n  " base)
            , (\x -> base <> " " <> x) <$> elements identifiers
            ]

{- | An episode always opens with a prompt, so promptChars is never zero and
the first assistant turn has something to answer.
-}
genEpisode :: Gen [Beat]
genEpisode = do
    opening <- promptBeat <$> genProse
    rest <- listOf genBeat
    pure (opening : rest)

{- | Renames every identifier to a same-length stand-in, everywhere it occurs.
Length preservation keeps the character budgets comparable, so every metric
must come out unchanged.
-}
renameIdentifiers :: Text -> Text
renameIdentifiers t = foldl' step t (zip identifiers (map disguise identifiers))
  where
    step acc (from, to) = T.replace from to acc
    disguise = T.map rot
    rot c
        | c `elem` ("abcdefghijklmnopqrstuvwxy" :: String) = succ c
        | c == 'z' = 'a'
        | c `elem` ("ABCDEFGHIJKLMNOPQRSTUVWXY" :: String) = succ c
        | c == 'Z' = 'A'
        | otherwise = c
