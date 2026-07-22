module Siza.Agent.Discover (
    GrammarMode (..),
    Dispatch,
    declaredPackages,
    declaresDepsCall,
    discoverModules,
    browseCall,
    browseText,
    discoverGrammarMsg,
    executionSucceeded,
    runDiscover,
    runDiscoverOutcomes,
    discoverSurfaces,
    proactiveDiscover,
    rediscoverModules,
    seamDiscover,
    toolCallSource,
    isOwningTool,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Discover (importedModules, rediscoverModules)
import Sabela.AI.Grammar (BrowseEntry (..), ImportStyle (..), parseBrowse)
import Sabela.AI.Grammar.Card (emittableCard)
import Sabela.AI.Grammar.Synth (
    Surface (..),
    synthesizeGrammarBounded,
    usedNames,
 )
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Discover.Envelope (envelopeCharBudget, envelopeChars)

import Sabela.LLM.Ollama.Client (ToolCall (..))

data GrammarMode = GrammarOn | GrammarOff
    deriving (Eq, Show)

type Dispatch = ToolCall -> IO (Either Text ToolOutcome)

isOwningTool :: Text -> Bool
isOwningTool n = n == "insert_cell" || n == "replace_cell_source"

runDiscover :: GrammarMode -> Dispatch -> [ToolCall] -> IO [Value]
runDiscover mode dispatch calls =
    discoverSurfaces mode dispatch proven (concatMap discoverModules calls)
  where
    proven = concatMap (usedNames . toolCallSource) calls

{- | R6.10: module-API bytes attach ONLY to an install write that FAILED with
a module-implicating diagnostic ('rediscoverModules' keys the class) — never
to any executionSucceeded result, never to a transport error.
-}
runDiscoverOutcomes ::
    GrammarMode -> Dispatch -> [(ToolCall, ToolOutcome)] -> IO [Value]
runDiscoverOutcomes mode dispatch pairs =
    discoverSurfaces mode dispatch proven (nub (concatMap targets implicated))
  where
    implicated = [p | p@(c, o) <- pairs, redInstall c o]
    proven = concatMap (usedNames . toolCallSource . fst) implicated
    targets (c, o) =
        rediscoverModules (toolCallSource c) (outcomeDiagnostic o)
    redInstall c o =
        isOwningTool (tcName c)
            && declaresDepsCall c
            && executionFailed o
            && not (null (targets (c, o)))

-- | Did the call's cell actually run clean (payload execution report)?
executionSucceeded :: ToolOutcome -> Bool
executionSucceeded (ToolOk (Object o)) = case KM.lookup "execution" o of
    Just (Object e) -> okField e
    _ -> okField o
  where
    okField m = case KM.lookup "ok" m of
        Just (Bool b) -> b
        _ -> False
executionSucceeded _ = False

{- | The cell LANDED but ran red: an execution report with @ok: false@. A
transport error or a report-free ack landed nothing browsable.
-}
executionFailed :: ToolOutcome -> Bool
executionFailed (ToolOk (Object o)) = case KM.lookup "execution" o of
    Just (Object e) -> KM.lookup "ok" e == Just (Bool False)
    _ -> False
executionFailed _ = False

-- | The red cell's diagnostic text, from the execution report.
outcomeDiagnostic :: ToolOutcome -> Text
outcomeDiagnostic (ToolOk (Object o)) = case KM.lookup "execution" o of
    Just (Object e) -> textField "error" e
    _ -> textField "error" o
  where
    textField k m = case KM.lookup k m of
        Just (String s) -> s
        _ -> ""
outcomeDiagnostic _ = ""

{- | Browse the given modules and render the mode's message; @proven@ names
(the writing cell's own uses) are exempt from the card's noise filter (R9.7).
-}
discoverSurfaces ::
    GrammarMode -> Dispatch -> [Text] -> [(Text, ImportStyle)] -> IO [Value]
discoverSurfaces mode dispatch proven specs = do
    surfaces <- mapM (browseSurface dispatch) specs
    pure (discoverMessages mode proven surfaces)

proactiveDiscover :: GrammarMode -> Dispatch -> IO [Value]
proactiveDiscover GrammarOff _ = pure []
proactiveDiscover GrammarOn dispatch = do
    srcs <- notebookSources dispatch
    runDiscover GrammarOn dispatch [asInsert s | s <- srcs]
  where
    asInsert s = ToolCall "insert_cell" (object ["source" .= s])

seamDiscover :: GrammarMode -> Dispatch -> [(Text, Text)] -> IO [Value]
seamDiscover GrammarOff _ _ = pure []
seamDiscover GrammarOn dispatch redCells =
    discoverSurfaces GrammarOn dispatch proven (nub targets)
  where
    targets = concat [rediscoverModules src err | (src, err) <- redCells]
    proven = concatMap (usedNames . fst) redCells

{- | Both arms render through the ONE synthesized-card path (R3.6): the raw
":browse" banner is gone — it leaked package-hash names unbounded.
-}
discoverMessages :: GrammarMode -> [Text] -> [Surface] -> [Value]
discoverMessages _ _ [] = []
discoverMessages _ proven surfaces = discoverGrammarMsg proven surfaces

browseSurface :: Dispatch -> (Text, ImportStyle) -> IO Surface
browseSurface dispatch (m, style) = do
    out <- dispatch (browseCall m)
    pure (Surface m style (browseText out))

discoverModules :: ToolCall -> [(Text, ImportStyle)]
discoverModules tc
    | not (isOwningTool (tcName tc)) = []
    | not (declaresDeps src) = []
    | otherwise = importedModules src
  where
    src = cellSource (tcArgs tc)

browseCall :: Text -> ToolCall
browseCall m =
    ToolCall "find_function" (object ["query" .= m])

browseText :: Either Text ToolOutcome -> Text
browseText (Right (ToolOk v)) = valueText v
browseText _ = ""

valueText :: Value -> Text
valueText (String s) = s
valueText (Array a) = T.intercalate "\n" (map valueText (toList a))
valueText (Object o) = T.intercalate "\n" (map valueText (KM.elems o))
valueText _ = ""

{- | The card message: shrunk until the wrapped envelope fits the budget
(R3.9), then gated useful-or-absent (9.1, R5.8\/R9.7) — browse\/compile-
verified names only, non-empty body, descriptive framing, else NOTHING.
-}
discoverGrammarMsg :: [Text] -> [Surface] -> [Value]
discoverGrammarMsg _ [] = []
discoverGrammarMsg proven surfaces =
    [ discoverEnvelope card
    | card <- [go (envelopeCharBudget - overhead)]
    , emittableCard (`elem` verified) card
    ]
  where
    verified =
        proven
            ++ [ lastSeg n
               | Surface _ _ browse <- surfaces
               , BrowseEntry n _ <- parseBrowse browse
               ]
    lastSeg = last . T.splitOn "."
    overhead = envelopeChars (discoverEnvelope "")
    go budget
        | budget <= 0 = ""
        | envelopeChars (discoverEnvelope card) <= envelopeCharBudget = card
        | otherwise = go (budget - shrinkStep)
      where
        card = synthesizeGrammarBounded budget proven surfaces
    shrinkStep = 100

discoverEnvelope :: Text -> Value
discoverEnvelope content =
    object
        [ "role" .= ("tool" :: Text)
        , "tool_name" .= ("discover" :: Text)
        , "content" .= content
        ]

{- | Every cell's source for discovery, in ONE @list_cells full:true@ call.
The preview default omits @source@ entirely, so @full@ must be requested —
without it the proactive lever is structurally dead (TRIAGE M1).
-}
notebookSources :: Dispatch -> IO [Text]
notebookSources dispatch = do
    listed <- dispatch (ToolCall "list_cells" (object ["full" .= True]))
    pure (cellSources (outcomeValue listed))

outcomeValue :: Either Text ToolOutcome -> Value
outcomeValue (Right (ToolOk v)) = v
outcomeValue _ = Null

-- | The @source@ field of each cell in a @list_cells@ result.
cellSources :: Value -> [Text]
cellSources (Array a) =
    [s | Object c <- toList a, Just (String s) <- [KM.lookup "source" c]]
cellSources (Object o) = maybe [] cellSources (KM.lookup "cells" o)
cellSources _ = []

toolCallSource :: ToolCall -> Text
toolCallSource = cellSource . tcArgs

cellSource :: Value -> Text
cellSource (Object o) = case (KM.lookup "source" o, KM.lookup "new_source" o) of
    (Just (String s), _) -> s
    (_, Just (String s)) -> s
    _ -> ""
cellSource _ = ""

declaresDeps :: Text -> Bool
declaresDeps src =
    any (("-- cabal:" `T.isPrefixOf`) . T.strip) (T.lines src)

-- | Whether a write call's source declares a @-- cabal:@ dependency line.
declaresDepsCall :: ToolCall -> Bool
declaresDepsCall = declaresDeps . toolCallSource

{- | The package names a cell's @-- cabal: build-depends:@ lines declare,
version bounds stripped — the R1.4 world-change legality key.
-}
declaredPackages :: Text -> [Text]
declaredPackages src =
    [ pkg
    | l <- T.lines src
    , "-- cabal:" `T.isPrefixOf` T.strip l
    , let (_, rest) = T.breakOn "build-depends:" l
    , not (T.null rest)
    , entry <- T.splitOn "," (T.drop (T.length "build-depends:") rest)
    , let pkg = T.takeWhile (\c -> c `notElem` (" =<>^&" :: String)) (T.strip entry)
    , not (T.null pkg)
    ]
