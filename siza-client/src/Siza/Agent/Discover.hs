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
    refusedSource,
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

refusedSource :: ToolCall -> ToolOutcome -> Maybe Text
refusedSource tc out
    | not (isOwningTool (tcName tc)) = Nothing
    | ToolErr (Object o) <- out
    , Just (String _) <- KM.lookup "notCommitted" o =
        case toolCallSource tc of
            src | not (T.null (T.strip src)) -> Just src
            _ -> Nothing
    | otherwise = Nothing

executionSucceeded :: ToolOutcome -> Bool
executionSucceeded (ToolOk (Object o)) = case KM.lookup "execution" o of
    Just (Object e) -> okField e
    _ -> okField o
  where
    okField m = case KM.lookup "ok" m of
        Just (Bool b) -> b
        _ -> False
executionSucceeded _ = False

executionFailed :: ToolOutcome -> Bool
executionFailed (ToolOk (Object o)) = case KM.lookup "execution" o of
    Just (Object e) -> KM.lookup "ok" e == Just (Bool False)
    _ -> False
executionFailed _ = False

outcomeDiagnostic :: ToolOutcome -> Text
outcomeDiagnostic (ToolOk (Object o)) = case KM.lookup "execution" o of
    Just (Object e) -> textField "error" e
    _ -> textField "error" o
  where
    textField k m = case KM.lookup k m of
        Just (String s) -> s
        _ -> ""
outcomeDiagnostic _ = ""

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

notebookSources :: Dispatch -> IO [Text]
notebookSources dispatch = do
    listed <- dispatch (ToolCall "list_cells" (object ["full" .= True]))
    pure (cellSources (outcomeValue listed))

outcomeValue :: Either Text ToolOutcome -> Value
outcomeValue (Right (ToolOk v)) = v
outcomeValue _ = Null

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

declaresDepsCall :: ToolCall -> Bool
declaresDepsCall = declaresDeps . toolCallSource

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
