module Eval.Discover (
    GrammarMode (..),
    Dispatch,
    discoverModules,
    browseCall,
    browseText,
    discoverGrammarMsg,
    runDiscover,
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

import Sabela.AI.Grammar (ImportStyle (..))
import Sabela.AI.Grammar.Synth (Surface (..), synthesizeGrammar)
import Sabela.AI.Types (ToolOutcome (..))

import Eval.Ollama (ToolCall (..))

data GrammarMode = GrammarOn | GrammarOff
    deriving (Eq, Show)

type Dispatch = ToolCall -> IO (Either Text ToolOutcome)

isOwningTool :: Text -> Bool
isOwningTool n = n == "insert_cell" || n == "replace_cell_source"

runDiscover :: GrammarMode -> Dispatch -> [ToolCall] -> IO [Value]
runDiscover mode dispatch calls =
    discoverSurfaces mode dispatch (concatMap discoverModules calls)

discoverSurfaces ::
    GrammarMode -> Dispatch -> [(Text, ImportStyle)] -> IO [Value]
discoverSurfaces mode dispatch specs = do
    surfaces <- mapM (browseSurface dispatch) specs
    pure (discoverMessages mode surfaces)

proactiveDiscover :: GrammarMode -> Dispatch -> IO [Value]
proactiveDiscover GrammarOff _ = pure []
proactiveDiscover GrammarOn dispatch = do
    srcs <- notebookSources dispatch
    runDiscover GrammarOn dispatch [asInsert s | s <- srcs]
  where
    asInsert s = ToolCall "insert_cell" (object ["source" .= s])

rediscoverModules :: Text -> Text -> [(Text, ImportStyle)]
rediscoverModules src err
    | grammarImplicated err = importedModules src
    | otherwise = []

seamDiscover :: GrammarMode -> Dispatch -> [(Text, Text)] -> IO [Value]
seamDiscover GrammarOff _ _ = pure []
seamDiscover GrammarOn dispatch redCells =
    discoverSurfaces GrammarOn dispatch (nub targets)
  where
    targets = concat [rediscoverModules src err | (src, err) <- redCells]

grammarImplicated :: Text -> Bool
grammarImplicated err =
    any (`T.isInfixOf` T.toLower err) ["not in scope", "no instance for"]

discoverMessages :: GrammarMode -> [Surface] -> [Value]
discoverMessages _ [] = []
discoverMessages GrammarOn surfaces = discoverGrammarMsg surfaces
discoverMessages GrammarOff surfaces = map rawDiscoverMsg surfaces

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

discoverGrammarMsg :: [Surface] -> [Value]
discoverGrammarMsg [] = []
discoverGrammarMsg surfaces =
    [discoverEnvelope (synthesizeGrammar surfaces)]

rawDiscoverMsg :: Surface -> Value
rawDiscoverMsg (Surface m _ browse) =
    discoverEnvelope
        ( "Discovered API of newly installed module "
            <> m
            <> " (use these real names, do not invent any):\n"
            <> browse
        )

discoverEnvelope :: Text -> Value
discoverEnvelope content =
    object
        [ "role" .= ("tool" :: Text)
        , "tool_name" .= ("discover" :: Text)
        , "content" .= content
        ]

{- | Every cell's source for discovery. @list_cells@ now carries the full
@source@ per cell, so this is a single call — no per-cell @read_cell@ round-trip
(the storm that dominated the transcript and context).
-}
notebookSources :: Dispatch -> IO [Text]
notebookSources dispatch = do
    listed <- dispatch (ToolCall "list_cells" (object []))
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

importedModules :: Text -> [(Text, ImportStyle)]
importedModules src =
    [m | l <- T.lines src, Just m <- [importedModule (T.strip l)]]

importedModule :: Text -> Maybe (Text, ImportStyle)
importedModule l = case T.words l of
    ("import" : "qualified" : m : rest) -> Just (m, QualifiedAs (alias m rest))
    ("import" : m : _) | m /= "qualified" -> Just (m, Unqualified)
    _ -> Nothing
  where
    alias _ ("as" : p : _) = p
    alias m _ = m
