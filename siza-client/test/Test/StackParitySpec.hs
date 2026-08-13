{-# LANGUAGE OverloadedStrings #-}

{- | The pin against drift: the chat loop and the MCP server must wrap a tool
call in the SAME layers. Function equality is not decidable, so the pin is
observational — one grid of calls, two entry points, identical tapes.
-}
module Test.StackParitySpec (stackParitySpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), decodeStrict, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.GrammarCards (GrammarMode (..))
import Siza.Agent.Loop (episodeStack)
import Siza.Agent.Stack (
    Dispatch,
    StackSession (..),
    Surface (..),
    SurfacePolicy (..),
    newStackSession,
    ssHealReds,
    stackLayers,
    surfacePolicy,
 )
import Siza.Agent.Stack.Call (CallResult (..), StackNote (..), runToolCall)
import Siza.Agent.Tools (renderOutcome)
import Siza.Lang.Haskell (parseModuleE)
import Siza.Mcp (McpEnv, mcpEnvOver, mcpSession, mcpStackFor, toolsCall)
import Test.StackFixtures (Fake, recordingNotebook)
import Test.TruthGen (genIdent, genModuleName, genPackage, genSource)

stackParitySpec :: Spec
stackParitySpec = describe "chat and MCP drive the same stack" $ do
    forM_ sessionKinds $ \(label, mkSession) -> do
        it ("puts the identical calls on the wire for the identical grid: " <> label) $ do
            (chatTape, _) <- runGrid mkSession episodeStack
            (mcpTape, _) <- runGrid mkSession mcpStackFor
            mcpTape `shouldBe` chatTape

        it ("returns the identical notes for the identical grid: " <> label) $ do
            (_, chatNotes) <- runGrid mkSession episodeStack
            (_, mcpNotes) <- runGrid mkSession mcpStackFor
            mcpNotes `shouldBe` chatNotes
            concat mcpNotes `shouldSatisfy` (not . null)

    it "exercises every layer, so equality is not equality of nothing" $ do
        (tape, notes) <- runGrid chatSession episodeStack
        concat notes `shouldSatisfy` any ((== "routed_retry") . snKind)
        map tcName tape
            `shouldSatisfy` \ns ->
                all (`elem` ns) ["insert_cell", "discover", "delete_cell"]
        stackLayers `shouldBe` ["normalize", "goal", "discover-ledger", "futility"]

    it "does not let a caller pick the chat surface's repair beat (C3-3)" $ do
        ss <- newStackSession GrammarOn ""
        ssHealReds ss `shouldBe` spHealsReds (surfacePolicy ChatSurface)

    it "gives each entry point the policy its surface declares (C3-3)" $ do
        mcp <- mcpSession
        chat <- newStackSession GrammarOn ""
        map ssSurface [chat, mcp] `shouldBe` [ChatSurface, McpSurface]
        surfacePolicy McpSurface `shouldNotBe` surfacePolicy ChatSurface

    it "carries onto MCP every field the chat result carries (C3-12)" $
        property $
            forAll genCall $ \call -> ioProperty $ do
                chat <- chatKeys call
                mcp <- mcpKeys call
                pure
                    ( counterexample
                        (show (call, chat, mcp))
                        (all (`elem` mcp) chat)
                    )

    it "agrees field for field on a call both surfaces put on the wire" $
        property $
            forAll (pure (ToolCall "list_cells" (object []))) $ \call ->
                ioProperty $ do
                    chat <- chatFields call
                    mcp <- mcpFields call
                    pure (KM.toList chat === KM.toList mcp)

    it "surfaces on MCP the discovery the chat loop injects (C3-6)" $
        property $
            forAll ((,,) <$> genPackage <*> genModuleName <*> genIdent) $
                \(pkg, m, n) -> ioProperty $ do
                    viaMcp <- discoveryNotes mcpStackFor pkg m n
                    viaChat <- discoveryNotes episodeStack pkg m n
                    pure
                        ( counterexample
                            (show (viaMcp, viaChat))
                            ("discover" `elem` viaMcp)
                            .&&. viaChat === viaMcp
                        )

-- | The two entry points' own result renderings, as JSON field maps.
chatFields :: ToolCall -> IO (KM.KeyMap Value)
chatFields call = do
    ss <- newStackSession GrammarOn ""
    cr <- runToolCall ss (episodeStack ss serverLike) call
    pure (jsonFields (renderOutcome (crOutcome cr)))

mcpFields :: ToolCall -> IO (KM.KeyMap Value)
mcpFields call = do
    ss <- mcpSession
    let env = mcpEnvOver ss serverLike [] :: McpEnv
    r <- toolsCall env (object ["name" .= tcName call, "arguments" .= tcArgs call])
    pure (jsonFields (firstBlock r))

chatKeys :: ToolCall -> IO [Text]
chatKeys = fmap (map K.toText . KM.keys) . chatFields

mcpKeys :: ToolCall -> IO [Text]
mcpKeys = fmap (map K.toText . KM.keys) . mcpFields

-- | The JSON object a surface's rendering carries, whatever label precedes it.
jsonFields :: Text -> KM.KeyMap Value
jsonFields t = case decodeStrict (TE.encodeUtf8 (T.dropWhile (/= '{') t)) of
    Just (Object o) -> o
    _ -> KM.empty

firstBlock :: Value -> Text
firstBlock (Object o) = case KM.lookup (K.fromText "content") o of
    Just (Array a) -> case foldr (:) [] a of
        (Object b : _) -> case KM.lookup (K.fromText "text") b of
            Just (String s) -> s
            _ -> ""
        _ -> ""
    _ -> ""
firstBlock _ = ""

{- | A server that refuses what does not parse and commits what does, with the
refusal shape the capability layer returns.
-}
serverLike :: Fake
serverLike (ToolCall name argv)
    | name `elem` ["insert_cell", "replace_cell_source"] =
        pure $ case parseModuleE src of
            Left _ ->
                Right
                    ( ToolErr
                        ( object
                            [ "error" .= ("the write was refused" :: Text)
                            , "verdict" .= ("diagnostic" :: Text)
                            , "notCommitted" .= ("compile-gate" :: Text)
                            , "diagnostic" .= ("parse error" :: Text)
                            , "source" .= src
                            ]
                        )
                    )
            Right _ ->
                Right
                    ( ToolOk
                        ( object
                            [ "cellId" .= (1 :: Int)
                            , "execution" .= object ["ok" .= True]
                            ]
                        )
                    )
  where
    src = argOf "source" argv
serverLike (ToolCall "list_cells" _) =
    pure (Right (ToolOk (object ["cells" .= ([] :: [Value])])))
serverLike _ = pure (Right (ToolOk (object ["result" .= ("" :: Text)])))

genCall :: Gen ToolCall
genCall =
    oneof
        [ ToolCall "insert_cell" . srcArgs <$> genSource
        , ToolCall "insert_cell" . srcArgs <$> genBadSource
        , pure (ToolCall "list_cells" (object []))
        , ToolCall "discover" . (\q -> object ["query" .= q]) <$> genIdent
        ]
  where
    srcArgs s = object ["source" .= s]

genBadSource :: Gen Text
genBadSource = do
    n <- genIdent
    elements [n <> " = ", n <> " = (", n <> " = do", "data = 1"]

-- | A dep-declaring write that lands red on an unbound name.
depsInsert :: Text -> Text -> Text -> ToolCall
depsInsert pkg m n =
    ToolCall
        "insert_cell"
        ( object
            [ "source"
                .= ( "-- cabal: build-depends: "
                        <> pkg
                        <> "\nimport "
                        <> m
                        <> "\nresult = "
                        <> n
                        <> " 1"
                   )
            ]
        )

{- | A notebook whose write runs red on an unbound name and whose module
browse answers with that name's signature.
-}
browsingNotebook :: Text -> Fake
browsingNotebook n (ToolCall name _)
    | name == "insert_cell" =
        pure
            ( Right
                ( ToolOk
                    ( object
                        [ "cellId" .= (1 :: Int)
                        , "execution"
                            .= object
                                [ "ok" .= False
                                , "error" .= ("Variable not in scope: " <> n)
                                ]
                        ]
                    )
                )
            )
    | otherwise =
        pure
            ( Right
                (ToolOk (object ["exports" .= [n <> " :: Int -> Int"]]))
            )

argOf :: Text -> Value -> Text
argOf k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
argOf _ _ = ""

{- | The two session constructors production uses. A parity claim proved under
one surface's session says nothing about the layers the other's policy turns on.
-}
sessionKinds :: [(String, IO StackSession)]
sessionKinds = [("chat session", chatSession), ("mcp session", mcpSession)]

chatSession :: IO StackSession
chatSession = newStackSession GrammarOn ""

{- | The note kinds a dep-declaring write that lands red comes back with, on
the session whose policy folds discovery into the call.
-}
discoveryNotes ::
    (StackSession -> Dispatch -> Dispatch) -> Text -> Text -> Text -> IO [Text]
discoveryNotes mkStack pkg m n = do
    ss <- mcpSession
    cr <- runToolCall ss (mkStack ss (browsingNotebook n)) (depsInsert pkg m n)
    pure (map snKind (crNotes cr))

{- | Run the grid through one entry point's stack: what reached the wire, and
what came back as notes.
-}
runGrid ::
    IO StackSession ->
    (StackSession -> Dispatch -> Dispatch) ->
    IO ([ToolCall], [[StackNote]])
runGrid mkSession mkStack = do
    (fake, tape) <- recordingNotebook
    ss <- mkSession
    results <- mapM (runToolCall ss (mkStack ss fake)) grid
    seen <- readIORef tape
    pure (seen, map crNotes results)

grid :: [ToolCall]
grid =
    [ ToolCall
        "insert_cell"
        (object ["input" .= object ["source" .= ("x = 1" :: Text)]])
    , ToolCall "insert_cell" (object ["source" .= ("boom" :: Text)])
    , ToolCall "insert_cell" (object ["source" .= ("boom" :: Text)])
    , ToolCall "discover" (object ["query" .= ("Map" :: Text)])
    , ToolCall "discover" (object ["query" .= ("Map" :: Text)])
    , ToolCall
        "insert_cell"
        (object ["source" .= ("y = 2" :: Text), "goal" .= ("show a chart" :: Text)])
    , ToolCall "insert_cell" (object ["source" .= ("blocked = 3" :: Text)])
    , ToolCall "delete_cell" (object ["cell_id" .= (101 :: Int)])
    ]
