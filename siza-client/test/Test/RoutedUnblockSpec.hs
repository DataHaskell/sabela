{-# LANGUAGE OverloadedStrings #-}

{- | R9-T4 routed unblock: a scripted caller whose insert is blocked by a
planted red cell converges to replace_cell_source on that cell within one turn
through the REAL 'runEpisodeSeeded' — the insert-retry loop (the topMonth
turn-30 trap) is unrepresentable, the blocked source lands green on the named
cell, and the routed retry is disclosed.
-}
module Test.RoutedUnblockSpec (routedUnblockSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..))
import Siza.Agent.Check (CheckResult (..))
import Siza.Agent.Loop (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    runEpisodeSeeded,
 )
import Test.DiscoverFixtures (textField)

blockedCellId :: Int
blockedCellId = 9

fixSource :: Text
fixSource = "topMonth :: String\ntopMonth = \"Jul\""

{- | The pending-error refusal an insert earns while cell 9 is red — the
decodable write-ack refusal shape (@refusal@ + @cellId@) the client routes on.
-}
pendingRefusal :: Value
pendingRefusal =
    object
        [ "error"
            .= ( "Cell "
                    <> T.pack (show blockedCellId)
                    <> " has an unresolved error, so a new cell cannot be added. \
                       \Fix it with replace_cell_source, or remove it with delete_cell."
               )
        , "refusal" .= ("pending-error" :: Text)
        , "cellId" .= blockedCellId
        , "pendingErrorCell" .= blockedCellId
        ]

-- | Run the trapped caller: it inserts on turn 0, then declares done.
runBlocked :: IO (AgentRun, [ToolCall])
runBlocked = do
    seen <- newIORef ([] :: [ToolCall])
    turnRef <- newIORef (0 :: Int)
    run <-
        runEpisodeSeeded
            []
            (const (pure ()))
            GrammarOff
            (EpisodeBudget{ebMaxRepairs = 50, ebDeadlineSecs = 600})
            (driver seen turnRef)
            "define topMonth"
            8
    calls <- readIORef seen
    pure (run, calls)
  where
    driver seen turnRef =
        Driver
            { drvChat = chat turnRef
            , drvDispatch = dispatch seen
            , drvNow = pure 0
            , drvVerify = pure (CheckPassed, Nothing)
            }
    dispatch seen tc = do
        modifyIORef' seen (++ [tc])
        pure . Right $ case tcName tc of
            "insert_cell" -> ToolErr pendingRefusal
            "replace_cell_source" ->
                ToolOk
                    ( object
                        [ "cellId" .= blockedCellId
                        , "execution" .= object ["ok" .= True]
                        ]
                    )
            "list_cells" -> ToolOk (object ["cells" .= ([] :: [Value])])
            _ -> ToolOk (object [])
    chat turnRef _ = do
        n <- readIORef turnRef
        modifyIORef' turnRef (+ 1)
        pure . Right $
            if n == 0
                then
                    Turn
                        (object ["role" .= ("assistant" :: Text), "content" .= ("inserting" :: Text)])
                        ""
                        [ToolCall "insert_cell" (object ["source" .= fixSource])]
                else
                    Turn
                        (object ["role" .= ("assistant" :: Text), "content" .= ("done" :: Text)])
                        ""
                        []

routedUnblockSpec :: Spec
routedUnblockSpec = describe "routed unblock: blocked insert converges to replace (R9-T4)" $ do
    it "re-dispatches the model's source as replace_cell_source on the named cell" $ do
        (_, calls) <- runBlocked
        let replaces =
                [ tc
                | tc <- calls
                , tcName tc == "replace_cell_source"
                , argInt "cell_id" tc == Just blockedCellId
                ]
        replaces `shouldSatisfy` (not . null)
        map (argText "new_source") replaces `shouldSatisfy` all (== fixSource)
    it "never insert-retries: the blocked insert is not re-sent turn after turn" $ do
        (_, calls) <- runBlocked
        let inserts = length [() | tc <- calls, tcName tc == "insert_cell"]
        inserts `shouldSatisfy` (<= 1)
    it "converges: the episode ends done, the source landed green on the cell" $ do
        (run, _) <- runBlocked
        arStopped run `shouldBe` "done"
    it "discloses the routed retry in the transcript" $ do
        (run, _) <- runBlocked
        let disclosed =
                any
                    (T.isInfixOf "routed retry" . textField "content")
                    (arTranscript run)
        disclosed `shouldBe` True

-- | Read an int / text tool-call argument.
argInt :: Text -> ToolCall -> Maybe Int
argInt k tc = case tcArgs tc of
    Object o -> case KM.lookup (K.fromText k) o of
        Just (Number x) -> Just (round x)
        _ -> Nothing
    _ -> Nothing

argText :: Text -> ToolCall -> Text
argText k tc = case tcArgs tc of
    Object o -> case KM.lookup (K.fromText k) o of
        Just (String s) -> s
        _ -> ""
    _ -> ""
