{-# LANGUAGE OverloadedStrings #-}

module Test.DiscoverSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Grammar (ImportStyle (..))
import Sabela.AI.Types (ToolOutcome (..))
import Test.Hspec

import Eval.Agent (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    defaultBudget,
    discoverModules,
    runEpisodeWith,
 )
import Eval.Discover (rediscoverModules)
import Eval.Ollama (ToolCall (..), Turn (..))
import Eval.Task (Grader (..), Task (..))

installSrc :: Text
installSrc =
    "-- cabal: build-depends: granite\nimport Granite.Svg\nimport qualified Data.Text as T"

spec :: Spec
spec = describe "D1 discover stage" $ do
    describe "discoverModules" $ do
        it "browses the imports an install brought into scope, with import style" $
            discoverModules (ToolCall "insert_cell" (object ["source" .= installSrc]))
                `shouldBe` [("Granite.Svg", Unqualified), ("Data.Text", QualifiedAs "T")]

        it "ignores a cell with no -- cabal: line (not an install)" $
            discoverModules
                ( ToolCall
                    "insert_cell"
                    (object ["source" .= ("import Data.List\nfoo = 1" :: Text)])
                )
                `shouldBe` []

        it "reads new_source on a replace_cell_source install" $
            discoverModules
                ( ToolCall
                    "replace_cell_source"
                    ( object
                        [ "new_source" .= ("-- cabal: build-depends: vector\nimport Data.Vector" :: Text)
                        ]
                    )
                )
                `shouldBe` [("Data.Vector", Unqualified)]

        it "ignores a non-owning tool" $
            discoverModules (ToolCall "read_cell" (object ["source" .= installSrc]))
                `shouldBe` []

    describe "rediscoverModules (step-4 seam classifier)" $ do
        let src = "import qualified DataFrame as D\nx = D.medianFoo df"
        it "re-browses the cell's modules on a not-in-scope error" $
            rediscoverModules src "error: Variable not in scope: D.medianFoo"
                `shouldBe` [("DataFrame", QualifiedAs "D")]
        it "re-browses on a no-instance error" $
            rediscoverModules src "error: No instance for (Num Text)"
                `shouldBe` [("DataFrame", QualifiedAs "D")]
        it "does not re-discover on an in-grammar type error" $
            rediscoverModules src "Couldn't match expected type Int with Double"
                `shouldBe` []

    describe "runEpisodeWith (discover after install)" $
        it "browses the new modules and feeds their signatures into the next proposal" $ do
            calls <- newIORef []
            seen <- newIORef ([] :: [[Value]])
            let disp tc@(ToolCall name a) = do
                    modifyIORef' calls (++ [tc])
                    pure $ case name of
                        "find_function" -> Right (browseResult (argOf a))
                        _ -> Right (ToolOk (object ["cellId" .= (1 :: Int), "ok" .= True]))
                chatRec msgs = do
                    modifyIORef' seen (++ [msgs])
                    cur <- length <$> readIORef seen
                    pure (Right (script !! (cur - 1)))
                driver =
                    Driver
                        { drvChat = chatRec
                        , drvDispatch = disp
                        , drvNow = pure 0
                        , drvVerify = pure True
                        }
            run <- runEpisodeWith openBudget driver dummyTask 10
            arStopped run `shouldBe` "done"
            issued <- readIORef calls
            let browsed = [argOf a | ToolCall "find_function" a <- issued]
            browsed `shouldBe` ["Granite.Svg", "Data.Text"]
            contexts <- readIORef seen
            let lastCtx = renderMsgs (last contexts)
            ("bars ::" `T.isInfixOf` lastCtx) `shouldBe` True
  where
    script = [installTurn, doneTurn]
    installTurn =
        Turn
            (object ["role" .= ("assistant" :: Text)])
            ""
            [ToolCall "insert_cell" (object ["source" .= installSrc])]
    doneTurn = Turn (object ["role" .= ("assistant" :: Text)]) "done" []

openBudget :: EpisodeBudget
openBudget = defaultBudget{ebMaxRepairs = maxBound, ebDeadlineSecs = 1 / 0}

dummyTask :: Task
dummyTask = Task "t" "plot something with granite" (ByValue "True")

-- | A fake :browse result naming a real-looking signature for the asked module.
browseResult :: Text -> ToolOutcome
browseResult m =
    ToolOk $
        object
            [ "op" .= ("browse" :: Text)
            , "arg" .= m
            , "result" .= ("bars :: [(Text, Double)] -> Plot -> Text" :: Text)
            ]

argOf :: Value -> Text
argOf (Object o) = case KM.lookup "query" o of
    Just (String t) -> t
    _ -> ""
argOf _ = ""

renderMsgs :: [Value] -> Text
renderMsgs = T.intercalate "\n" . map (T.pack . show)
