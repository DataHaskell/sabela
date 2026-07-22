{-# LANGUAGE OverloadedStrings #-}

module Test.DiscoverSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Grammar (ImportStyle (..))
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Check (CheckResult (..))
import Test.Hspec

import Eval.Agent (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    defaultBudget,
    discoverModules,
    runEpisodeWith,
 )
import Eval.Discover (GrammarMode (..), proactiveDiscover, rediscoverModules)
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
        it "does NOT re-browse on a no-instance error (§9.1: not an unresolved symbol)" $
            rediscoverModules src "error: No instance for (Num Text)"
                `shouldBe` []
        it "re-browses on a module-load failure (unresolved module)" $
            rediscoverModules src "error: Could not load module `DataFrame'"
                `shouldBe` [("DataFrame", QualifiedAs "D")]
        it "does not re-discover on an in-grammar type error" $
            rediscoverModules src "Couldn't match expected type Int with Double"
                `shouldBe` []

    describe "proactiveDiscover (M1: the ON-arm lever must actually fire)" $
        it "asks list_cells for full sources and browses the cabal cells" $ do
            calls <- newIORef []
            let multiCell =
                    [ object ["source" .= installSrc]
                    , object ["source" .= ("plain = 1" :: Text)]
                    ]
                -- Mirrors the server: `source` is only present under full:true.
                disp tc@(ToolCall name a) = do
                    modifyIORef' calls (++ [tc])
                    pure $ case name of
                        "list_cells"
                            | fullTrue a ->
                                Right (ToolOk (object ["cells" .= multiCell]))
                            | otherwise ->
                                Right
                                    ( ToolOk
                                        ( object
                                            [ "cells"
                                                .= [object ["id" .= (0 :: Int)]]
                                            ]
                                        )
                                    )
                        "find_function" -> Right (browseResult (argOf a))
                        _ -> Right (ToolOk (object []))
            msgs <- proactiveDiscover GrammarOn disp
            null msgs `shouldBe` False
            issued <- readIORef calls
            let listArgs = [a | ToolCall "list_cells" a <- issued]
            length listArgs `shouldBe` 1
            all fullTrue listArgs `shouldBe` True
            [argOf a | ToolCall "find_function" a <- issued]
                `shouldBe` ["Granite.Svg", "Data.Text"]

    describe "runEpisodeWith (discover after a RED install, R6.10)" $ do
        it
            "browses the modules a red install implicated and feeds their \
            \signatures into the next proposal"
            $ do
                calls <- newIORef []
                seen <- newIORef ([] :: [[Value]])
                let disp tc@(ToolCall name a) = do
                        modifyIORef' calls (++ [tc])
                        pure $ case name of
                            "find_function" -> Right (browseResult (argOf a))
                            "insert_cell" -> Right redInstallOutcome
                            _ -> Right okOutcome
                    chatRec msgs = do
                        modifyIORef' seen (++ [msgs])
                        cur <- length <$> readIORef seen
                        pure (Right (script !! (cur - 1)))
                    driver =
                        Driver
                            { drvChat = chatRec
                            , drvDispatch = disp
                            , drvNow = pure 0
                            , drvVerify = pure (CheckPassed, Nothing)
                            }
                run <- runEpisodeWith openBudget driver (taskPrompt dummyTask) 10
                arStopped run `shouldBe` "done"
                issued <- readIORef calls
                let browsed = [argOf a | ToolCall "find_function" a <- issued]
                browsed `shouldBe` ["Granite.Svg", "Data.Text"]
                contexts <- readIORef seen
                let lastCtx = renderMsgs (last contexts)
                ("bars ::" `T.isInfixOf` lastCtx) `shouldBe` True
        it "a SUCCEEDED install write gets no module-API bytes (R6.10)" $ do
            calls <- newIORef []
            let disp tc@(ToolCall name a) = do
                    modifyIORef' calls (++ [tc])
                    pure $ case name of
                        "find_function" -> Right (browseResult (argOf a))
                        _ -> Right okOutcome
                scriptOnce msgs =
                    pure . Right $
                        if any hasInstall msgs then doneTurn else installTurn
                hasInstall v = "insert_cell" `T.isInfixOf` T.pack (show v)
                driver =
                    Driver
                        { drvChat = scriptOnce
                        , drvDispatch = disp
                        , drvNow = pure 0
                        , drvVerify = pure (CheckPassed, Nothing)
                        }
            run <- runEpisodeWith openBudget driver (taskPrompt dummyTask) 10
            arStopped run `shouldBe` "done"
            issued <- readIORef calls
            [argOf a | ToolCall "find_function" a <- issued] `shouldBe` []
  where
    script = [installTurn, fixTurn, doneTurn]
    installTurn =
        Turn
            (object ["role" .= ("assistant" :: Text)])
            ""
            [ToolCall "insert_cell" (object ["source" .= installSrc])]
    fixTurn =
        Turn
            (object ["role" .= ("assistant" :: Text)])
            ""
            [ ToolCall
                "replace_cell_source"
                ( object
                    [ "cell_id" .= (1 :: Int)
                    , "new_source" .= (installSrc <> "\nx = bars" :: Text)
                    ]
                )
            ]
    doneTurn = Turn (object ["role" .= ("assistant" :: Text)]) "done" []
    redInstallOutcome =
        ToolOk $
            object
                [ "cellId" .= (1 :: Int)
                , "execution"
                    .= object
                        [ "ok" .= False
                        , "error" .= ("Variable not in scope: bars" :: Text)
                        ]
                ]
    okOutcome =
        ToolOk $
            object
                [ "cellId" .= (1 :: Int)
                , "execution" .= object ["ok" .= True]
                ]

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

-- | Does a list_cells argument object carry @full: true@ (the M1 fix)?
fullTrue :: Value -> Bool
fullTrue (Object o) = KM.lookup "full" o == Just (Bool True)
fullTrue _ = False

renderMsgs :: [Value] -> Text
renderMsgs = T.intercalate "\n" . map (T.pack . show)
