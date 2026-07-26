{-# LANGUAGE OverloadedStrings #-}

module Test.LeverSurfaceSpec (leverSurfaceSpec) where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..))
import Siza.Agent.Check (CheckResult (..))
import Siza.Agent.Discover (
    GrammarMode (..),
    proactiveDiscover,
    runDiscoverOutcomes,
    seamDiscover,
 )
import Siza.Agent.Loop (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    runEpisodeSeeded,
 )
import Test.DiscoverFixtures (argText, field, textField)

installSrc :: Text
installSrc = "-- cabal: build-depends: granite\nimport Granite.Svg\nbars' = ()"

moduleErr :: Text
moduleErr = "error: Could not load module 'Granite.Svg'"

redInstall :: (ToolCall, ToolOutcome)
redInstall =
    ( ToolCall "insert_cell" (object ["source" .= installSrc])
    , ToolOk
        ( object
            [ "execution"
                .= object ["ok" .= False, "error" .= moduleErr]
            ]
        )
    )

dispatchFix :: ToolCall -> IO (Either Text ToolOutcome)
dispatchFix (ToolCall name _) = pure . Right . ToolOk $ case name of
    "find_function" ->
        object ["result" .= ("bars :: [(Text, Double)] -> Plot -> Text" :: Text)]
    "list_cells" -> object ["cells" .= ([] :: [Value])]
    _ -> object ["ok" .= True]

dispatchWithSource :: ToolCall -> IO (Either Text ToolOutcome)
dispatchWithSource (ToolCall "list_cells" _) =
    pure . Right . ToolOk $
        object ["cells" .= [object ["id" .= (1 :: Int), "source" .= installSrc]]]
dispatchWithSource tc = dispatchFix tc

gridCalls :: [Value]
gridCalls =
    [ object ["query" .= ("colX" :: Text)]
    , object ["query" .= ("colX" :: Text)]
    , object ["query" .= ("mystA" :: Text)]
    , object ["query" .= ("mystA" :: Text), "mode" .= ("inventory" :: Text)]
    , object ["query" .= ("mystA" :: Text), "mode" .= ("construct" :: Text)]
    , object ["query" .= ("mystA" :: Text), "module" .= ("Zoo.Charts" :: Text)]
    , object ["query" .= ("colX" :: Text)]
    , object ["query" .= ("mystB" :: Text)]
    ]

foundColX :: Value
foundColX =
    object
        [ "state" .= ("found" :: Text)
        , "query" .= ("colX" :: Text)
        , "shown" .= (1 :: Int)
        , "total" .= (1 :: Int)
        , "hits"
            .= [ object
                    [ "name" .= ("colX" :: Text)
                    , "module" .= ("Zoo.Charts" :: Text)
                    , "package" .= ("zoo" :: Text)
                    , "version" .= ("1.0" :: Text)
                    , "install" .= ("installed" :: Text)
                    , "matchKind" .= ("exact" :: Text)
                    , "origin" .= ("index" :: Text)
                    , "type" .= ("Plot -> Int" :: Text)
                    , "cabal" .= ("build-depends: zoo" :: Text)
                    ]
               ]
        ]

missOf :: Text -> Value
missOf q =
    object
        [ "state" .= ("not_found" :: Text)
        , "query" .= q
        , "shown" .= (0 :: Int)
        , "hits" .= ([] :: [Value])
        , "consulted" .= (["index"] :: [Text])
        ]

argQuery :: ToolCall -> Text
argQuery = argText "query" . tcArgs

catalogue :: ToolCall -> IO (Either Text ToolOutcome)
catalogue tc = pure . Right . ToolOk $ case tcName tc of
    "list_cells" -> object ["cells" .= ([] :: [Value])]
    "discover"
        | argQuery tc == "colX" -> foundColX
        | otherwise -> missOf (argQuery tc)
    _ -> object ["ok" .= True]

worldWithSource :: ToolCall -> IO (Either Text ToolOutcome)
worldWithSource tc = case tcName tc of
    "list_cells" ->
        pure . Right . ToolOk $
            object
                ["cells" .= [object ["id" .= (1 :: Int), "source" .= installSrc]]]
    "find_function" ->
        pure . Right . ToolOk $
            object
                ["result" .= ("bars :: [(Text, Double)] -> Plot -> Text" :: Text)]
    _ -> catalogue tc

planted :: GrammarMode -> ToolCall -> IO (Either Text ToolOutcome)
planted GrammarOn tc = catalogue tc
planted GrammarOff tc
    | tcName tc == "discover"
    , argQuery tc == "colX" =
        pure (Right (ToolOk (missOf "colX")))
    | otherwise = catalogue tc

armGrid ::
    (GrammarMode -> ToolCall -> IO (Either Text ToolOutcome)) ->
    GrammarMode ->
    IO (AgentRun, [Value])
armGrid world mode = do
    counter <- newIORef (0 :: Int)
    seen <- newIORef ([] :: [Value])
    let chat _ = do
            n <- readIORef counter
            writeIORef counter (n + 1)
            pure . Right $
                Turn
                    (object ["role" .= ("assistant" :: Text), "content" .= ("" :: Text)])
                    ""
                    [ToolCall "discover" (gridCalls !! (n `mod` length gridCalls))]
        dispatch tc = do
            modifyIORef'
                seen
                (++ [object ["tool" .= tcName tc, "args" .= tcArgs tc]])
            world mode tc
        driver =
            Driver
                { drvChat = chat
                , drvDispatch = dispatch
                , drvNow = pure 0
                , drvVerify = pure (CheckUncheckable, Nothing)
                }
    run <-
        runEpisodeSeeded
            []
            (const (pure ()))
            mode
            EpisodeBudget{ebMaxRepairs = 4, ebDeadlineSecs = 600}
            driver
            "find the plotting api"
            (length gridCalls)
    inner <- readIORef seen
    pure (run, inner)

transcriptBytes :: AgentRun -> [Text]
transcriptBytes =
    map (TE.decodeUtf8 . LBS.toStrict . encode) . arTranscript

toolContents :: Text -> AgentRun -> [Text]
toolContents name run =
    [ textField "content" m
    | m <- arTranscript run
    , textField "role" m == "tool"
    , textField "tool_name" m == name
    ]

leverSurfaceSpec :: Spec
leverSurfaceSpec = describe "grammar-lever surface (post-R5-T1 saturation proof)" $ do
    it "write-triggered discover cards are byte-identical across arms (R3.6)" $ do
        onMsgs <- runDiscoverOutcomes GrammarOn dispatchFix [redInstall]
        offMsgs <- runDiscoverOutcomes GrammarOff dispatchFix [redInstall]
        onMsgs `shouldBe` offMsgs
        onMsgs `shouldSatisfy` (not . null)
    it "proactive discovery on an empty notebook emits nothing on EITHER arm" $ do
        onMsgs <- proactiveDiscover GrammarOn dispatchFix
        offMsgs <- proactiveDiscover GrammarOff dispatchFix
        (onMsgs, offMsgs) `shouldBe` ([], [])
    it "the seam card is the lever's remaining distinguishable surface" $ do
        onMsgs <- seamDiscover GrammarOn dispatchFix [(installSrc, moduleErr)]
        offMsgs <- seamDiscover GrammarOff dispatchFix [(installSrc, moduleErr)]
        offMsgs `shouldBe` []
        onMsgs `shouldSatisfy` (not . null)
    it "proactive discovery on a dep-declaring notebook is GrammarOn-only" $ do
        onMsgs <- proactiveDiscover GrammarOn dispatchWithSource
        offMsgs <- proactiveDiscover GrammarOff dispatchWithSource
        offMsgs `shouldBe` []
        onMsgs `shouldSatisfy` (not . null)
    gridSpec

gridSpec :: Spec
gridSpec = describe "discover-call-class grid (R8-T4 arm-independence pin)" $ do
    it "the whole grid is byte-identical across arms, goal args included" $ do
        (runOn, argsOn) <- armGrid (const catalogue) GrammarOn
        (runOff, argsOff) <- armGrid (const catalogue) GrammarOff
        transcriptBytes runOn `shouldBe` transcriptBytes runOff
        (arFinal runOn, arStopped runOn)
            `shouldBe` (arFinal runOff, arStopped runOff)
        discoverArgs argsOn `shouldBe` discoverArgs argsOff
    it "the grid actually engages every discover-call class" $ do
        (run, args) <- armGrid (const catalogue) GrammarOff
        let outs = toolContents "discover" run
        outs `shouldSatisfy` any ("\"state\":\"found\"" `T.isInfixOf`)
        outs `shouldSatisfy` any ("\"state\":\"duplicate\"" `T.isInfixOf`)
        outs `shouldSatisfy` (not . any ("satisfied by held facts" `T.isInfixOf`))
        args `shouldSatisfy` any (\a -> goalTypeIn a == "Plot")
    it "RED on the plant: a mode-gated honesty branch breaks grid equality" $ do
        (runOn, _) <- armGrid planted GrammarOn
        (runOff, _) <- armGrid planted GrammarOff
        (transcriptBytes runOn == transcriptBytes runOff) `shouldBe` False
        toolContents "discover" runOff
            `shouldSatisfy` any ("not_found" `T.isInfixOf`)
    it "a dep-declaring notebook differs ONLY by the proactive card" $ do
        (runOn, _) <- armGrid (const worldWithSource) GrammarOn
        (runOff, _) <- armGrid (const worldWithSource) GrammarOff
        let cardMsg m = "Live API grammar" `T.isInfixOf` textField "content" m
            keepOn = filter (not . cardMsg) (arTranscript runOn)
        arTranscript runOn `shouldSatisfy` any cardMsg
        arTranscript runOff `shouldSatisfy` (not . any cardMsg)
        map enc keepOn `shouldBe` map enc (arTranscript runOff)
  where
    discoverArgs = filter (\a -> textField "tool" a == "discover")
    goalTypeIn a =
        maybe "" (textField "type") (field "args" a >>= field "_goal")
    enc = TE.decodeUtf8 . LBS.toStrict . encode
