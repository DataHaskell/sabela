{-# LANGUAGE OverloadedStrings #-}

module Test.WorldCardSpec (worldCardSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.Envelope (envelopeCharBudget, envelopeChars)
import Siza.Agent.Discover.HistoryGuard (
    guardDiscover,
    newSearchLedger,
    seedSearchLedger,
 )
import Siza.Agent.Discover.ProducerCard (producerCard)
import Siza.Agent.Discover.Types (StandingGoal (..))
import Test.DiscoverFixtures (
    field,
    installNamesFile,
    runCatArgs,
    stateOf,
    textField,
 )

worldCardSpec :: Spec
worldCardSpec = describe "world change and the producer card (R9-T2)" $ do
    worldChangeSpec
    guardGateSpec
    producerCardSpec

gateDispatch :: ToolCall -> IO (Either Text ToolOutcome)
gateDispatch tc = case tcName tc of
    "discover" ->
        pure . Right . ToolOk $
            if q == "bars"
                then hitEnv "bars" "bars" "[(Text, Double)] -> Plot -> Text"
                else hitEnv q "defaultLineStyle" "LineStyle"
    n -> pure (Left ("unexpected tool " <> n))
  where
    q = case tcArgs tc of
        Object o | Just (String s) <- KM.lookup "query" o -> s
        _ -> ""
    hitEnv :: Text -> Text -> Text -> Value
    hitEnv qq n ty =
        object
            [ "query" .= qq
            , "state" .= ("found" :: Text)
            , "hits"
                .= [ object
                        [ "name" .= n
                        , "type" .= ty
                        , "module" .= ("M" :: Text)
                        , "package" .= ("cumulus" :: Text)
                        , "version" .= ("1.0" :: Text)
                        , "install" .= ("installed" :: Text)
                        , "matchKind" .= ("exact" :: Text)
                        , "origin" .= ("session" :: Text)
                        ]
                   ]
            , "total" .= (1 :: Int)
            ]

guardGateSpec :: Spec
guardGateSpec = describe "satisfaction never withholds an answer through guardDiscover" $
    it "every post-satisfaction call answers; none is a held-facts citation" $ do
        ledger <- newSearchLedger
        let disp q =
                guardDiscover
                    ledger
                    gateDispatch
                    (ToolCall "discover" (object ["query" .= (q :: Text)]))
        outs <- mapM disp ["bars", "hunt1", "hunt2", "hunt3", "hunt4"]
        let texts =
                [ T.intercalate " " [textField k v | k <- ["next", "summary"]]
                | Right (ToolOk v) <- outs
                ]
        length texts `shouldBe` 5
        forM_ texts $ \t ->
            t `shouldSatisfy` (not . T.isInfixOf "satisfied by held facts")

data Ev = SameDep | NewDep | Plain | Restart
    deriving (Eq, Show)

evCall :: Ev -> ToolCall
evCall SameDep =
    ToolCall
        "insert_cell"
        (object ["source" .= ("-- cabal: build-depends: dataframe" :: Text)])
evCall NewDep =
    ToolCall
        "insert_cell"
        (object ["source" .= ("-- cabal: build-depends: cumulus" :: Text)])
evCall Plain =
    ToolCall "insert_cell" (object ["source" .= ("x = 1" :: Text)])
evCall Restart = ToolCall "kernel_restart" (object [])

worldDispatch :: ToolCall -> IO (Either Text ToolOutcome)
worldDispatch tc = case tcName tc of
    "list_cells" ->
        pure . Right . ToolOk $
            object
                [ "cells"
                    .= [ object
                            [ "source"
                                .= ( "-- cabal: build-depends: dataframe\n\
                                     \import DataFrame" ::
                                        Text
                                   )
                            , "defines" .= ([] :: [Text])
                            ]
                       ]
                ]
    "discover" ->
        pure . Right . ToolOk $
            object
                [ "query" .= ("gust" :: Text)
                , "state" .= ("found" :: Text)
                , "hits"
                    .= [ object
                            [ "name" .= ("gust" :: Text)
                            , "module" .= ("Zephyr.Core" :: Text)
                            , "package" .= ("zephyr" :: Text)
                            , "version" .= ("1.0" :: Text)
                            , "install" .= ("installed" :: Text)
                            , "matchKind" .= ("exact" :: Text)
                            , "origin" .= ("session" :: Text)
                            ]
                       ]
                , "total" .= (1 :: Int)
                ]
    "kernel_restart" -> pure (Right (ToolOk (object [])))
    "insert_cell" ->
        pure
            ( Right
                (ToolOk (object ["execution" .= object ["ok" .= True]]))
            )
    n -> pure (Left ("unexpected tool " <> n))

worldChangeSpec :: Spec
worldChangeSpec = describe "worldChange fires iff an event landed (R1.4)" $
    it "judges every generated event sequence" $ do
        let seqs =
                [[]]
                    ++ [[a] | a <- evs]
                    ++ [[a, b] | a <- evs, b <- evs]
            evs = [SameDep, NewDep, Plain, Restart]
        forM_ seqs $ \events -> do
            ledger <- newSearchLedger
            seedSearchLedger worldDispatch ledger
            let disp = guardDiscover ledger worldDispatch
            _ <- disp (ToolCall "discover" (object ["query" .= ("gale" :: Text)]))
            forM_ events (disp . evCall)
            Right (ToolOk out) <-
                disp (ToolCall "discover" (object ["query" .= ("gust" :: Text)]))
            let expected = any (`elem` [NewDep, Restart]) events
            (events, not (T.null (textField "worldChange" out)))
                `shouldBe` (events, expected)
            Right (ToolOk out2) <-
                disp (ToolCall "discover" (object ["query" .= ("lull" :: Text)]))
            (events, textField "worldChange" out2) `shouldBe` (events, "")

producerCardSpec :: Spec
producerCardSpec = describe "empty miss with established target answers producers" $ do
    it "ranks producers first and reconciles counts over generated catalogues" $
        forM_ [0, 1, 5, 40 :: Int] $ \n ->
            forM_ [1 .. 8 :: Int] $ \limit -> do
                let sg = StandingGoal "Plot" "bars" "cumulus"
                    producers =
                        [ ("mk" <> tShow i, "Ctx -> Plot") | i <- [1 .. n]
                        ]
                    fillers =
                        [ ("other" <> tShow i, "Int -> Text") | i <- [1 .. 10 :: Int]
                        ]
                    exports = fillers ++ producers
                    v = fromMaybe (object []) (producerCard sg exports limit)
                    card = field "card" v
                    shownN = intField "shown" v
                    omittedN = intField "omitted" v
                    totalN = intField "total" v
                stateOf v `shouldBe` "found"
                fmap (textField "package") card `shouldBe` Just "cumulus"
                shownN + omittedN `shouldBe` totalN
                totalN `shouldBe` length exports
                shownN `shouldBe` min limit (length exports)
                envelopeChars v `shouldSatisfy` (<= envelopeCharBudget)
                let shownLines = cardExports v
                    isProducer l = "Plot" `T.isSuffixOf` l
                    afterFirstFiller =
                        dropWhile isProducer shownLines
                take (min limit n) shownLines
                    `shouldSatisfy` all isProducer
                afterFirstFiller `shouldSatisfy` (not . any isProducer)
    it "an empty catalogue yields no card (the honest miss stands)" $ do
        let sg = StandingGoal "Plot" "bars" "cumulus"
        producerCard sg [] 8 `shouldBe` Nothing
    it "the wired path answers the card on an established-target miss" $ do
        installNamesFile
        let goalArg =
                object
                    [ "type" .= ("Plot" :: Text)
                    , "consumer" .= ("bars" :: Text)
                    , "package" .= ("cumulus" :: Text)
                    ]
            args =
                object
                    ["query" .= ("defaultPlot" :: Text), "_goal" .= goalArg]
        v <- runCatArgs "defaultPlot" args
        stateOf v `shouldBe` "found"
        fmap (textField "package") (field "card" v) `shouldBe` Just "cumulus"
  where
    tShow = T.pack . show
    intField k v = case field k v of
        Just (Number x) -> round x :: Int
        _ -> -1
    cardExports v = case field "card" v >>= field "exports" of
        Just (Array a) -> [s | String s <- foldr (:) [] a]
        _ -> []
