{-# LANGUAGE OverloadedStrings #-}

{- | Acceptance for the rule that the harness never reports its own limits as
the candidate's defect. Each case is a row of the plan's case table.
-}
module Test.HarnessHonestyLiveSpec (spec) where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Notebook (execReadCell)
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.SessionTypes (CellLang (..))
import Sabela.State (App (..))
import Sabela.State.NotebookStore (modifyNotebook)
import Test.GateFixture (callTool, field, insertSrc, textField, withFixture)
import Test.Live (requireLiveIntegration)

spec :: Spec
spec = describe "the harness reports its own limits as its own" $ do
    it "case 8: a genuine diagnostic arrives without the harness's own frames" $ do
        requireLiveIntegration
        withFixture "sabela-honesty-scaffold" $ \(app, store, rn) -> do
            _ <- insertSrc app store rn "sabelaWarmup = (1 :: Int)"

            ack <-
                callTool
                    app
                    store
                    rn
                    "try"
                    (object ["code" .= ("length \"abc\" + True" :: T.Text)])

            let wire = T.pack (BL.unpack (encode ack))
            wire `shouldSatisfy` T.isInfixOf "Couldn't match expected type"
            wire `shouldNotSatisfy` T.isInfixOf "_sabelaCandidate"
            wire `shouldNotSatisfy` T.isInfixOf "ghciStepIO"
            wire `shouldNotSatisfy` T.isInfixOf "Data.Typeable"

    it "case 2: a value with no Show instance is not called a compile error" $ do
        requireLiveIntegration
        withFixture "sabela-honesty-unshowable" $ \(app, store, rn) -> do
            _ <- insertSrc app store rn "sabelaWarmup = (1 :: Int)"

            ack <-
                callTool
                    app
                    store
                    rn
                    "try"
                    (object ["code" .= ("(+1) :: Int -> Int" :: T.Text)])

            textField "type" ack `shouldBe` Just "Int -> Int"
            textField "verdict" ack `shouldBe` Just "ok"
            field "valueShown" ack `shouldBe` Just (Bool False)

            case textField "reason" ack of
                Just r -> r `shouldSatisfy` T.isInfixOf "Show"
                Nothing -> expectationFailure "expected a reason naming the instance"

            let wire = T.pack (BL.unpack (encode ack))
            wire `shouldNotSatisfy` T.isInfixOf "_sabelaCandidate"
            wire `shouldNotSatisfy` T.isInfixOf "take 4001"
            wire `shouldNotSatisfy` T.isInfixOf "No instance for"

    it "case 10: a goal type with no near miss still finds what fits it" $ do
        requireLiveIntegration
        withFixture "sabela-honesty-fits" $ \(app, store, rn) -> do
            _ <-
                insertSrc app store rn "import Sabela.Notebook\nsabelaPic = plot [(0,0),(1,1)]"

            ack <-
                callTool
                    app
                    store
                    rn
                    "find_by_type"
                    (object ["goal" .= ("_ :: Picture -> Picture -> Picture" :: T.Text)])

            let wire = T.pack (BL.unpack (encode ack))
            wire `shouldSatisfy` (\w -> T.isInfixOf "mappend" w || T.isInfixOf "<>" w)

    it "case 12: a bare underscore's fits are surfaced, not discarded" $ do
        requireLiveIntegration
        withFixture "sabela-honesty-bare-hole" $ \(app, store, rn) -> do
            _ <-
                insertSrc app store rn "import Sabela.Notebook\nsabelaS = plot [(0,0),(1,1)]"

            ack <-
                callTool
                    app
                    store
                    rn
                    "try"
                    (object ["code" .= ("displayPicture (_ sabelaS sabelaS)" :: T.Text)])

            case field "holeFits" ack of
                Just (Array fits) -> length fits `shouldSatisfy` (> 0)
                _ -> expectationFailure "expected the fits GHC already reported"

    it "case 10 end to end: reading the failing cell states the goal" $ do
        requireLiveIntegration
        withFixture "sabela-honesty-readcell" $ \(app, _store, _rn) -> do
            modifyNotebook (appNotebook app) $ \nb ->
                nb
                    { nbCells =
                        [ Cell
                            { cellId = 8
                            , cellType = CodeCell
                            , cellLang = Haskell
                            , cellSource = "displayPicture (combine s c)"
                            , cellOutputs = []
                            , cellError =
                                Just
                                    "cell 8, line 1: Variable not in scope: \
                                    \combine :: Picture -> Picture -> Picture"
                            , cellDirty = False
                            }
                        ]
                    }

            ack <- execReadCell app (object ["cell_id" .= (8 :: Int)])
            let v = toolOutcomeValue ack
            textField "goal" v
                `shouldBe` Just "combine :: Picture -> Picture -> Picture"
