{-# LANGUAGE OverloadedStrings #-}

{- | C2-1a: a verdict may only be read from the channel that computed it.
A refused scratch cell produced no output; the compile gate's rejection echoes
the source back, and that source contains the marker's own pass token.
-}
module Test.MarkerEchoSpec (markerEchoSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.ToolName (ToolName (..), toolWireName)
import Sabela.AI.Types (ToolOutcome (..), toolOutcomeValue)
import Siza.Agent.Check (CheckResult (..), checkVerdict3With)
import Siza.Agent.VerifyTool (runVerifyCall)
import Test.TruthGen (genCheckOver, genGhcDiagnostic, genIdent)

markerEchoSpec :: Spec
markerEchoSpec =
    describe "marker echo: a rejection is not a verdict (C2-1a)" $ do
        it "never reads a pass off a rejection that echoes the source" $
            property $
                forAll episode $ \(name, check, diag) -> ioProperty $ do
                    (call, _) <- refusingKernel True name check diag
                    (r, _) <- checkVerdict3With call check
                    pure (r `shouldNotBe` CheckPassed)

        it "the verify tool never answers pass when its scratch cell is refused" $
            property $
                forAll episode $ \(name, check, diag) -> ioProperty $ do
                    (call, _) <- refusingKernel True name check diag
                    out <- runVerifyCall call check
                    pure (verdictOf out `shouldNotBe` "pass")

        it "classification does not depend on what the rejection payload says" $
            property $
                forAll episode $ \(name, check, diag) -> ioProperty $ do
                    (echoing, _) <- refusingKernel True name check diag
                    (silent, _) <- refusingKernel False name check diag
                    (a, _) <- checkVerdict3With echoing check
                    (b, _) <- checkVerdict3With silent check
                    pure (a `shouldBe` b)

        it "runs nothing against a cell that was never created" $
            property $
                forAll episode $ \(name, check, diag) -> ioProperty $ do
                    (call, tape) <- refusingKernel True name check diag
                    _ <- checkVerdict3With call check
                    seen <- readIORef tape
                    pure $ do
                        seen `shouldSatisfy` notElem (toolWireName ExecuteCell)
                        seen `shouldSatisfy` notElem (toolWireName DeleteCell)

        it "still reads a genuine pass off the executed cell's output" $
            property $
                forAll episode $ \(_, check, _) -> ioProperty $ do
                    let call = runningKernel "GRADE_PASS"
                    (r, _) <- checkVerdict3With call check
                    pure (r `shouldBe` CheckPassed)

        it "still reads a genuine failure off the executed cell's output" $
            property $
                forAll episode $ \(_, check, _) -> ioProperty $ do
                    let call = runningKernel "GRADE_FAIL"
                    (r, _) <- checkVerdict3With call check
                    pure (r `shouldBe` CheckFailed)

-- | A binding name, a check over it, and the diagnostic its rejection carries.
episode :: Gen (Text, Text, Text)
episode = do
    n <- genIdent
    c <- genCheckOver [n]
    (,,) n c <$> genGhcDiagnostic

{- | The compile gate's real rejection shape: it carries the submitted source
beside the diagnostic, which for a marker cell contains the pass token. The
notebook defines the name the check reads and types it, so the check clears
the vet and the only thing left to read is the rejection.
-}
refusingKernel ::
    Bool ->
    Text ->
    Text ->
    Text ->
    IO (ToolName -> Value -> IO (Either Text ToolOutcome), IORef [Text])
refusingKernel echoSource name check diag = do
    tape <- newIORef []
    let call tn args = do
            modifyIORef' tape (<> [toolWireName tn])
            pure $ case tn of
                InsertCell -> Right (ToolErr (rejection (argOf "source" args)))
                Try -> boolAnswer (if argOf "code" args == check then "True" else "False")
                ListCells -> Right (ToolOk (cellsDefining [name]))
                CheckType ->
                    Right (ToolOk (object ["result" .= (name <> " :: Int")]))
                _ -> Right (ToolOk (object []))
    pure (call, tape)
  where
    rejection src =
        object $
            [ "verdict" .= ("diagnostic" :: Text)
            , "notCommitted" .= ("compile-gate" :: Text)
            , "diagnostic" .= diag
            ]
                <> ["source" .= src | echoSource]

-- | A kernel that commits the marker cell and prints @grade@.
runningKernel :: Text -> ToolName -> Value -> IO (Either Text ToolOutcome)
runningKernel grade tn _ =
    pure $ case tn of
        InsertCell -> Right (ToolOk (object ["cellId" .= (1 :: Int)]))
        ExecuteCell ->
            Right
                ( ToolOk
                    ( object
                        [ "cellId" .= (1 :: Int)
                        , "ok" .= True
                        , "outputs"
                            .= [ object
                                    [ "oiMime" .= ("text/plain" :: Text)
                                    , "oiOutput" .= (grade <> "\n")
                                    ]
                               ]
                        ]
                    )
                )
        Try -> boolAnswer "True"
        _ -> Right (ToolOk (object []))

boolAnswer :: Text -> Either Text ToolOutcome
boolAnswer v =
    Right (ToolOk (object ["type" .= ("Bool" :: Text), "stdout" .= v]))

cellsDefining :: [Text] -> Value
cellsDefining ns =
    object ["cells" .= [object ["id" .= (1 :: Int), "defines" .= ns]]]

argOf :: Text -> Value -> Text
argOf k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
argOf _ _ = ""

verdictOf :: ToolOutcome -> Text
verdictOf out = case toolOutcomeValue out of
    Object o -> case KM.lookup (K.fromText "verdict") o of
        Just (String s) -> s
        _ -> ""
    _ -> ""
