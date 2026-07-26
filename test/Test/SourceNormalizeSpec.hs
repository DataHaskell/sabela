{-# LANGUAGE OverloadedStrings #-}

module Test.SourceNormalizeSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isRight)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.Directory (doesFileExist, findExecutable)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Sabela.AI.Capabilities (executeTool)
import Sabela.AI.Capabilities.Try (trialPlanErrorText)
import Sabela.AI.Capabilities.TryPlan (TrialPlanError (..), planTrial)
import Sabela.AI.NormalizeGate (gatedNormalizeInsert, gatedRewrite)
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (ToolOutcome, toolOutcomeIsError, toolOutcomeValue)
import Sabela.Anthropic.Types (AnthropicConfig (..), newCancelToken)
import Sabela.Handlers (ReactiveNotebook, setupReactive)
import Sabela.Model (CellType (..))
import Sabela.Server (newApp)
import Sabela.Session.Project (buildTimeSupportDir)
import Sabela.State (App (..))

letSineCandidate :: Text
letSineCandidate =
    T.unlines
        [ "import Data.List (intercalate)"
        , "import Text.Printf"
        , "let w = 400 :: Double"
        , "    h = 200 :: Double"
        , "    n = 1000 :: Int"
        , "    xs = [fromIntegral i / fromIntegral n * 2*pi | i <- [0..n]]"
        , "    ys = map sin xs"
        , "    points = zip xs ys"
        , "    pathData = intercalate \" L\" $ map (\\(x,y) -> printf \"%.2f %.2f\" \
          \(x*w/(2*pi)) ((h/2)-(y*h/2))) points"
        , "    svg = concat [\"<svg xmlns='http://www.w3.org/2000/svg' width='\", \
          \show w, \"' height='\", show h, \"'>\""
        , "                 ,\"<path d='M\", pathData, \"' stroke='black' fill='none'/>\""
        , "                 ,\"</svg>\"]"
        , "length svg"
        ]

rawNewlineCandidate :: Text
rawNewlineCandidate =
    "import Data.List (intercalate)\n"
        <> "tag = \"</svg"
        <> "\n"
        <> ">\"\n"
        <> "\ndisplaySvg (intercalate \"\" [tag])\n"

stillUnterminatedCandidate :: Text
stillUnterminatedCandidate =
    "tag = \"</svg" <> "\n" <> "forgot the quote\n" <> "\ntag\n"

spuriousUnicodeCandidate :: Text
spuriousUnicodeCandidate =
    "tag = \"\\u003csvg\\u003e\"\ntag\n"

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField k v = case field k v of
    Just (String s) -> Just s
    _ -> Nothing

spec :: Spec
spec = describe "G7: one normalizer for every candidate source" $ do
    pureSpec
    integrationSpec

pureSpec :: Spec
pureSpec = describe "pure generator + gate behaviour" $ do
    describe "try and insert_cell judge byte-identical normalized source" $
        forM_ codeGrid $ \src ->
            it ("agrees on: " <> show (T.take 40 src)) $ do
                let (_, insertSrc', _) = gatedNormalizeInsert CodeCell src
                    (trySrc', _) = gatedRewrite src
                insertSrc' `shouldBe` trySrc'

    describe "the let-block candidate: planned either way" $ do
        it "planTrial accepts the raw shape (imports, decls, one expression)" $
            planTrial letSineCandidate `shouldSatisfy` isRight
        it "gatedRewrite desugars the let before try ever sees it" $ do
            let (normalized, notes) = gatedRewrite letSineCandidate
            normalized `shouldSatisfy` T.isInfixOf "w = 400 :: Double"
            normalized `shouldSatisfy` (not . T.isInfixOf "let w")
            notes `shouldSatisfy` any (T.isInfixOf "top-level `let")

    describe "raw-newline-string (regression, live_test5 turns 17-22)" $ do
        it "escapes the embedded raw newline and keeps the fix" $ do
            let (normalized, notes) = gatedRewrite rawNewlineCandidate
            normalized
                `shouldBe` "import Data.List (intercalate)\ntag = \"</svg\\n>\"\n\
                           \\ndisplaySvg (intercalate \"\" [tag])\n"
            notes `shouldSatisfy` any (T.isInfixOf "raw newline")
            notes `shouldSatisfy` any (T.isInfixOf "Build on the CURRENT source")

    describe "the raw-newline fix never makes source worse" $
        it "an unterminated literal stays unterminated: judged as written" $ do
            let (normalized, notes) = gatedRewrite stillUnterminatedCandidate
            normalized `shouldBe` stillUnterminatedCandidate
            notes `shouldSatisfy` any (T.isInfixOf "reverted")

    describe "spurious \\uXXXX escapes" $
        it "are rewritten to the characters they denote" $ do
            let (normalized, notes) = gatedRewrite spuriousUnicodeCandidate
            normalized `shouldBe` "tag = \"<svg>\"\ntag\n"
            notes `shouldSatisfy` any (T.isInfixOf "\\uXXXX")

    describe "message parity (task 4): try vs a committed cell's wider policy" $ do
        it "names the cell/try gap for a policy-only rejection" $
            trialPlanErrorText TrialMultipleExpressions
                `shouldSatisfy` T.isInfixOf "cells accept this; try does not"
        it "an effectful statement also gets the parity clause" $
            trialPlanErrorText (TrialEffectfulStatement "x <- readFile \"f\"")
                `shouldSatisfy` T.isInfixOf "cells accept this; try does not"
        it "a GHCi meta-command stays a plain refusal (no cell can hold one either)" $
            trialPlanErrorText (TrialMetaCommand ":!ls")
                `shouldSatisfy` (not . T.isInfixOf "cells accept this")

codeGrid :: [Text]
codeGrid =
    [ "let xs = [1,2,3]"
    , "let a = 1\nlet b = 2"
    , "let x = 1\n    y = 2\nz = x + y"
    , "x = 1\ny = 2"
    , "main = do\n  print 1"
    , "-- cabal: build-depends: text\nimport Data.Text (Text)\nx = (1 :: Int)"
    , letSineCandidate
    , rawNewlineCandidate
    , stillUnterminatedCandidate
    , spuriousUnicodeCandidate
    , ""
    ]

requireLiveIntegration :: IO ()
requireLiveIntegration = do
    cabal <- findExecutable "cabal"
    case cabal of
        Nothing -> pendingWith "cabal not found on PATH; skipping G7 integration"
        Just _ -> pure ()
    supportPresent <-
        doesFileExist (buildTimeSupportDir </> "sabela-notebook.cabal")
    if supportPresent
        then pure ()
        else pendingWith "sabela-notebook support source not on disk"

newFixture :: FilePath -> IO (App, AIStore.AIStore, ReactiveNotebook)
newFixture dir = do
    mgr <- newManager defaultManagerSettings
    app <- newApp dir Set.empty (Just mgr) Nothing [buildTimeSupportDir]
    rn <- setupReactive app
    let cfg =
            AnthropicConfig
                { acApiKey = ""
                , acModel = "placeholder"
                , acBaseUrl = "https://api.anthropic.com"
                }
    store <- AIStore.newAIStore cfg mgr
    pure (app, store, rn)

callTool ::
    App -> AIStore.AIStore -> ReactiveNotebook -> Text -> Value -> IO ToolOutcome
callTool app store rn name input = do
    ct <- newCancelToken
    executeTool app store rn ct name input

integrationSpec :: Spec
integrationSpec = describe "live dispatch: insert_cell / try agree post-G7" $ do
    it "raw-newline-string: normalizes, compiles, and commits via insert_cell" $ do
        requireLiveIntegration
        withSystemTempDirectory "sabela-g7-rawnewline" $ \dir -> do
            (app, store, rn) <- newFixture dir
            outcome <-
                callTool app store rn "insert_cell" (object ["source" .= rawNewlineCandidate])
            let ack = toolOutcomeValue outcome
            toolOutcomeIsError outcome `shouldBe` False
            field "cellId" ack `shouldSatisfy` (/= Nothing)
            textField "note" ack `shouldSatisfy` maybe False (T.isInfixOf "raw newline")

    it "the let-block candidate is accepted by try post-normalization" $ do
        requireLiveIntegration
        withSystemTempDirectory "sabela-g7-tryLet" $ \dir -> do
            (app, store, rn) <- newFixture dir
            outcome <- callTool app store rn "try" (object ["code" .= letSineCandidate])
            let out = toolOutcomeValue outcome
            toolOutcomeIsError outcome `shouldBe` False
            textField "normalized" out
                `shouldSatisfy` maybe False (T.isInfixOf "top-level `let")

    it "replace_cell_source discloses the same per-fix notes as insert_cell" $ do
        requireLiveIntegration
        withSystemTempDirectory "sabela-g7-replace" $ \dir -> do
            (app, store, rn) <- newFixture dir
            seedOutcome <-
                callTool
                    app
                    store
                    rn
                    "insert_cell"
                    (object ["source" .= ("seed = (1 :: Int)" :: Text)])
            cid <- case field "cellId" (toolOutcomeValue seedOutcome) of
                Just (Number cidN) -> pure (round cidN :: Int)
                _ ->
                    expectationFailure "insert_cell did not return a cellId" >> error "unreachable"
            replaceOutcome <-
                callTool
                    app
                    store
                    rn
                    "replace_cell_source"
                    (object ["cell_id" .= cid, "new_source" .= letSineCandidate])
            let out = toolOutcomeValue replaceOutcome
            toolOutcomeIsError replaceOutcome `shouldBe` False
            textField "note" out `shouldSatisfy` maybe False (T.isInfixOf "top-level `let")
