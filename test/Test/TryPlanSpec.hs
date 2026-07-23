{-# LANGUAGE OverloadedStrings #-}

module Test.TryPlanSpec (spec) where

import qualified Data.Text as T
import ScriptHs.Parser (CabalMeta (..))
import Test.Hspec

import Sabela.AI.Capabilities.TryPlan

spec :: Spec
spec = describe "try candidate planning" $ do
    it "keeps one plain expression eligible for the live fast path" $ do
        let Right plan = planTrial "sum liveSeed"
        trialExpression plan `shouldBe` Just "sum liveSeed"
        candidateNeedsDisposable plan `shouldBe` False

    it "routes imports, declarations, and full candidate metadata to scratch" $ do
        let src =
                T.unlines
                    [ "-- cabal: build-depends: text"
                    , "-- cabal: default-extensions: OverloadedStrings"
                    , "import qualified Data.Text as T"
                    , "answer = T.length \"forty-two\""
                    , "answer"
                    ]
            Right plan = planTrial src
        metaDeps (trialMeta plan) `shouldBe` ["text"]
        metaExts (trialMeta plan) `shouldBe` ["OverloadedStrings"]
        trialSetup plan `shouldSatisfy` T.isInfixOf "import qualified Data.Text as T"
        trialSetup plan `shouldSatisfy` T.isInfixOf "answer = T.length"
        trialExpression plan `shouldBe` Just "answer"
        candidateNeedsDisposable plan `shouldBe` True

    it "allows declarations-only code to be compile-checked without execution" $ do
        let Right plan = planTrial "answer :: Int\nanswer = 42"
        trialExpression plan `shouldBe` Nothing
        candidateNeedsDisposable plan `shouldBe` True

    it "rejects multiple prompt actions rather than executing the first one" $
        planTrial "print (1 :: Int)\nprint (2 :: Int)"
            `shouldBe` Left TrialMultipleExpressions

    it "does not reorder a declaration from after the candidate expression" $
        planTrial "answer\nanswer = (42 :: Int)"
            `shouldBe` Left TrialExpressionNotFinal

    it "rejects IO binds, GHCi commands, TH, FFI, unsafe purity escapes, and flags" $ do
        planTrial "x <- readFile \"secret\"" `shouldSatisfy` isEffectReject
        planTrial ":! touch escaped" `shouldSatisfy` isMetaReject
        planTrial "$(pure [])" `shouldSatisfy` isUnsafeReject
        planTrial "$compileTimeAction" `shouldSatisfy` isUnsafeReject
        planTrial "[compileTimeQuote|payload|]" `shouldSatisfy` isUnsafeReject
        planTrial "foreign import ccall unsafe \"getpid\" pid :: Int\npid"
            `shouldSatisfy` isUnsafeReject
        planTrial "foreign  import ccall unsafe \"getpid\" pid :: Int\npid"
            `shouldSatisfy` isUnsafeReject
        planTrial "unsafePerformIO (writeFile \"escaped\" \"x\")"
            `shouldSatisfy` isUnsafeReject
        planTrial "-- cabal: ghc-options: -fplugin Evil\n42"
            `shouldSatisfy` isUnsafeReject
        planTrial "{-# LANGUAGE Unsafe #-}\n42" `shouldSatisfy` isUnsafeReject
        planTrial "-- cabal: default-extensions: Trustworthy\n42"
            `shouldSatisfy` isUnsafeReject
        planTrial "{-# LANGUAGE NoSafe #-}\n42" `shouldSatisfy` isUnsafeReject
        planTrial "-- cabal: default-extensions: CPP\n42"
            `shouldSatisfy` isUnsafeReject
        planTrial "-- cabal: made-up: value\n42" `shouldSatisfy` isUnsafeReject
        planTrial
            "-- cabal: source-repository-package: https://example.invalid/p main\n42"
            `shouldSatisfy` isUnsafeReject
        planTrial "-- cabal: packages: ../outside\n42"
            `shouldSatisfy` isUnsafeReject

    it "does not confuse the ordinary application operator with a splice" $ do
        let Right plan = planTrial "sum $ map id [1 :: Int, 2]"
        trialExpression plan `shouldBe` Just "sum $ map id [1 :: Int, 2]"

isEffectReject :: Either TrialPlanError TrialPlan -> Bool
isEffectReject (Left (TrialEffectfulStatement _)) = True
isEffectReject _ = False

isMetaReject :: Either TrialPlanError TrialPlan -> Bool
isMetaReject (Left (TrialMetaCommand _)) = True
isMetaReject _ = False

isUnsafeReject :: Either TrialPlanError TrialPlan -> Bool
isUnsafeReject (Left (TrialUnsafeSyntax _)) = True
isUnsafeReject _ = False
