{-# LANGUAGE OverloadedStrings #-}

module Test.GateFrontierLiveSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.GateRepair.Candidates (
    missingModuleCandidates,
 )
import Sabela.AI.Capabilities.Try (execTry)
import Sabela.AI.Types (toolOutcomeValue)
import Test.GateFixture (
    cellCount,
    field,
    insertSrc,
    textField,
    withFixture,
 )
import Test.Live (requireLiveIntegration)

{- | The cell shape the gate used to answer badly: a module it cannot resolve
masks a body error, and only the mask was ever reported.
-}
maskedSrc :: T.Text
maskedSrc =
    "import Data.List.Split (splitOn)\n\
    \parts = splitOn \",\" \"a,b,c\"\n\
    \bogus = notARealFunctionAnywhere parts"

requireStorePackage :: IO ()
requireStorePackage = do
    cs <-
        missingModuleCandidates
            "Could not find module \8216Data.List.Split\8217."
            "import Data.List.Split (splitOn)"
    case cs of
        [] -> pendingWith "split is not in the local cabal store; skipping"
        _ -> pure ()

{- | Independent bindings, exactly one of which does not compile. The frontier
must cut the candidate back to the part that did rather than reject it whole.
-}
localiseSrc :: T.Text
localiseSrc =
    "alpha = length [1 :: Int, 2, 3]\n\
    \\n\
    \beta = notARealFunctionAnywhere alpha\n"

spec :: Spec
spec = describe "the gate answers with what is left, not what it hit first" $ do
    it "names the component a rejected trial stopped compiling at" $ do
        requireLiveIntegration
        withFixture "sabela-try-localise" $ \(app, _, _) -> do
            v <- toolOutcomeValue <$> execTry app (object ["code" .= localiseSrc])
            let loc = field "localisation" v
            (loc >>= textField "firstFailingComponent") `shouldBe` Just "beta"
            (loc >>= field "provenComponents")
                `shouldBe` Just (toJSON (["alpha"] :: [T.Text]))
            cellCount app `shouldReturn` 0

    it "declares the dependency, then reports the error that was behind it" $ do
        requireLiveIntegration
        requireStorePackage
        withFixture "sabela-gate-frontier" $ \(app, store, rn) -> do
            ack <- insertSrc app store rn maskedSrc

            textField "notCommitted" ack `shouldBe` Just "compile-gate"
            textField "verdict" ack `shouldBe` Just "diagnostic"

            case field "partialRepair" ack >>= field "applied" of
                Just (Array applied) ->
                    T.concat [a | String a <- foldr (:) [] applied]
                        `shouldSatisfy` T.isInfixOf "build-depends: split"
                _ -> expectationFailure "expected the proven dependency fix to be named"

            textField "source" ack `shouldBe` Just maskedSrc
            textField "compiledSource" ack
                `shouldSatisfy` maybe False (T.isInfixOf "build-depends: split")
            textField "diagnostic" ack
                `shouldSatisfy` maybe False (T.isInfixOf "notARealFunctionAnywhere")
            textField "diagnostic" ack
                `shouldSatisfy` maybe False (not . T.isInfixOf "Could not find module")

            cellCount app `shouldReturn` 0
