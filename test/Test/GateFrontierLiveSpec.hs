{-# LANGUAGE OverloadedStrings #-}

module Test.GateFrontierLiveSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.GateRepair.Candidates (
    missingModuleCandidates,
 )
import Sabela.AI.Capabilities.Edit.HoleRewrite (holeName)
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

{- | A head that is not in scope, applied to a value whose type the compiler
can see: the shape fix H turns into a question for GHC.
-}
holeHeadSrc :: T.Text
holeHeadSrc =
    "do\n\
    \  let alphaList = [1 :: Int, 2, 3]\n\
    \  print (notARealFunctionAnywhere alphaList)\n"

{- | The same shape with the result pinned, which is when GHC can name the
functions that inhabit the hole rather than only its type.
-}
pinnedHeadSrc :: T.Text
pinnedHeadSrc =
    "do\n\
    \  let alphaList = [1 :: Int, 2, 3]\n\
    \  print (notARealFunctionAnywhere alphaList + (0 :: Int))\n"

{- | The same shape with a hole the candidate wrote itself: the answer must be
read from the harness's own hole, and no line of GHC's prose may be offered as
a fit.
-}
ownHoleSrc :: T.Text
ownHoleSrc =
    "do\n\
    \  let alphaList = [1 :: Int, 2, 3]\n\
    \  print (1 + _)\n\
    \  print (notARealFunctionAnywhere alphaList + (0 :: Int))\n"

-- | Whether a fit is a name a caller could write, rather than quoted prose.
writable :: Value -> Bool
writable v = case textField "write" v of
    Just w -> not (T.null w) && T.all plain w
    Nothing -> False
  where
    plain c = c `notElem` (" |\8226:\8216\8217" :: String)

spec :: Spec
spec = describe "the gate answers with what is left, not what it hit first" $ do
    it "offers what GHC puts where a not-in-scope head stood" $ do
        requireLiveIntegration
        withFixture "sabela-gate-hole-rewrite" $ \(app, store, rn) -> do
            ack <- insertSrc app store rn holeHeadSrc

            textField "notCommitted" ack `shouldBe` Just "compile-gate"
            let rewrite = field "holeRewrite" ack
            (rewrite >>= textField "substituted")
                `shouldBe` Just "notARealFunctionAnywhere"
            (rewrite >>= textField "with") `shouldBe` Just holeName
            (rewrite >>= textField "holeType") `shouldSatisfy` (/= Nothing)
            textField "source" ack `shouldBe` Just holeHeadSrc

            cellCount app `shouldReturn` 0

    it "offers the fits when the hole's result type is pinned too" $ do
        requireLiveIntegration
        withFixture "sabela-gate-hole-fits" $ \(app, store, rn) -> do
            ack <- insertSrc app store rn pinnedHeadSrc

            textField "notCommitted" ack `shouldBe` Just "compile-gate"
            (field "holeRewrite" ack >>= textField "substituted")
                `shouldBe` Just "notARealFunctionAnywhere"
            case field "holeRewrite" ack >>= field "holeFits" of
                Just (Array fits) -> length fits `shouldSatisfy` (> 0)
                _ -> expectationFailure "expected GHC's fits for the rewritten head"

            cellCount app `shouldReturn` 0

    it "reads its own hole, not the one the candidate already carried" $ do
        requireLiveIntegration
        withFixture "sabela-gate-own-hole" $ \(app, store, rn) -> do
            ack <- insertSrc app store rn ownHoleSrc

            textField "notCommitted" ack `shouldBe` Just "compile-gate"
            let rewrite = field "holeRewrite" ack
            (rewrite >>= textField "substituted")
                `shouldBe` Just "notARealFunctionAnywhere"
            (rewrite >>= textField "holeType")
                `shouldSatisfy` maybe False (T.isInfixOf "->")
            case rewrite >>= field "holeFits" of
                Just (Array fits) ->
                    foldr (:) [] fits `shouldSatisfy` all writable
                _ -> pure ()

            cellCount app `shouldReturn` 0

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
