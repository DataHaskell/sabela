{-# LANGUAGE OverloadedStrings #-}

module Test.CompileGateWireSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.CompileGate (rejectionJson, submittedOnly)
import Sabela.Session.Materialize (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    MaterializeStage (..),
    disposableRouteName,
 )

reject ::
    Maybe Text ->
    Maybe Int ->
    Text ->
    [Text] ->
    DisposableResult ->
    Value
reject exposedBy mReplaces src = rejectionJson exposedBy mReplaces (submittedOnly src)

field :: Text -> Value -> Maybe Value
field key (Object obj) = KM.lookup (Key.fromText key) obj
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField key value = case field key value of
    Just (String text) -> Just text
    _ -> Nothing

baseResult :: DisposableResult
baseResult =
    DisposableResult
        { disposableRoute = disposableRouteName
        , disposableVerdict = DisposableCompileError
        , disposableType = Nothing
        , disposableStdout = ""
        , disposableStderr = ""
        , disposableFailure = Nothing
        , disposableReplayedCells = [0]
        , disposableSkippedCells = []
        , disposableDependencies = []
        }

holeDiagnostic :: Text
holeDiagnostic =
    "cell 1: Found hole: _ :: Point -> Point -> Picture\n\
    \Valid hole fits include\n\
    \  line :: Point -> Point -> Picture\n\
    \    with line @Point"

animateSrc :: Text
animateSrc =
    "import Sabela.Notebook (plot, animateWith)\n\
    \sinWavePicture t = plot [(t, sin t)]\n\
    \animateWith animCanvas defaultAnim sinWavePicture"

phantomDiag :: Text
phantomDiag =
    "<interactive>:245:17: error: [GHC-88464]\n\
    \    Variable not in scope: animCanvas :: AnimOpts\n\
    \\n\
    \<interactive>:245:40: error: [GHC-88464]\n\
    \    Variable not in scope: sinWavePicture :: Time -> Picture"

spec :: Spec
spec = describe "compile-gate rejection envelope wire pins" $ do
    describe "knock-on hygiene (live_gemma2 animate)" $ do
        it "drops phantom not-in-scope errors for names the candidate defines" $ do
            let result =
                    baseResult
                        { disposableFailure =
                            Just (MaterializeFailure StageCandidateTypecheck Nothing phantomDiag)
                        }
                v =
                    reject Nothing Nothing animateSrc [] result
            case textField "diagnostic" v of
                Nothing -> expectationFailure "no diagnostic"
                Just d -> do
                    d `shouldSatisfy` T.isInfixOf "animCanvas"
                    d `shouldSatisfy` (not . T.isInfixOf "not in scope: sinWavePicture")

        it "keeps the diagnostic whole when every error would be a knock-on" $ do
            let onlyPhantom =
                    "<interactive>:245:40: error: [GHC-88464]\n\
                    \    Variable not in scope: sinWavePicture :: Time -> Picture"
                result =
                    baseResult
                        { disposableFailure =
                            Just (MaterializeFailure StageCandidateTypecheck Nothing onlyPhantom)
                        }
                v =
                    reject Nothing Nothing animateSrc [] result
            textField "diagnostic" v `shouldBe` Just onlyPhantom

        it "notes names the replaced cell's previous source defined" $ do
            let xvDiag =
                    "<interactive>:238:32: error: [GHC-88464]\n\
                    \    Variable not in scope: xValues :: [Double]"
                result =
                    baseResult
                        { disposableFailure =
                            Just (MaterializeFailure StageCandidateTypecheck Nothing xvDiag)
                        }
                v =
                    reject
                        Nothing
                        (Just 3)
                        "animate 5 (\\t -> plot (zip xValues xValues))"
                        ["xValues", "numPoints"]
                        result
            case textField "note" v of
                Nothing -> expectationFailure "expected a replaced-definitions note"
                Just n -> do
                    n `shouldSatisfy` T.isInfixOf "xValues"
                    n `shouldSatisfy` T.isInfixOf "previous version"
                    n `shouldSatisfy` (not . T.isInfixOf "numPoints")

        it "carries guidance when a diagnose rule matches the rejection" $ do
            let hidden =
                    "Could not load module \8216Data.Text\8217.\n\
                    \It is a member of the hidden package \8216text-2.1.2\8217."
                result =
                    baseResult
                        { disposableFailure =
                            Just (MaterializeFailure StageCandidateSetup Nothing hidden)
                        }
                v =
                    reject Nothing Nothing "import Data.Text" [] result
            case field "guidance" v of
                Just (Array gs) -> length gs `shouldSatisfy` (>= 1)
                _ -> expectationFailure "expected a guidance array"

        it "does not tell a cell to declare a package it already declares" $ do
            let missing =
                    "<no location info>: error: [GHC-35235]\n\
                    \    Could not find module \8216Control.Algebra.State\8217."
                src =
                    "-- cabal: build-depends: bluefin\n\
                    \import Control.Algebra.State\n"
                result =
                    baseResult
                        { disposableFailure =
                            Just (MaterializeFailure StageCandidateSetup Nothing missing)
                        }
                v =
                    reject Nothing Nothing src [] result
                messages = case field "guidance" v of
                    Just (Array gs) ->
                        [ m
                        | Object g <- toList gs
                        , Just (String m) <- [KM.lookup "message" g]
                        ]
                    _ -> []
            messages `shouldSatisfy` (not . null)
            messages `shouldSatisfy` (not . any (T.isInfixOf "FIRST line"))
            messages `shouldSatisfy` any (T.isInfixOf "bluefin")
    it
        "compile error on an insert: no cellId, verdict/stage/diagnostic/source present"
        $ do
            let result =
                    baseResult
                        { disposableFailure =
                            Just (MaterializeFailure StageCandidateSetup Nothing "parse error on input")
                        }
                v =
                    reject Nothing Nothing "broken = " [] result
            textField "notCommitted" v `shouldBe` Just "compile-gate"
            field "cellId" v `shouldBe` Nothing
            textField "verdict" v `shouldBe` Just "diagnostic"
            textField "stage" v `shouldBe` Just "candidate_setup"
            textField "diagnostic" v `shouldBe` Just "parse error on input"
            textField "source" v `shouldBe` Just "broken = "
            field "holeFits" v `shouldBe` Nothing
            field "holeProbe" v `shouldBe` Nothing

    it "compile error on a replace: names the replaced cell" $ do
        let result =
                baseResult
                    { disposableFailure =
                        Just
                            ( MaterializeFailure
                                StageCandidateTypecheck
                                Nothing
                                "Variable not in scope: foo"
                            )
                    }
            v =
                reject Nothing (Just 3) "foo" [] result
        field "cellId" v `shouldBe` Just (Number 3)
        textField "stage" v `shouldBe` Just "candidate_typecheck"

    it "carries parsed hole fits alongside the raw diagnostic when GHC reports them" $ do
        let result =
                baseResult
                    { disposableFailure =
                        Just
                            (MaterializeFailure StageCandidateTypecheck Nothing holeDiagnostic)
                    }
            v =
                reject
                    Nothing
                    Nothing
                    "line (_ :: Point) (_ :: Point)"
                    []
                    result
        textField "diagnostic" v `shouldBe` Just holeDiagnostic
        case field "holeFits" v of
            Just (Array fits) -> length fits `shouldBe` 1
            _ -> expectationFailure "expected a non-empty holeFits array"

    it
        "G3: the rejection carries the probe conclusions harvested by this gate check"
        $ do
            let result =
                    baseResult
                        { disposableFailure =
                            Just
                                (MaterializeFailure StageCandidateTypecheck Nothing holeDiagnostic)
                        }
                v =
                    reject
                        Nothing
                        Nothing
                        "line (_ :: Point) (_ :: Point)"
                        []
                        result
            case field "holeProbe" v >>= field "facts" of
                Just (Array facts) -> length facts `shouldBe` 1
                _ -> expectationFailure "expected a holeProbe facts array"
            (field "holeProbe" v >>= field "provenance")
                `shouldBe` Just (String "via: hole-probe")

    it
        "an infra failure (build timeout) is the closed infra verdict, not a diagnostic"
        $ do
            let result =
                    baseResult
                        { disposableVerdict = DisposableTimedOut
                        , disposableFailure =
                            Just
                                (MaterializeFailure StageSession Nothing "trial exceeded its time budget")
                        }
                v =
                    reject Nothing Nothing "x = 1" [] result
            textField "verdict" v `shouldBe` Just "no-verdict-infra"
            case textField "error" v of
                Just msg -> msg `shouldSatisfy` (/= "")
                Nothing -> expectationFailure "expected an error message"

    it "an infra failure never claims the candidate was verified or committed" $ do
        let result = baseResult{disposableVerdict = DisposableUnavailable}
            v =
                reject Nothing Nothing "x = 1" [] result
        textField "verdict" v `shouldBe` Just "no-verdict-infra"
        textField "diagnostic" v
            `shouldBe` Just
                "The compile gate could not verify this write; nothing was committed."
