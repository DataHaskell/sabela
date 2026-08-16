{-# LANGUAGE OverloadedStrings #-}

{- | The verified-façade facts on the compile-gate rejection wire: the
in-line exported-by annotation reaches the diagnostic field, and a claimless
rejection is byte-identical to before the feature existed.
-}
module Test.TypeOriginWireSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.CompileGate (rejectionJson, submittedOnly)
import Sabela.AI.TypeOrigin (Namespace (..), OriginId (..))
import Sabela.AI.TypeOriginProbe (annotateDisposableWith)
import Sabela.Session.Materialize (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    MaterializeStage (..),
    disposableRouteName,
 )

reject :: Text -> DisposableResult -> Value
reject src = rejectionJson Nothing Nothing (submittedOnly src) []

textField :: Text -> Value -> Maybe Text
textField key value = case value of
    Object obj -> case KM.lookup (Key.fromText key) obj of
        Just (String text) -> Just text
        _ -> Nothing
    _ -> Nothing

nbDiag :: Text
nbDiag =
    "cell 1: Couldn't match type: Data.Vector.Unboxed.Base.Vector Double\n\
    \                 with: Vector Double\n\
    \      NB: \8216Vector\8217 is defined in \8216Data.Vector.Storable\8217\n\
    \          \8216Data.Vector.Unboxed.Base.Vector\8217\n\
    \            is defined in \8216Data.Vector.Unboxed.Base\8217"

claims :: [(OriginId, Text)]
claims =
    [
        ( OriginId
            (Just "vector-0.13.2.0")
            "Data.Vector.Unboxed.Base"
            "Vector"
            NsType
        , "Data.Vector.Unboxed"
        )
    ]

failing :: DisposableResult
failing =
    DisposableResult
        { disposableRoute = disposableRouteName
        , disposableVerdict = DisposableCompileError
        , disposableType = Nothing
        , disposableStdout = ""
        , disposableStderr = ""
        , disposableFailure =
            Just (MaterializeFailure StageCandidateTypecheck Nothing nbDiag)
        , disposableReplayedCells = [0]
        , disposableSkippedCells = []
        , disposableDependencies = []
        }

srcText :: Text
srcText =
    "import qualified Data.Vector.Storable as V\n\
    \colsLists = map V.toList vecs"

spec :: Spec
spec = describe "verified facade facts on the rejection wire" $ do
    it "the rejection's diagnostic carries the exported-by fact in-line" $ do
        let v = reject srcText (annotateDisposableWith claims failing)
        case textField "diagnostic" v of
            Nothing -> expectationFailure "no diagnostic"
            Just d -> do
                d
                    `shouldSatisfy` T.isInfixOf
                        "\8216Data.Vector.Unboxed.Base\8217 (exported by vector:Data.Vector.Unboxed)"
                d `shouldSatisfy` (not . T.isInfixOf "Storable\8217 (exported by")
    it "an unannotated rejection is byte-identical to before" $
        reject srcText (annotateDisposableWith [] failing)
            `shouldBe` reject srcText failing
