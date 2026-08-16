{-# LANGUAGE OverloadedStrings #-}

{- | The probe glue over a stubbed session: claims exist only when a probe
verified the exact defining identity, and every seam transformer annotates
without touching unrelated text.
-}
module Test.TypeOriginProbeSpec (spec) where

import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.TypeOrigin (Namespace (..), OriginId (..))
import Sabela.AI.TypeOriginProbe (
    annotateDisposableWith,
    claimsWith,
    exportedByPairs,
    prefixCandidates,
 )
import Sabela.Session.Materialize (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    MaterializeStage (..),
    disposableRouteName,
 )

nbLine :: Text
nbLine =
    "\8216Data.Vector.Unboxed.Base.Vector\8217 is defined in \8216Data.Vector.Unboxed.Base\8217"

unboxedOrigin :: OriginId
unboxedOrigin = OriginId Nothing "Data.Vector.Unboxed.Base" "Vector" NsType

definedAt :: Text -> Maybe Text
definedAt m =
    Just ("data family Vector a\n  -- Defined in \8216" <> m <> "\8217")

result :: DisposableResult
result =
    DisposableResult
        { disposableRoute = disposableRouteName
        , disposableVerdict = DisposableCompileError
        , disposableType = Nothing
        , disposableStdout = ""
        , disposableStderr = nbLine
        , disposableFailure =
            Just (MaterializeFailure StageSession Nothing nbLine)
        , disposableReplayedCells = []
        , disposableSkippedCells = []
        , disposableDependencies = []
        }

spec :: Spec
spec = describe "facade probes over a stubbed session" $ do
    it "claims only the candidate whose probe reports the defining site" $ do
        let runner cand _ = pure $ case cand of
                "Data.Vector.Unboxed" -> definedAt "Data.Vector.Unboxed.Base"
                _ -> definedAt "Data.Vector"
            cands _ = pure ["Data.Vector", "Data.Vector.Unboxed"]
        claims <- claimsWith runner cands nbLine
        claims `shouldBe` [(unboxedOrigin, "Data.Vector.Unboxed")]
    it "prefers the closest of several verified facades" $ do
        let runner _ _ = pure (definedAt "Data.Vector.Unboxed.Base")
            cands _ = pure ["Data.Vector", "Data.Vector.Unboxed"]
        claims <- claimsWith runner cands nbLine
        map snd claims `shouldBe` ["Data.Vector.Unboxed"]
    it "claims nothing when the scope never establishes" $ do
        claims <-
            claimsWith
                (\_ _ -> pure Nothing)
                (\_ -> pure ["Data.Vector.Unboxed"])
                nbLine
        claims `shouldBe` []
    it "claims nothing without candidates, and probes no session" $ do
        calls <- newIORef (0 :: Int)
        let runner _ _ = modifyIORef' calls (+ 1) >> pure Nothing
        claims <- claimsWith runner (\_ -> pure []) nbLine
        claims `shouldBe` []
        readIORef calls `shouldReturn` 0
    it "never probes when the text names no impl-flavoured origin" $ do
        calls <- newIORef (0 :: Int)
        let runner _ _ = modifyIORef' calls (+ 1) >> pure Nothing
        claims <-
            claimsWith
                runner
                (\_ -> pure ["Data.Vector.Unboxed"])
                "\8216Vector\8217 is defined in \8216Data.Vector.Storable\8217"
        claims `shouldBe` []
        readIORef calls `shouldReturn` 0

    describe "prefixCandidates (discovery only)" $ do
        it "lists proper dotted ancestors, nearest first" $
            prefixCandidates "Data.Vector.Unboxed.Base"
                `shouldBe` ["Data.Vector.Unboxed", "Data.Vector"]
        it "offers nothing useful for a rootless internal module" $
            prefixCandidates "Internal.Matrix" `shouldBe` []

    describe "seam transformers" $ do
        let claims = [(unboxedOrigin, "Data.Vector.Unboxed")]
        it "annotates both the stderr and the failure message" $ do
            let r = annotateDisposableWith claims result
            disposableStderr r
                `shouldSatisfy` T.isSuffixOf "(exported by Data.Vector.Unboxed)"
            fmap failureMessage (disposableFailure r)
                `shouldSatisfy` maybe
                    False
                    (T.isSuffixOf "(exported by Data.Vector.Unboxed)")
        it "leaves a claimless result untouched" $
            annotateDisposableWith [] result `shouldBe` result
        it "renders the exportedBy payload object" $
            exportedByPairs claims
                `shouldBe` [
                               ( "exportedBy"
                               , object
                                    ["Vector" .= ("Data.Vector.Unboxed" :: Text)]
                               )
                           ]
        it "renders no pair without claims" $
            (exportedByPairs [] :: [Pair]) `shouldBe` []
