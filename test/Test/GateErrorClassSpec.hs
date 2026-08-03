{-# LANGUAGE OverloadedStrings #-}

{- | A repair probe may only advance the frontier when it improved the
diagnostic within the class it targeted. Trading a scope error for a parse
error or a type error is not an improvement, however much the error count
falls (LEDGER C1-4e, C3-10).
-}
module Test.GateErrorClassSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.Edit.GateFrontier (
    Step (..),
    startFrontier,
    stepFrontier,
 )
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
 )
import Test.HarnessGen (
    genCellSource,
    genMissingModuleDiagnostic,
    genParseErrorDiagnostic,
    genScopeDiagnostic,
    genTypeErrorDiagnostic,
 )

failing :: Text -> DisposableResult
failing diag =
    DisposableResult
        "disposable_scratch"
        DisposableCompileError
        Nothing
        ""
        diag
        Nothing
        []
        []
        []

isAdvance :: Step -> Bool
isAdvance (Advance _) = True
isAdvance _ = False

spec :: Spec
spec = describe "the frontier only advances within the error class it targeted" $ do
    it
        "prop_noScopeForParseTrade: a probe that turns a scope error into a \
        \parse error never advances"
        $ property
        $ forAll
            ((,,) <$> genCellSource <*> genScopeDiagnostic <*> genParseErrorDiagnostic)
        $ \(src, was, now) ->
            forAll genCellSource $ \candidate ->
                not
                    ( isAdvance
                        ( stepFrontier
                            (startFrontier src src (failing was))
                            (candidate, ["r"])
                            (failing now)
                        )
                    )

    it
        "prop_noScopeForTypeTrade: a probe that clears a scope error by \
        \introducing a type error never advances"
        $ property
        $ forAll
            ((,,) <$> genCellSource <*> genScopeDiagnostic <*> genTypeErrorDiagnostic)
        $ \(src, was, now) ->
            forAll genCellSource $ \candidate ->
                not
                    ( isAdvance
                        ( stepFrontier
                            (startFrontier src src (failing was))
                            (candidate, ["r"])
                            (failing now)
                        )
                    )

    it
        "prop_unmaskingStillAdvances: clearing a missing-module error advances \
        \even though the errors it was hiding then surface"
        $ property
        $ forAll
            ((,,) <$> genCellSource <*> genMissingModuleDiagnostic <*> genTypeErrorDiagnostic)
        $ \(src, was, now) ->
            forAll genCellSource $ \candidate ->
                isAdvance
                    ( stepFrontier
                        (startFrontier src src (failing was))
                        (candidate, ["dep"])
                        (failing now)
                    )
