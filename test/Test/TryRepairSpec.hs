{-# LANGUAGE OverloadedStrings #-}

{- | The repair the model actually meets. A failing trial and a refused write
put the same question to the same compiler, so they must carry the same answer
and cost the same one extra compile. Properties are over generated calls.
-}
module Test.TryRepairSpec (spec) where

import Data.Aeson (Value (..), encode, object)
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (Pair)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Sabela.AI.Capabilities.Edit.CompileGate (gateRepairPairs)
import Sabela.AI.Capabilities.Edit.HoleRewrite (holeName)
import Sabela.AI.Capabilities.Try.Candidate (ContainedRun)
import Sabela.AI.Capabilities.Try.Payload (disposablePayload)
import Sabela.AI.Capabilities.Try.Payload.Repair (tryRepairPairs)
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
 )
import Test.HoleAdapterGen (
    ArgSource (..),
    genArgSource,
    genFive,
    genThree,
    genTwo,
    mismatchDiag,
 )
import Test.HoleAdapterRig (convBlob, diagWith)
import Test.HoleRewriteGen (field, namedHoleBlob, scopeDiag)

spec :: Spec
spec = describe "the repair a rejected trial carries" $ do
    seamSpec
    costSpec
    guardSpec

-- One seam ------------------------------------------------------------------

seamSpec :: Spec
seamSpec = describe "one seam, both surfaces" $ do
    prop "a failing trial carries what the gate carries for the same rejection" $
        forAll genArgSource $ \as ->
            forAll genThree $ \(expected, actual, conv) -> monadicIO $ do
                let diagnostic = diagWith as expected actual
                    blob = convBlob actual expected conv
                (trial, _) <- run (countedRun blob)
                gatePairs <-
                    run
                        ( gateRepairPairs
                            (const (pure blob))
                            DisposableCompileError
                            diagnostic
                            (asSrc as)
                        )
                trialPairs <- run (trialRepair trial (asSrc as) diagnostic)
                assert (encode (object gatePairs) == encode (object trialPairs))

    prop "and the answer they agree on is a rewrite, not an empty list" $
        forAll genArgSource $ \as ->
            forAll genThree $ \(expected, actual, conv) -> monadicIO $ do
                (trial, _) <- run (countedRun (convBlob actual expected conv))
                pairs <- run (trialRepair trial (asSrc as) (diagWith as expected actual))
                assert (any (named "holeRewrite") pairs)

    prop "and both read the candidate's own knock-ons out of the rejection" $
        forAll genFive $ \(binder, fn, arg, conv, ghost) ->
            forAll genTwo $ \(expected, actual) -> monadicIO $ do
                let src = binder <> " = " <> fn <> " " <> arg <> "\n"
                    raw =
                        knockOnBlock binder ghost
                            <> "\n\n"
                            <> mismatchDiag expected actual fn 1 arg
                    blob = convBlob actual expected conv
                (trial, _) <- run (countedRun blob)
                gatePairs <-
                    run (gateRepairPairs (const (pure blob)) DisposableCompileError raw src)
                trialPairs <- run (tryRepairPairs trial src (failedWith raw))
                assert
                    ( encode (object gatePairs) == encode (object trialPairs)
                        && ghost `notElem` publishedWrites trialPairs
                    )

-- What it costs --------------------------------------------------------------

costSpec :: Spec
costSpec = describe "what the trial's repair costs" $ do
    prop "one extra contained compile on a rejection, counted not timed" $
        forAll genArgSource $ \as ->
            forAll genThree $ \(expected, actual, conv) -> monadicIO $ do
                (trial, count) <- run (countedRun (convBlob actual expected conv))
                _ <- run (trialRepair trial (asSrc as) (diagWith as expected actual))
                n <- run count
                assert (n == 1)

    prop "none at all on a trial the compiler did not refuse" $
        forAll genArgSource $ \as ->
            forAll genTwo $ \(expected, actual) ->
                forAll (elements otherVerdicts) $ \verdict -> monadicIO $ do
                    (trial, count) <- run (countedRun "")
                    pairs <-
                        run
                            ( tryRepairPairs
                                trial
                                (asSrc as)
                                (resultOf verdict (diagWith as expected actual))
                            )
                    n <- run count
                    assert (n == 0 && null pairs)

    prop "leaves a trial the compiler accepted with the payload it already had" $
        forAll genArgSource $ \as -> monadicIO $ do
            let green = (resultOf DisposableOk ""){disposableStdout = asArg as}
                already = encode (disposablePayload green)
            (trial, _) <- run (countedRun "")
            pairs <- run (tryRepairPairs trial (asSrc as) green)
            assert (null pairs && encode (disposablePayload green) == already)

-- When the caller already asked ----------------------------------------------

guardSpec :: Spec
guardSpec = describe "a trial that already asks the compiler a hole" $ do
    prop "is not rewritten when the compiler already answered a hole of its own" $
        forAll genArgSource $ \as ->
            forAll genThree $ \(expected, actual, ty) -> monadicIO $ do
                (trial, count) <- run (countedRun "")
                pairs <-
                    run
                        ( trialRepair
                            trial
                            (asSrc as)
                            (diagWith as expected actual <> ownHoleBlock ty)
                        )
                n <- run count
                assert (n == 0 && not (any (named "holeRewrite") pairs))

    prop "never hands back a source carrying the harness's own hole" $
        forAll genArgSource $ \as ->
            forAll genThree $ \(expected, actual, conv) -> monadicIO $ do
                (trial, _) <- run (countedRun (convBlob actual expected conv))
                pairs <- run (trialRepair trial (asSrc as) (diagWith as expected actual))
                assert (T.count holeName (rendered pairs) == 1)

    prop "earns no substituted hole where the candidate carries a bare one" $
        forAll genFive $ \(binder, other, ghost, ty, _) -> monadicIO $ do
            let src = binder <> " = " <> ghost <> "\n" <> other <> " = _\n"
            (trial, count) <- run (countedRun (namedHoleBlob "_" ty []))
            pairs <- run (trialRepair trial src (scopeDiag 88464 ghost))
            n <- run count
            assert (n == 0 && not (any (named "typeDirected") pairs))

-- Rig -------------------------------------------------------------------------

trialRepair :: ContainedRun -> Text -> Text -> IO [Pair]
trialRepair trial src diagnostic =
    tryRepairPairs trial src (failedWith diagnostic)

{- | What GHC prints about a later use of a definition it already rejected: a
block naming the candidate's own binder, carrying fits of its own. Neither
surface's repair may read it as a rejection to answer.
-}
knockOnBlock :: Text -> Text -> Text
knockOnBlock binder offered =
    "<interactive>:9:1: error: [GHC-88464]\n    Variable not in scope: "
        <> binder
        <> " :: Aa\n      Valid hole fits include\n        "
        <> offered
        <> " :: Aa -> Bb"

rendered :: [Pair] -> Text
rendered = T.pack . show . object

-- | Every name the payload's fit lists tell the caller to write.
publishedWrites :: [Pair] -> [Text]
publishedWrites pairs = writesUnder (object pairs) <> nested
  where
    nested = maybe [] writesUnder (field "holeRewrite" (object pairs))
    writesUnder v = case field "holeFits" v of
        Just (Array xs) ->
            [w | x <- foldr (:) [] xs, Just (String w) <- [field "write" x]]
        _ -> []

-- | What GHC prints about a hole the caller wrote, which the harness must leave.
ownHoleBlock :: Text -> Text
ownHoleBlock ty =
    "\n\n<interactive>:9:3: error: [GHC-88464]\n    \8226 Found hole: _ :: " <> ty

otherVerdicts :: [DisposableVerdict]
otherVerdicts =
    [ DisposableOk
    , DisposableRuntimeError
    , DisposableTimedOut
    , DisposableUnavailable
    ]

{- | A contained run that answers every compile with one blob and counts how
many it was asked for, so the seam's cost is recorded rather than timed.
-}
countedRun :: Text -> IO (ContainedRun, IO Int)
countedRun blob = do
    calls <- newIORef (0 :: Int)
    let contained _ = do
            atomicModifyIORef' calls (\n -> (n + 1, ()))
            pure (resultOf DisposableCompileError blob)
    pure (contained, readIORef calls)

named :: Text -> Pair -> Bool
named k = (== k) . Key.toText . fst

failedWith :: Text -> DisposableResult
failedWith = resultOf DisposableCompileError

resultOf :: DisposableVerdict -> Text -> DisposableResult
resultOf verdict blob =
    DisposableResult
        { disposableRoute = "disposable_scratch"
        , disposableVerdict = verdict
        , disposableType = Nothing
        , disposableStdout = ""
        , disposableStderr = blob
        , disposableFailure = Nothing
        , disposableReplayedCells = []
        , disposableSkippedCells = []
        , disposableDependencies = []
        }
