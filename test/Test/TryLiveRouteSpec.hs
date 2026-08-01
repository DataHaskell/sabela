{-# LANGUAGE OverloadedStrings #-}

{- | C1-14d: try's answer must not depend on which of its two backends served
the probe, and it must never assert that a containment backend is unavailable
when the same call reaches one.
-}
module Test.TryLiveRouteSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Sabela.AI.Capabilities.Try.Route (LiveDecision (..), liveDecision)
import Sabela.AI.Types (toolOutcomeValue)
import qualified Sabela.SessionTypes as ST

{- | Expressions the probe might carry, over installed and invented packages so
no arm of the router can be keyed on a library.
-}
expressionGrid :: [Text]
expressionGrid =
    [ "Data.Map.size <$> pure m"
    , "pure (1 :: Int)"
    , "length <$> readFile p"
    , "Data.Text.pack <$> getLine"
    , "Data.Aeson.encode <$> loadConfig"
    , "Zubu.Artefact.load \"artefact.zub\""
    ]

-- | The wordings GHCi's scratch probe uses to say "this is IO, I did not run it".
ioMarkers :: [Text]
ioMarkers =
    [ "scratch candidate is IO; it was not executed"
    , "SABELA_UNRESTRICTED_IO: candidate is IO"
    , "Scratch candidate is io"
    ]

otherErrors :: [Text]
otherErrors = ["", "Variable not in scope: foo", "Couldn't match expected type"]

verdicts :: [ST.PureEvalVerdict]
verdicts =
    [ ST.PureEvalSucceeded
    , ST.PureEvalUnshowable
    , ST.PureEvalRejected
    , ST.PureEvalRuntimeError
    , ST.PureEvalTimedOut
    , ST.PureEvalStale
    , ST.PureEvalInvariantFailed
    , ST.PureEvalUnavailable
    ]

result :: ST.PureEvalVerdict -> Text -> Text -> ST.PureEvalResult
result verdict typ err =
    ST.PureEvalResult
        { ST.pureEvalVerdict = verdict
        , ST.pureEvalGeneration = 0
        , ST.pureEvalInferredType = typ
        , ST.pureEvalOutput = ""
        , ST.pureEvalError = err
        , ST.pureEvalBindingsUnchanged = True
        , ST.pureEvalItUnchanged = True
        , ST.pureEvalRecovery = ST.PureEvalNoRecovery
        }

genResult :: Gen (Bool, ST.PureEvalResult)
genResult = do
    invariant <- arbitrary
    verdict <- elements verdicts
    typ <- elements ["Int", "IO ()", "IO [String]", ""]
    err <- elements (ioMarkers <> otherErrors)
    pure (invariant, result verdict typ err)

genIOResult :: Gen (Bool, ST.PureEvalResult)
genIOResult = do
    marker <- elements ioMarkers
    typ <- elements ["IO ()", "IO [String]", "IO Zubu.Artefact"]
    _ <- elements expressionGrid
    pure (True, result ST.PureEvalRejected typ marker)

renderedText :: LiveDecision -> Text
renderedText LiveEscalateContained = ""
renderedText (LiveAnswer out) = render (toolOutcomeValue out)
  where
    render (Object o) = T.unwords [render v | v <- KM.elems o]
    render (String s) = s
    render (Array a) = T.unwords (map render (foldr (:) [] a))
    render other = T.pack (show other)

routeOf :: LiveDecision -> Maybe Text
routeOf LiveEscalateContained = Nothing
routeOf (LiveAnswer out) = case toolOutcomeValue out of
    Object o -> case KM.lookup (Key.fromText "route") o of
        Just (String s) -> Just s
        _ -> Nothing
    _ -> Nothing

spec :: Spec
spec = describe "try's fast-path probe never speaks for the contained one" $ do
    prop "never claims a containment backend is unavailable" $
        forAll genResult $ \(invariant, r) ->
            let text = renderedText (liveDecision invariant r)
             in counterexample (T.unpack text) $
                    not ("no qualified containment backend" `T.isInfixOf` text)

    prop "escalates an IO rejection instead of answering it" $
        forAll genIOResult $ \(invariant, r) ->
            liveDecision invariant r === LiveEscalateContained

    prop "an IO rejection is never reported as an unavailable route" $
        forAll genIOResult $ \(invariant, r) ->
            routeOf (liveDecision invariant r) =/= Just "unavailable"

    prop "the decision does not depend on which IO wording came back" $
        forAll (elements ioMarkers) $ \a ->
            forAll (elements ioMarkers) $ \b ->
                liveDecision True (result ST.PureEvalRejected "IO ()" a)
                    === liveDecision True (result ST.PureEvalRejected "IO ()" b)

    it "still reports a genuine compile error from the live probe" $
        routeOf
            (liveDecision True (result ST.PureEvalRejected "" "Variable not in scope: foo"))
            `shouldBe` Just "pure_live"
