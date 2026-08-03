{-# LANGUAGE OverloadedStrings #-}

{- | An insert must never reach the notebook without a compile gate, whatever
state the notebook is in. A red notebook is when the gate matters most, so it
is exactly when it must not be skipped (LEDGER C2-8a), and a write rerouted
into a replace must disclose that whether it lands or is rejected (C2-8b/c).
-}
module Test.InsertGateRouteSpec (spec) where

import Data.Aeson (Value (..), object)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.Edit (
    InsertAttempt (..),
    InsertRoute (..),
    discloseSupersede,
    insertRetryFuel,
    insertRoute,
    nextInsertAttempt,
 )
import Sabela.AI.Capabilities.Edit.Ack (withNote)
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.Handlers (DefConflict (..), NotebookViolation (..))
import Test.HarnessGen (genIdent)

genCell :: Gen Text
genCell = do
    binders <- listOf1 genIdent
    pure (T.unlines [b <> " = " <> b <> "Impl" | b <- binders])

{- | A red cell that shares a binder with the candidate half the time, so both
the supersede and the refuse routes are exercised.
-}
genRedCell :: Text -> Gen (Int, Text)
genRedCell src = do
    cid <- choose (0, 40)
    share <- arbitrary
    other <- genCell
    pure (cid, if share then src else other)

genOutcome :: Gen ToolOutcome
genOutcome =
    elements
        [ ToolOk (object [])
        , ToolOk
            (Object (KM.singleton (Key.fromText "note") (String "kept the cabal line")))
        , ToolErr (object [])
        , ToolErr
            (Object (KM.singleton (Key.fromText "note") (String "kept the cabal line")))
        ]

noteOf :: ToolOutcome -> Maybe Text
noteOf out = case out of
    ToolOk v -> look v
    ToolErr v -> look v
  where
    look (Object o) = case KM.lookup (Key.fromText "note") o of
        Just (String s) -> Just s
        _ -> Nothing
    look _ = Nothing

spec :: Spec
spec = describe "every insert route is a gated one" $ do
    it
        "prop_appendOnlyFromACleanNotebook: the plain append route is reachable \
        \only when nothing is pending; a pending error always routes through \
        \the gated replace or a refusal"
        $ property
        $ forAll genCell
        $ \src ->
            forAll (oneof [pure Nothing, Just <$> genRedCell src]) $ \mRed ->
                case insertRoute mRed src of
                    RouteGateThenAppend -> mRed === Nothing
                    RouteSupersede cid -> fmap fst mRed === Just cid
                    RouteRefuse cid -> fmap fst mRed === Just cid

    it
        "prop_supersedeIsDisclosedOnEveryOutcome: a rerouted insert says so \
        \whether the replace landed or was rejected, and never destroys a note \
        \the replace had already set"
        $ property
        $ forAll ((,) <$> choose (0, 40) <*> genOutcome)
        $ \(cid, out) ->
            let out' = discloseSupersede cid out
                stated = fromMaybe "" (noteOf out')
             in conjoin
                    [ counterexample
                        "discloses the reroute"
                        (stated `shouldSatisfy'` T.isInfixOf (T.pack (show cid)))
                    , counterexample
                        "keeps the earlier note"
                        ( case noteOf out of
                            Nothing -> property True
                            Just earlier -> stated `shouldSatisfy'` T.isInfixOf earlier
                        )
                    ]

    it
        "prop_everyNoteSurvives: notes accumulate on any outcome, so a stacked \
        \disclosure is never destroyed by the next one"
        $ property
        $ forAll ((,) <$> listOf1 genNote <*> genOutcome)
        $ \(ns, out) ->
            let out' = foldl (flip withNote) out ns
                stated = fromMaybe "" (noteOf out')
             in conjoin
                    [ counterexample (T.unpack n) (stated `shouldSatisfy'` T.isInfixOf n)
                    | n <- ns
                    ]

    it
        "prop_theRedFlipRetryTerminates: a notebook that keeps turning red \
        \under the append cannot make the write loop"
        $ property
        $ forAll ((,) <$> choose (0, 20) <*> choose (0, 40))
        $ \(fuel, cid) ->
            let steps n
                    | n > insertRetryFuel + 21 = n
                    | otherwise = case nextInsertAttempt (fuelAt n) (VPendingError cid "red") of
                        RetryInsert _ -> steps (n + 1)
                        AbandonInsert _ -> n
                fuelAt n = fuel - n
             in steps 0 === fuel

    it "the fuel the live path starts with is finite" $
        insertRetryFuel `shouldSatisfy` (\n -> n >= 0 && n < 10)

    it "a violation that is not a pending error is never retried" $
        property $
            forAll (choose (0, 20)) $
                \fuel -> case nextInsertAttempt fuel (VDuplicateDef (DefConflict "x" 1)) of
                    AbandonInsert _ -> True
                    RetryInsert _ -> False
  where
    shouldSatisfy' x p = counterexample (show x) (p x)
    genNote = do
        n <- choose (1 :: Int, 999)
        w <- elements ["kept the cabal line", "reclassified as prose", "replaced cell"]
        pure (w <> " " <> T.pack (show n))
