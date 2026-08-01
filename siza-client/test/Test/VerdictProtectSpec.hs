{-# LANGUAGE OverloadedStrings #-}

{- | A verdict the harness computed is never elided into a back-reference.
The protection keys on the stamp the message carries, so it holds for every
constructor on the verdict channel rather than for one spelling of one name.
-}
module Test.VerdictProtectSpec (verdictProtectSpec) where

import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Siza.Agent.EmitLedger (blockFloor, dedupInjected, newEmitLedger)
import Siza.Agent.Messages (
    doneSignalMsg,
    noCheckSignalMsg,
    unconfirmedMsgWith,
    verifyMsgWith,
 )
import Test.DiscoverFixtures (textField)
import Test.TruthGen (genIdent)

verdictProtectSpec :: Spec
verdictProtectSpec = describe "verdict blocks are elision-exempt" $ do
    it "repeats a verdict byte-complete however often it is emitted" $
        property $
            forAll verdictMessages $ \m -> ioProperty $ do
                led <- newEmitLedger
                outs <- mapM (\turn -> dedupInjected led turn [m]) [1 .. 3]
                let body = textField "content" m
                pure (map (textField "content") (concat outs) `shouldBe` replicate 3 body)

    it "emits verdict bodies long enough for elision to be a live risk" $
        property $
            forAll verdictMessages $ \m ->
                T.length (textField "content" m) `shouldSatisfy` (>= blockFloor)

{- | Every verdict-channel message, carrying a body over the elision floor —
so the property is about a block the ledger would otherwise rewrite.
-}
verdictMessages :: Gen Value
verdictMessages = do
    long <- longCheck
    n <- choose (1, 4)
    cids <- vectorOf n (choose (0, 40))
    missing <- listOf genIdent
    elements
        [ doneSignalMsg cids long
        , noCheckSignalMsg (Just long)
        , verifyMsgWith (length cids) missing (Just long)
        , unconfirmedMsgWith (length cids) [] (Just long)
        ]

-- | A conjunction long enough to clear 'blockFloor' on its own.
longCheck :: Gen Text
longCheck = do
    ns <- vectorOf 12 genIdent
    pure (T.intercalate " && " [n <> " == length (show " <> n <> ")" | n <- ns])
