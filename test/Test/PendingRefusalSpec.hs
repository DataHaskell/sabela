{-# LANGUAGE OverloadedStrings #-}

{- | R10-T2 pending-error refusal invariants: over a controlled set of red-cell
diagnostics the 'VPendingError' envelope decodes against the ONE write-ack
refusal schema, stays under the 2,500-byte budget, NEVER embeds the raw
multi-line GHC error, and names exactly the two legal edits
(@replace_cell_source@ | @delete_cell@). When an unchecked mechanical candidate
exists for the blocking cell it is carried as @suggestedSource@ with its honest
verification status, and no
channel instructs a fresh search (R5.7/R5.8). Pure; no kernel needed.
-}
module Test.PendingRefusalSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), encode)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Sabela.AI.Capabilities.Edit.Admit (
    holeBodyCompletion,
    pendingErrorFor,
    violationJson,
 )
import Sabela.AI.LeakShape (controlCharred, embeddedSerialisation)
import Sabela.AI.WriteAck (
    AckEnvelope (..),
    RefusalAck (..),
    parseAckEnvelope,
    pendingErrorAck,
 )
import Sabela.Handlers (NotebookViolation (..))

{- | A raw multi-line GHC error blob — the exact text an insert refusal must
NEVER re-ship downstream (R3.10).
-}
rawGhcError :: Text
rawGhcError =
    "<interactive>:4:9: error: [GHC-88464]\n\
    \    Variable not in scope: defPlot :: Plot\n\
    \    Suggested fix: Perhaps use ‘replot’ (imported from Graphics.X)\n\
    \    In the expression: defPlot"

-- | A controlled set of red-cell diagnostics: (red-cell source, blocking id).
diagnostics :: [(Text, Int)]
diagnostics =
    [ ("defPlot :: Plot", 1)
    , ("bars :: Text", 3)
    , ("main = T.unpack x", 7)
    , ("x = 1 +", 4)
    , ("data Foo = Foo { bad", 9)
    , ("f :: Int -> Int\nf x = undefinedName x", 2)
    ]

serialisedBytes :: Value -> Int
serialisedBytes = fromIntegral . LBS.length . encode

encText :: Value -> Text
encText = TE.decodeUtf8 . LBS.toStrict . encode

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

textOf :: Value -> Text
textOf (String s) = s
textOf _ = ""

errorText :: Value -> Text
errorText = maybe "" textOf . field "error"

-- | Phrases the refusal must never carry: steering the model back to search.
searchSteers :: [Text]
searchSteers = ["discover", "search again", "find_by_type", "rephrase", "api_reference"]

spec :: Spec
spec = describe "R10-T2 pending-error refusal invariants" $ do
    describe "GENERAL-INVARIANT over the controlled diagnostic set" $ do
        forM_ diagnostics $ \(src, cid) -> do
            let envNoCand = violationJson (VPendingError cid rawGhcError)
                envForCell = pendingErrorFor cid src

            it ("decodes on the ONE refusal schema, cell " <> show cid) $
                forM_ [envNoCand, envForCell] $ \env ->
                    case parseAckEnvelope env of
                        Just (EnvRefusal ra) ->
                            (raKind ra, raCell ra) `shouldBe` ("pending-error", Just cid)
                        other ->
                            expectationFailure ("not a pending refusal: " <> show other)

            it ("stays under the 2,500-byte budget, cell " <> show cid) $
                forM_ [envNoCand, envForCell] $ \env ->
                    serialisedBytes env `shouldSatisfy` (<= 2500)

            it ("NEVER embeds the raw multi-line GHC error, cell " <> show cid) $ do
                errorText envNoCand `shouldNotSatisfy` T.isInfixOf "GHC-88464"
                errorText envNoCand `shouldNotSatisfy` T.isInfixOf "Variable not in scope"
                encText envNoCand `shouldNotSatisfy` T.isInfixOf rawGhcError

            it ("names exactly the two legal edits, cell " <> show cid) $
                forM_ [envNoCand, envForCell] $ \env -> do
                    errorText env `shouldSatisfy` T.isInfixOf "replace_cell_source"
                    errorText env `shouldSatisfy` T.isInfixOf "delete_cell"

            it ("carries no leak shape (control chars / serialisation), cell " <> show cid) $
                forM_ [envNoCand, envForCell] $ \env -> do
                    controlCharred (encText env) `shouldBe` False
                    embeddedSerialisation (errorText env) `shouldBe` False

    describe "R5.7/R5.8: an unchecked candidate proposal is labelled honestly" $ do
        let cand = holeBodyCompletion ["defPlot"] "defPlot :: Plot"
            env = pendingErrorAck 1 (Just cand)

        it "the bodiless-signature red cell yields a suggestedSource candidate" $
            field "suggestedSource" (pendingErrorFor 1 "defPlot :: Plot")
                `shouldBe` Just (String cand)

        it "carries the candidate verbatim as suggestedSource" $
            field "suggestedSource" env `shouldBe` Just (String cand)

        it "labels the bodiless-signature completion as unchecked" $
            field "suggestedSourceStatus" env `shouldBe` Just (String "unchecked")

        it "does not claim the mechanical proposal was already checked" $ do
            let msg = T.toLower (errorText env)
            msg `shouldSatisfy` T.isInfixOf "repair proposal"
            msg `shouldSatisfy` T.isInfixOf "compiler will check"
            msg `shouldNotSatisfy` T.isInfixOf "checked fix"

        it "never instructs a fresh search when a candidate is ready (R5.8)" $ do
            let msg = T.toLower (errorText env)
            forM_ searchSteers $ \s ->
                (s, T.isInfixOf s msg) `shouldBe` (s, False)

        it "a non-bodiless red cell carries no candidate (honest Nothing)" $
            field "suggestedSource" (pendingErrorFor 3 "x = 1 +")
                `shouldBe` Nothing
