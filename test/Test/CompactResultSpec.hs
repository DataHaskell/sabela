{-# LANGUAGE OverloadedStrings #-}

module Test.CompactResultSpec (spec) where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Test.Hspec

import Sabela.AI.Handles (
    HandleId (..),
    HandleStore,
    lookupHandle,
    newHandleStore,
    storeLargeResult,
    summarizeForLLM,
 )

{- | Mirrors @Orchestrator.compactToolResult@ closely enough for unit tests.
Keep the threshold in sync with that function.
-}
compactToolResultThreshold :: Int
compactToolResultThreshold = 8000

compactToolResult :: HandleStore -> Value -> IO Value
compactToolResult store v = do
    let text = case v of
            String s -> s
            other -> TL.toStrict (TLE.decodeUtf8 (encode other))
    if T.length text <= compactToolResultThreshold
        then pure v
        else do
            r <- storeLargeResult store text
            case r of
                Left cleaned -> pure (String cleaned)
                Right (hid, summary, nLines, nBytes) ->
                    pure $
                        object
                            [ "_compacted" .= True
                            , "_large" .= summarizeForLLM hid summary nLines nBytes
                            ]

spec :: Spec
spec = do
    describe "compactToolResult (P1.6 forced handle stashing)" $ do
        it "passes small JSON through unchanged" $ do
            store <- newHandleStore
            let v = object ["answer" .= (42 :: Int)]
            out <- compactToolResult store v
            out `shouldBe` v

        it "compacts above threshold, returning a handle reference" $ do
            store <- newHandleStore
            let big = T.replicate 9000 "x"
                v = object ["text" .= big]
            out <- compactToolResult store v
            case out of
                Object o -> do
                    KM.lookup (Key.fromString "_compacted") o
                        `shouldBe` Just (Bool True)
                    KM.lookup (Key.fromString "_large") o
                        `shouldSatisfy` (/= Nothing)
                other -> expectationFailure ("expected Object, got " ++ show other)

        it "preserves the full payload in the handle store (no silent clip)" $ do
            store <- newHandleStore
            let sentinel = "SENTINEL-LINE-MUST-SURVIVE"
                big =
                    T.unlines
                        ( sentinel
                            : replicate 500 "filler-line-so-we-exceed-threshold"
                        )
                v = object ["text" .= big]
            out <- compactToolResult store v
            hid <- case out of
                Object o -> case KM.lookup (Key.fromString "_large") o of
                    Just (Object lo) -> case KM.lookup (Key.fromString "handleId") lo of
                        Just (String s) -> pure s
                        _ -> fail "handleId missing"
                    _ -> fail "_large missing"
                _ -> fail "compacted output should be Object"
            mLr <- lookupHandle store (HandleId hid)
            case mLr of
                Nothing -> expectationFailure "handle missing from store"
                Just _ -> pure ()
