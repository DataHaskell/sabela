{-# LANGUAGE OverloadedStrings #-}

module Test.FetchSpec (spec) where

import Control.Exception (toException)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Fetch (
    OverCap (..),
    drainBounded,
    firstLine,
    statusError,
    transportError,
 )
import Sabela.AI.GitHub (githubStatus)
import Sabela.AI.Sdist (sdistStatus)

spec :: Spec
spec = describe "bounded HTTP fetch" $ do
    describe "statusError" $ do
        it "passes a 2xx through to the body read" $
            statusError "Hackage" [] 200 `shouldBe` Nothing
        it "names the service on an unmapped failure status" $
            statusError "Hackage" [] 500
                `shouldBe` Just "Hackage returned HTTP 500"
        it "prefers the caller's ladder for a listed status" $
            statusError "X" [(404, "gone")] 404 `shouldBe` Just "gone"

    describe "the Hackage status ladder" $ do
        it "reads a 404 as a possibly-wrong version" $
            sdistStatus "aeson" "9.9" 404
                `shouldSatisfy` maybe
                    False
                    (T.isInfixOf "version may be wrong")
        it "reports any other failure as plain HTTP" $
            sdistStatus "aeson" "9.9" 503
                `shouldBe` Just "Hackage returned HTTP 503"
        it "lets a success through to the body" $
            sdistStatus "aeson" "9.9" 200 `shouldBe` Nothing

    describe "the GitHub status ladder" $ do
        it "reads 403 and 429 as rate limiting" $ do
            githubStatus 403
                `shouldSatisfy` maybe False (T.isInfixOf "rate-limited")
            githubStatus 429
                `shouldSatisfy` maybe False (T.isInfixOf "rate-limited")
        it "reads 404 as a missing repository, ref, or path" $
            githubStatus 404
                `shouldBe` Just "GitHub has no such repository, ref, or path"

    describe "transportError" $
        it "keeps the service and the first line of the reason" $
            transportError "GitHub" (toException (userError "boom"))
                `shouldBe` "GitHub request failed: user error (boom)"

    describe "drainBounded" $ do
        it "truncates at the cap when told to" $ do
            feeder <- chunkFeeder [BS.replicate 600 1, BS.replicate 600 2]
            r <- drainBounded 1000 TruncateAtCap feeder
            fmap BL.length r `shouldBe` Right 1000
        it "fails over the cap when told to" $ do
            feeder <- chunkFeeder [BS.replicate 600 1, BS.replicate 600 2]
            r <- drainBounded 1000 (FailOverCap "too big") feeder
            r `shouldBe` Left "too big"

    describe "firstLine" $
        it "keeps only the first line of a multi-line reason" $
            firstLine "boom\nstack" `shouldBe` "boom"

-- | Yields each chunk once, then empty — the shape http-client's brRead has.
chunkFeeder :: [BS.ByteString] -> IO (IO BS.ByteString)
chunkFeeder chunks = do
    ref <- newIORef chunks
    pure $ do
        cs <- readIORef ref
        case cs of
            [] -> pure BS.empty
            (c : rest) -> writeIORef ref rest >> pure c
