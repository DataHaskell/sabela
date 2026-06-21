{-# LANGUAGE OverloadedStrings #-}

{- | The saved @siza login@ token: the pure url-match + expiry decision
('evalStatus'), the JSON wire shape (pinned so a frontend/hub change is caught),
and a save→load→clear round-trip under a temporary @XDG_STATE_HOME@.
-}
module Test.HubTokenSpec (hubTokenSpec) where

import Control.Exception (SomeException, bracket, try)
import Data.Aeson (decode, encode)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Siza.HubToken (
    HubToken (..),
    TokenStatus (..),
    clearHubToken,
    evalStatus,
    loadHubToken,
    saveHubToken,
    statusForUrl,
 )
import System.Directory (
    createDirectoryIfMissing,
    getTemporaryDirectory,
    removeDirectoryRecursive,
 )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import Test.Hspec

at :: Integer -> UTCTime
at h = UTCTime (fromGregorian 2026 6 21) (secondsToDiffTime (h * 3600))

tok :: HubToken
tok = HubToken "https://hub.example.com" "sbl-abc" (at 12)

hubTokenSpec :: Spec
hubTokenSpec = do
    describe "Siza.HubToken.evalStatus" $ do
        it "is NoToken when nothing is saved" $
            evalStatus "https://hub.example.com" (at 1) Nothing `shouldBe` NoToken

        it "is Valid before expiry" $
            evalStatus "https://hub.example.com" (at 11) (Just tok)
                `shouldBe` Valid "sbl-abc"

        it "is Expired at or after the expiry time" $
            evalStatus "https://hub.example.com" (at 12) (Just tok) `shouldBe` Expired

        it "ignores a trailing slash on the URL" $
            evalStatus "https://hub.example.com/" (at 11) (Just tok)
                `shouldBe` Valid "sbl-abc"

        it "is NoToken for a token saved against a different hub" $
            evalStatus "https://other.example.com" (at 11) (Just tok) `shouldBe` NoToken

    describe "Siza.HubToken JSON" $
        it "round-trips and keeps the {url, token, expiresAt} shape" $ do
            decode (encode tok) `shouldBe` Just tok
            let s = encode tok
            decode s `shouldBe` Just tok

    describe "Siza.HubToken save/load/clear" $
        it "persists, reloads, and clears under XDG_STATE_HOME" $
            withTempState $ do
                saveHubToken tok
                loadHubToken `shouldReturn` Just tok
                statusForUrl "https://hub.example.com" `shouldReturn` Expired
                clearHubToken
                loadHubToken `shouldReturn` Nothing

{- | Run an action with @XDG_STATE_HOME@ pointed at a fresh temp dir, restoring
the original afterwards so other specs that read 'stateBase' are unaffected.
-}
withTempState :: IO a -> IO a
withTempState act =
    bracket acquire release (const act)
  where
    acquire = do
        old <- lookupEnv "XDG_STATE_HOME"
        base <- getTemporaryDirectory
        let dir = base </> "siza-hubtoken-test"
        _ <- try (removeDirectoryRecursive dir) :: IO (Either SomeException ())
        createDirectoryIfMissing True dir
        setEnv "XDG_STATE_HOME" dir
        pure (old, dir)
    release (old, dir) = do
        maybe (unsetEnv "XDG_STATE_HOME") (setEnv "XDG_STATE_HOME") old
        _ <- try (removeDirectoryRecursive dir) :: IO (Either SomeException ())
        pure ()
