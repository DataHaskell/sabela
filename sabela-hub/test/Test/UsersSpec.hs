{-# LANGUAGE OverloadedStrings #-}

{- | User-role store specs: bootstrap admin seeding, identity normalization
at the lookup boundary, grant/revoke, disk reload, and hydrate hardening.
-}
module Test.UsersSpec (spec) where

import Control.Exception (SomeException, finally, try)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hub.OAuth (generateRandomToken)
import Hub.Types (isLowerHex)
import Hub.Users (
    emailHash,
    grantAdmin,
    isAdmin,
    newUserStore,
    revokeAdmin,
 )
import System.Directory (
    createDirectoryIfMissing,
    getTemporaryDirectory,
    removeDirectoryRecursive,
 )
import System.FilePath ((</>))
import Test.Hspec

-- | Run with a per-test temp dir, removed afterwards (parallel-safe).
withUsersDir :: (FilePath -> IO a) -> IO a
withUsersDir act = do
    base <- getTemporaryDirectory
    tok <- generateRandomToken
    let dir = base </> "sabela-users-test-" <> T.unpack (T.take 8 tok)
    act dir
        `finally` (try (removeDirectoryRecursive dir) :: IO (Either SomeException ()))

spec :: Spec
spec = describe "Hub.Users" $ do
    describe "emailHash" $ do
        it "is lowercase hex (passes the slug guard, no path chars)" $ do
            isLowerHex (emailHash "Alice@Example.com") `shouldBe` True
            emailHash "a.b@c.d" `shouldSatisfy` (not . T.any (`elem` ['@', '.', '/']))
        it "collapses casing to one identity" $
            emailHash "Alice@X.com" `shouldBe` emailHash "alice@x.com"
        it "distinct emails get distinct hashes (injective encoding)" $
            emailHash "ab@x" `shouldSatisfy` (/= emailHash "a.b@x")

    describe "bootstrap admin" $ do
        it "is ensured admin at hydrate" $ withUsersDir $ \dir -> do
            store <- newUserStore dir (Just "boot@example.com")
            isAdmin store "boot@example.com" `shouldReturn` True
        it "a mixed-case bootstrap still resolves after a mixed-case login" $
            withUsersDir $ \dir -> do
                store <- newUserStore dir (Just "  Boot@Example.COM ")
                isAdmin store "boot@example.com" `shouldReturn` True
                isAdmin store "BOOT@example.com" `shouldReturn` True
        it "a blank bootstrap writes no stray record" $ withUsersDir $ \dir -> do
            store <- newUserStore dir (Just "   ")
            store2 <- newUserStore dir Nothing
            isAdmin store "" `shouldReturn` False
            isAdmin store2 "" `shouldReturn` False
        it "re-grants the bootstrap admin even if previously revoked" $
            withUsersDir $ \dir -> do
                store <- newUserStore dir (Just "boot@x.com")
                revokeAdmin store "boot@x.com"
                store2 <- newUserStore dir (Just "boot@x.com")
                isAdmin store2 "boot@x.com" `shouldReturn` True

    describe "grant / revoke / reload" $ do
        it "grant then revoke round-trips" $ withUsersDir $ \dir -> do
            store <- newUserStore dir Nothing
            isAdmin store "a@x.com" `shouldReturn` False
            grantAdmin store "a@x.com"
            isAdmin store "a@x.com" `shouldReturn` True
            revokeAdmin store "a@x.com"
            isAdmin store "a@x.com" `shouldReturn` False
        it "grants normalize the email" $ withUsersDir $ \dir -> do
            store <- newUserStore dir Nothing
            grantAdmin store "  A@X.com "
            isAdmin store "a@x.com" `shouldReturn` True
        it "roles survive a reload from disk" $ withUsersDir $ \dir -> do
            store <- newUserStore dir Nothing
            grantAdmin store "a@x.com"
            store2 <- newUserStore dir Nothing
            isAdmin store2 "a@x.com" `shouldReturn` True

    describe "hydrate hardening" $ do
        it "ignores directory names that are not lowercase hex" $
            withUsersDir $ \dir -> do
                createDirectoryIfMissing True (dir </> "EVIL")
                BS.writeFile
                    (dir </> "EVIL" </> "meta")
                    (TE.encodeUtf8 "email=mallory@x\nisAdmin=true\ncreatedAt=2026-01-01\n")
                store <- newUserStore dir Nothing
                isAdmin store "mallory@x" `shouldReturn` False
        it "isAdmin requires exact-match true, not key presence" $
            withUsersDir $ \dir -> do
                let h = T.unpack (emailHash "sneaky@x.com")
                createDirectoryIfMissing True (dir </> h)
                BS.writeFile
                    (dir </> h </> "meta")
                    (TE.encodeUtf8 "email=sneaky@x.com\nisAdmin=yes\ncreatedAt=2026-01-01\n")
                store <- newUserStore dir Nothing
                isAdmin store "sneaky@x.com" `shouldReturn` False
