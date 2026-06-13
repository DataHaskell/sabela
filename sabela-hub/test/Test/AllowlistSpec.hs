{-# LANGUAGE OverloadedStrings #-}

-- | Signup allowlist + verified-email JWT extraction specs.
module Test.AllowlistSpec (spec) where

import Control.Exception (SomeException, try)
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Hub.Allowlist (
    checkAllowed,
    isAllowed,
    parseAllowlist,
 )
import Hub.OAuth (extractVerifiedEmailFromJwt)
import Hub.Types (normalizeEmail)
import System.Directory (
    getTemporaryDirectory,
    removeFile,
 )
import System.FilePath ((</>))
import Test.Hspec

{- | header.payload.sig with a fixed header; the payload is base64url JSON.
No signature check — the token comes straight from Google over HTTPS.
-}
mkJwt :: Text -> Text
mkJwt payload = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9." <> payload <> ".sig"

-- {"email":"Alice@Example.com","email_verified":true}
verifiedPayload :: Text
verifiedPayload = "eyJlbWFpbCI6IkFsaWNlQEV4YW1wbGUuY29tIiwiZW1haWxfdmVyaWZpZWQiOnRydWV9"

-- {"email":"alice@example.com","email_verified":false}
unverifiedPayload :: Text
unverifiedPayload = "eyJlbWFpbCI6ImFsaWNlQGV4YW1wbGUuY29tIiwiZW1haWxfdmVyaWZpZWQiOmZhbHNlfQ"

-- {"email":"alice@example.com"}
missingClaimPayload :: Text
missingClaimPayload = "eyJlbWFpbCI6ImFsaWNlQGV4YW1wbGUuY29tIn0"

-- {"email":"alice@example.com","email_verified":"true"}
stringTruePayload :: Text
stringTruePayload = "eyJlbWFpbCI6ImFsaWNlQGV4YW1wbGUuY29tIiwiZW1haWxfdmVyaWZpZWQiOiJ0cnVlIn0"

spec :: Spec
spec = describe "Allowlist" $ do
    describe "normalizeEmail" $ do
        it "lowercases and trims" $
            normalizeEmail "  Alice@Example.COM " `shouldBe` "alice@example.com"

    describe "parseAllowlist / isAllowed" $ do
        let al =
                parseAllowlist
                    "# team\n\
                    \alice@example.com\n\
                    \\n\
                    \  Bob@Example.org  \n\
                    \@datahaskell.com\n"
        it "allows an exact email entry" $
            isAllowed al "alice@example.com" `shouldBe` True
        it "matches case-insensitively on both sides" $
            isAllowed al (normalizeEmail "BOB@example.ORG") `shouldBe` True
        it "allows any address on a domain entry" $
            isAllowed al "carol@datahaskell.com" `shouldBe` True
        it "does not treat a domain entry as a suffix match" $
            isAllowed al "mallory@evildatahaskell.com" `shouldBe` False
        it "denies an email not listed" $
            isAllowed al "mallory@example.net" `shouldBe` False
        it "denies everyone on an empty allowlist (fail closed)" $ do
            isAllowed (parseAllowlist "") "alice@example.com" `shouldBe` False
            isAllowed (parseAllowlist "# only comments\n") "alice@example.com"
                `shouldBe` False
        it "strips a trailing inline comment from an entry" $
            isAllowed
                (parseAllowlist "alice@example.com  # the boss\n")
                "alice@example.com"
                `shouldBe` True
        it "a bare @ line does not become an allow-all domain" $
            isAllowed (parseAllowlist "@\n") "anyone@anywhere.com" `shouldBe` False
        it "an @-less input never matches a domain entry" $
            isAllowed (parseAllowlist "@example.com\n") "noatsign" `shouldBe` False

    describe "checkAllowed (the IO decision the callback uses)" $ do
        it "an unset file means open signup (dev mode)" $
            checkAllowed Nothing "anyone@x.com" `shouldReturn` True
        it "a configured-but-missing file denies (fail closed)" $
            checkAllowed (Just "/nonexistent/sabela/allowlist") "alice@x.com"
                `shouldReturn` False
        it "a readable file allows a listed email and denies others" $ do
            base <- getTemporaryDirectory
            let path = base </> "sabela-allowlist-test"
            TIO.writeFile path "alice@x.com\n@team.com\n"
            checkAllowed (Just path) "Alice@X.com" `shouldReturn` True
            checkAllowed (Just path) "carol@team.com" `shouldReturn` True
            checkAllowed (Just path) "mallory@x.com" `shouldReturn` False
            _ <- try (removeFile path) :: IO (Either SomeException ())
            pure ()

    describe "extractVerifiedEmailFromJwt" $ do
        it "returns the email when email_verified is boolean true" $
            extractVerifiedEmailFromJwt (mkJwt verifiedPayload)
                `shouldBe` Right "Alice@Example.com"
        it "rejects when email_verified is false" $
            extractVerifiedEmailFromJwt (mkJwt unverifiedPayload)
                `shouldSatisfy` isLeft
        it
            "rejects when email_verified is missing (OIDC: trustworthy only when verified)"
            $ extractVerifiedEmailFromJwt (mkJwt missingClaimPayload)
                `shouldSatisfy` isLeft
        it "rejects a string \"true\" (the ID-token claim is boolean)" $
            extractVerifiedEmailFromJwt (mkJwt stringTruePayload)
                `shouldSatisfy` isLeft
