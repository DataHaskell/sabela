{-# LANGUAGE OverloadedStrings #-}

{- | The request headers siza sends to @/api/ai/*@. The hub gates proxied
requests on the @_sabela_session@ OAuth cookie and strips @Authorization@
before forwarding, so driving a hub-hosted notebook needs a @Cookie:@ header
carried through 'SABELA_COOKIE'. This pins that the cookie is emitted when set,
the session header is always present (the hub does NOT strip @X-Sabela-Session@),
and the bearer token is still emitted for the localhost trust model.
-}
module Test.TransportSpec (transportSpec) where

import Control.Exception (bracket)
import Siza.Transport (Env (..), aiHeaders, applyUrlOverride, resolveEnv)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

-- | Run an env-mutating test, restoring @SABELA_URL@ afterwards.
withSabelaUrl :: IO a -> IO a
withSabelaUrl =
    bracket (lookupEnv "SABELA_URL") restore . const
  where
    restore = maybe (unsetEnv "SABELA_URL") (setEnv "SABELA_URL")

baseEnv :: Env
baseEnv =
    Env
        { envSabelaUrl = Nothing
        , envToken = Nothing
        , envSession = "siza-host-1"
        , envCookie = Nothing
        , envToolTimeout = 60
        }

transportSpec :: Spec
transportSpec = describe "Siza.Transport.aiHeaders" $ do
    it "always sends content-type and the session header" $ do
        let hs = aiHeaders baseEnv
        lookup "content-type" hs `shouldBe` Just "application/json"
        lookup "X-Sabela-Session" hs `shouldBe` Just "siza-host-1"

    it "omits Authorization and Cookie when neither is set" $ do
        let hs = aiHeaders baseEnv
        lookup "Authorization" hs `shouldBe` Nothing
        lookup "Cookie" hs `shouldBe` Nothing

    it "sends the bearer token when SABELA_AI_TOKEN is set" $ do
        let hs = aiHeaders baseEnv{envToken = Just "tok"}
        lookup "Authorization" hs `shouldBe` Just "Bearer tok"

    it "sends the session cookie when SABELA_COOKIE is set (hub path)" $ do
        let hs = aiHeaders baseEnv{envCookie = Just "_sabela_session=abc"}
        lookup "Cookie" hs `shouldBe` Just "_sabela_session=abc"

    it "can carry both cookie and bearer at once" $ do
        let hs =
                aiHeaders
                    baseEnv{envCookie = Just "_sabela_session=abc", envToken = Just "tok"}
        lookup "Cookie" hs `shouldBe` Just "_sabela_session=abc"
        lookup "Authorization" hs `shouldBe` Just "Bearer tok"

    describe "applyUrlOverride (--url is the CLI face of SABELA_URL)" $ do
        it "makes --url visible to resolveEnv, so the hub token attaches" $
            withSabelaUrl $ do
                unsetEnv "SABELA_URL"
                applyUrlOverride (Just "http://flag:3000")
                env <- resolveEnv
                envSabelaUrl env `shouldBe` Just "http://flag:3000"

        it "leaves an existing SABELA_URL alone when no --url is given" $
            withSabelaUrl $ do
                setEnv "SABELA_URL" "http://env:3000"
                applyUrlOverride Nothing
                env <- resolveEnv
                envSabelaUrl env `shouldBe` Just "http://env:3000"

        it "lets --url win over SABELA_URL (one knob, CLI first)" $
            withSabelaUrl $ do
                setEnv "SABELA_URL" "http://env:3000"
                applyUrlOverride (Just "http://flag:3000")
                env <- resolveEnv
                envSabelaUrl env `shouldBe` Just "http://flag:3000"
