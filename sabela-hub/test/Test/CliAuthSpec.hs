{-# LANGUAGE OverloadedStrings #-}

{- | The @siza login@ device-authorization flow, driven end to end through the
hub WAI app: start → authorize page → approve → poll → resolve the minted token
at the proxy boundary. The test session ("usersid") is 'SStarting', so a request
that resolves to it returns the "Starting your notebook environment" page —
which is how we assert a token (or cookie) resolved without a live backend.
-}
module Test.CliAuthSpec (spec) where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
import Network.Wai (Application, defaultRequest, requestHeaders, requestMethod)
import Network.Wai.Test
import Test.Hspec
import Test.ProxyHelpers (makeAppSess)

spec :: Spec
spec = describe "Hub.CliAuth: siza login device flow" $ do
    it "start returns the device-flow fields" $ do
        app <- makeAppSess
        r <- post app "/_hub/cli-auth/start" [] ""
        simpleStatus r `shouldBe` status200
        jstr "deviceCode" r `shouldSatisfy` notEmpty
        jstr "userCode" r `shouldSatisfy` notEmpty
        body r `shouldSatisfy` isInfixOf "/_hub/cli-auth?code="

    it "poll on an unknown deviceCode reports expired" $ do
        app <- makeAppSess
        r <-
            post
                app
                "/_hub/cli-auth/poll"
                []
                (encode (object ["deviceCode" .= ("nope" :: Text)]))
        jstr "status" r `shouldBe` Just "expired"

    it "the authorize page falls back to login when unauthenticated" $ do
        app <- makeAppSess
        r <- get app "/_hub/cli-auth?code=ABCD1234" []
        body r `shouldNotSatisfy` isInfixOf "Approve access"

    it "approve binds a token that resolves to the approver's session" $ do
        app <- makeAppSess
        start <- post app "/_hub/cli-auth/start" [] ""
        let device = fromJust (jstr "deviceCode" start)
            user = fromJust (jstr "userCode" start)
        page <- get app ("/_hub/cli-auth?code=" <> user) [cookie "usersid"]
        body page `shouldSatisfy` isInfixOf "Approve access"
        body page `shouldSatisfy` isInfixOf (T.unpack user)
        let csrf = fromJust (csrfFrom page)
        appr <-
            post
                app
                "/_hub/cli-auth/approve"
                [cookie "usersid"]
                (encode (object ["userCode" .= user, "csrf" .= csrf]))
        simpleStatus appr `shouldBe` status200
        polled <-
            post app "/_hub/cli-auth/poll" [] (encode (object ["deviceCode" .= device]))
        jstr "status" polled `shouldBe` Just "approved"
        let token = fromJust (jstr "token" polled)
        -- The token, presented as a bearer with no cookie, resolves to usersid.
        viaToken <- get app "/" [("Authorization", "Bearer " <> TE.encodeUtf8 token)]
        body viaToken `shouldSatisfy` isInfixOf "Starting your notebook environment"
        -- Control: no credential at all does NOT resolve to a session.
        anon <- get app "/" []
        body anon `shouldNotSatisfy` isInfixOf "Starting your notebook environment"

    it "approve with a mismatched csrf is rejected and mints no token" $ do
        app <- makeAppSess
        start <- post app "/_hub/cli-auth/start" [] ""
        let device = fromJust (jstr "deviceCode" start)
            user = fromJust (jstr "userCode" start)
        _ <- get app ("/_hub/cli-auth?code=" <> user) [cookie "usersid"]
        appr <-
            post
                app
                "/_hub/cli-auth/approve"
                [cookie "usersid"]
                (encode (object ["userCode" .= user, "csrf" .= ("wrong" :: Text)]))
        simpleStatus appr `shouldBe` status410
        polled <-
            post app "/_hub/cli-auth/poll" [] (encode (object ["deviceCode" .= device]))
        jstr "status" polled `shouldBe` Just "pending"

    it "an unauthenticated approve is refused (401)" $ do
        app <- makeAppSess
        appr <-
            post
                app
                "/_hub/cli-auth/approve"
                []
                (encode (object ["userCode" .= ("X" :: Text), "csrf" .= ("Y" :: Text)]))
        simpleStatus appr `shouldBe` status401

-- ---------------------------------------------------------------------------
-- request helpers
-- ---------------------------------------------------------------------------

post :: Application -> Text -> [Header] -> BL.ByteString -> IO SResponse
post app path hdrs bdy =
    runSession
        ( srequest $
            SRequest
                ( setPath
                    defaultRequest
                        { requestMethod = methodPost
                        , requestHeaders = ("content-type", "application/json") : hdrs
                        }
                    (TE.encodeUtf8 path)
                )
                bdy
        )
        app

get :: Application -> Text -> [Header] -> IO SResponse
get app path hdrs =
    runSession
        (request (setPath defaultRequest{requestHeaders = hdrs} (TE.encodeUtf8 path)))
        app

cookie :: Text -> Header
cookie sid = ("Cookie", "_sabela_session=" <> TE.encodeUtf8 sid)

body :: SResponse -> String
body = LC8.unpack . simpleBody

notEmpty :: Maybe Text -> Bool
notEmpty = maybe False (not . T.null)

jstr :: Text -> SResponse -> Maybe Text
jstr k r = case decode (simpleBody r) of
    Just (Object o) -> case KM.lookup (Key.fromText k) o of
        Just (String s) -> Just s
        _ -> Nothing
    _ -> Nothing

-- | Pull the CSRF nonce the authorize page embeds in @csrf:'<nonce>'@.
csrfFrom :: SResponse -> Maybe Text
csrfFrom r =
    let t = TE.decodeUtf8 (BL.toStrict (simpleBody r))
        (_, rest) = T.breakOn "csrf:'" t
     in if T.null rest
            then Nothing
            else Just (T.takeWhile (/= '\'') (T.drop (T.length "csrf:'") rest))
