{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.CrossSeamSpec (crossSeamSpec) where

import Data.Aeson (
    Object,
    ToJSON,
    Value (Object),
    decode,
    encode,
    object,
    toJSON,
    (.=),
 )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)

import qualified Data.ByteString.Lazy.Char8 as LBS8
import Sabela.AI.Capabilities.ToolName (ToolName (ExecuteCell))
import Sabela.AI.KernelState (Activity (Executing), KernelState (Alive, Cold))
import qualified Sabela.AI.Provenance as Server
import Sabela.AI.Types (
    ToolOutcome (ToolErr),
    toolOutcomeIsError,
    toolOutcomeValue,
 )
import qualified Siza.Provenance.Event as Client
import Siza.Retro (decodeSession)
import Test.Hspec

serverEvent :: Server.SessionEvent
serverEvent =
    Server.SessionEvent
        { Server.seAt = UTCTime (fromGregorian 2026 6 20) (secondsToDiffTime 3600)
        , Server.seSession = "siza-abc"
        , Server.seNotebook = "nb"
        , Server.seActor = Server.InBrowserChat
        , Server.seCall = ExecuteCell
        , Server.seInput = object ["cell_id" .= (3 :: Int)]
        , Server.seOutcome = ToolErr (object ["busy" .= True])
        , Server.seKernelBefore = Alive 2 Executing True
        , Server.seGen = 7
        , Server.sePrev = Nothing
        }

clientEvent :: Client.SessionEvent
clientEvent =
    Client.SessionEvent
        { Client.seAt = UTCTime (fromGregorian 2026 6 20) (secondsToDiffTime 3600)
        , Client.seSession = "siza-abc"
        , Client.seNotebook = "nb"
        , Client.seActor = Client.InBrowserChat
        , Client.seCall = ExecuteCell
        , Client.seInput = object ["cell_id" .= (3 :: Int)]
        , Client.sePreflight = Nothing
        , Client.seOutcome = ToolErr (object ["busy" .= True])
        , Client.seKernelBefore = Alive 2 Executing True
        , Client.seGen = 7
        , Client.sePrev = Nothing
        }

crossSeamSpec :: Spec
crossSeamSpec = describe "cross-seam provenance interop (Part 7 shared log)" $ do
    it "a SERVER-written record decodes with the CLIENT parser" $
        case decode (encode serverEvent) :: Maybe Client.SessionEvent of
            Nothing -> expectationFailure "client parser dropped a server record"
            Just ev -> do
                Client.seSession ev `shouldBe` "siza-abc"
                Client.seGen ev `shouldBe` 7
                Client.seCall ev `shouldBe` ExecuteCell
                Client.seActor ev `shouldBe` Client.InBrowserChat
                toolOutcomeIsError (Client.seOutcome ev) `shouldBe` True
                Client.seKernelBefore ev `shouldBe` Alive 2 Executing True
                Client.sePreflight ev `shouldBe` Nothing

    it "a CLIENT-written record decodes with the SERVER parser" $
        case decode (encode clientEvent) :: Maybe Server.SessionEvent of
            Nothing -> expectationFailure "server parser dropped a client record"
            Just ev -> do
                Server.seSession ev `shouldBe` "siza-abc"
                Server.seGen ev `shouldBe` 7
                Server.seCall ev `shouldBe` ExecuteCell
                Server.seActor ev `shouldBe` Server.InBrowserChat
                toolOutcomeIsError (Server.seOutcome ev) `shouldBe` True
                Server.seKernelBefore ev `shouldBe` Alive 2 Executing True

    it "the actor tag agrees across seams" $
        Server.actorTag Server.InBrowserChat
            `shouldBe` Client.actorWire Client.InBrowserChat

    it "a Cold kernel-before survives the server->client hop" $
        case decode (encode serverEvent{Server.seKernelBefore = Cold}) of
            Just (ev :: Client.SessionEvent) ->
                Client.seKernelBefore ev `shouldBe` Cold
            Nothing -> expectationFailure "client parser dropped a Cold server record"

    it "the outcome payload survives the hop intact" $
        case decode (encode serverEvent) :: Maybe Client.SessionEvent of
            Just ev ->
                toolOutcomeValue (Client.seOutcome ev)
                    `shouldBe` object ["busy" .= True]
            Nothing -> expectationFailure "client parser dropped a server record"

    it "both seams emit byte-identical shared fields" $
        case (asObject serverEvent, asObject clientEvent) of
            (Just s, Just c) ->
                mapM_
                    (\k -> KM.lookup (Key.fromText k) s `shouldBe` KM.lookup (Key.fromText k) c)
                    sharedKeys
            _ -> expectationFailure "a seam did not encode to a JSON object"

    it "both seams emit the same key set on a shared field set" $
        case (asObject serverEvent, asObject clientEvent) of
            (Just s, Just c) ->
                mapM_
                    (\k -> KM.member (Key.fromText k) s `shouldBe` KM.member (Key.fromText k) c)
                    sharedKeys
            _ -> expectationFailure "a seam did not encode to a JSON object"

    it "siza retro decodes BOTH a server-seam and a client-seam line" $ do
        let blob =
                LBS8.intercalate "\n" [encode serverEvent, encode clientEvent]
            evs = decodeSession blob
        length evs `shouldBe` 2
        map Client.seSession evs `shouldBe` ["siza-abc", "siza-abc"]
        map Client.seGen evs `shouldBe` [7, 7]
        map Client.seActor evs
            `shouldBe` [Client.InBrowserChat, Client.InBrowserChat]

sharedKeys :: [Text]
sharedKeys =
    ["actor", "tool", "outcome", "kernelBefore", "session", "gen", "preflight"]

asObject :: (ToJSON a) => a -> Maybe Object
asObject x = case toJSON x of
    Object o -> Just o
    _ -> Nothing
