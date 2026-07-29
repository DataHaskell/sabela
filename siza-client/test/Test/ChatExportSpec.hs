{-# LANGUAGE OverloadedStrings #-}

module Test.ChatExportSpec (chatExportSpec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Hspec

import Siza.Agent.Chat.Export (exportCommand, exportFileName, exportText)

msgs :: [Value]
msgs =
    [ object ["role" .= ("system" :: Text), "content" .= ("be brief" :: Text)]
    , object ["role" .= ("user" :: Text), "content" .= ("plot a sine wave" :: Text)]
    ]

chatExportSpec :: Spec
chatExportSpec = describe "chat /export" $ do
    describe "exportCommand" $ do
        it "recognises the bare command" $
            exportCommand "/export" `shouldBe` Just Nothing
        it "recognises a path argument, spaces included" $
            exportCommand "/export docs/discover/live/run 3.md"
                `shouldBe` Just (Just "docs/discover/live/run 3.md")
        it "tolerates surrounding whitespace" $
            exportCommand "  /export  " `shouldBe` Just Nothing
        it "ignores lookalikes and ordinary requests" $ do
            exportCommand "/exportfoo" `shouldBe` Nothing
            exportCommand "please export the chat" `shouldBe` Nothing

    describe "exportFileName" $
        it "is a timestamped markdown name" $ do
            let name = exportFileName (posixSecondsToUTCTime 0)
            name `shouldBe` "siza-chat-19700101-000000.md"

    describe "exportText" $ do
        it "renders the audit sections with the model in the title" $ do
            let t = exportText "gemma4:latest" msgs
            t `shouldSatisfy` T.isInfixOf "gemma4:latest"
            t `shouldSatisfy` T.isInfixOf "## 1. system"
            t `shouldSatisfy` T.isInfixOf "## 2. user"
            t `shouldSatisfy` T.isInfixOf "plot a sine wave"
