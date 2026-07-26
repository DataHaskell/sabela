{-# LANGUAGE OverloadedStrings #-}

module Test.CompactSpec (compactSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Compact (compactSeed, recallResult)

big :: Text
big = T.replicate 60 "discover answer line\n"

assistantWith :: Text -> Value
assistantWith t =
    object
        ["role" .= ("assistant" :: Text), "content" .= ("ok" :: Text), "thinking" .= t]

toolMsgV :: Text -> Text -> Value
toolMsgV name content =
    object ["role" .= ("tool" :: Text), "tool_name" .= name, "content" .= content]

userMsgV :: Text -> Value
userMsgV t = object ["role" .= ("user" :: Text), "content" .= t]

field :: Text -> Value -> Maybe Text
field k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> Just s
    _ -> Nothing
field _ _ = Nothing

compactSpec :: Spec
compactSpec = describe "seed compaction" $ do
    it "keeps the user prompt verbatim, so `it` still refers" $ do
        let (seed, _) = compactSeed [userMsgV "plot a sine wave", userMsgV "animate it"]
        map (field "content") seed
            `shouldBe` [Just "plot a sine wave", Just "animate it"]

    it "drops the model's own thinking" $ do
        let (seed, _) = compactSeed [assistantWith "long private reasoning"]
        field "thinking" (head seed) `shouldBe` Nothing

    it "keeps the assistant's prose, which the next prompt may refer to" $ do
        let (seed, _) = compactSeed [assistantWith "reasoning"]
        field "content" (head seed) `shouldBe` Just "ok"

    it "elides a large tool result to a reference" $ do
        let (seed, _) = compactSeed [toolMsgV "discover" big]
        field "content" (head seed)
            `shouldSatisfy` maybe False ("[result #1" `T.isPrefixOf`)

    it "names the tool and the index in the stub" $ do
        let (seed, _) = compactSeed [toolMsgV "discover" big]
            c = fromMaybe "" (field "content" (head seed))
        c `shouldSatisfy` T.isInfixOf "from discover"
        c `shouldSatisfy` T.isInfixOf "recall_result"

    it "keeps a short result verbatim: a stub would cost more than it saves" $ do
        let (seed, _) = compactSeed [toolMsgV "check_type" "x :: Int"]
        field "content" (head seed) `shouldBe` Just "x :: Int"

    it "recalls an elided result in full, byte for byte" $ do
        let (_, store) = compactSeed [toolMsgV "discover" big]
        recallResult store (object ["index" .= (1 :: Int)]) `shouldBe` big

    it "numbers several elided results independently" $ do
        let (_, store) = compactSeed [toolMsgV "discover" big, toolMsgV "try" (big <> "!")]
        recallResult store (object ["index" .= (2 :: Int)]) `shouldBe` big <> "!"

    it "an unknown index says so, and lists what there is" $
        recallResult (Map.fromList [(1, "a")]) (object ["index" .= (9 :: Int)])
            `shouldSatisfy` T.isInfixOf "No result #9"

    it "a stringly-typed index still resolves" $
        recallResult (Map.fromList [(3, "found")]) (object ["index" .= ("3" :: Text)])
            `shouldBe` "found"

    it "a missing index asks for one rather than answering emptily" $
        recallResult (Map.fromList [(1, "a")]) (object [])
            `shouldSatisfy` T.isInfixOf "needs an integer"
