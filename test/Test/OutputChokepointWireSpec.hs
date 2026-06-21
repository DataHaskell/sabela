{-# LANGUAGE OverloadedStrings #-}

{- | Pins the R2-5 'Output' chokepoint by driving the REAL producers:
'storeLargeResult', 'inlineOrStash', 'compactOutputs', 'compactMaybeText'.
Asserts the inline @{mime,output}@ shape, the stashed handle keys, and that
both compaction paths emit the SAME inline shape for the same text.
-}
module Test.OutputChokepointWireSpec (spec) where

import Data.Aeson (Value (..), toJSON, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (sort)
import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Test.Hspec

import Sabela.AI.Capabilities.Util (
    compactMaybeText,
    compactOutputs,
    inlineOrStash,
 )
import Sabela.AI.Handles (Output (..), newHandleStore, storeLargeResult)
import Sabela.AI.Store (AIStore, newAIStore)
import Sabela.Anthropic (defaultConfig)
import Sabela.Model (MimeType (..), OutputItem (..))

mkStore :: IO AIStore
mkStore = do
    mgr <- newManager defaultManagerSettings
    newAIStore (defaultConfig "test-key") mgr

objectKeys :: Value -> [String]
objectKeys (Object o) = sort (map (T.unpack . Key.toText) (KM.keys o))
objectKeys _ = []

spec :: Spec
spec = describe "Output chokepoint (R2-5)" $ do
    describe "storeLargeResult returns Output" $ do
        it "below-threshold text is Inline (placeholder MimePlain)" $ do
            hs <- newHandleStore
            out <- storeLargeResult hs "short"
            out `shouldBe` Inline MimePlain "short"

        it "above-threshold text is Stashed" $ do
            hs <- newHandleStore
            let big = T.unlines (map (T.pack . show) [1 :: Int .. 100])
            out <- storeLargeResult hs big
            case out of
                Stashed _ -> pure ()
                Inline _ _ -> expectationFailure "large payload should stash"

    describe "inline wire shape (byte-identical to today)" $ do
        it "Inline toJSON has exactly {mime,output}" $ do
            let v = toJSON (Inline MimeHtml "<p>hi</p>")
            objectKeys v `shouldBe` ["mime", "output"]
            v
                `shouldBe` A.object
                    ["mime" .= ("text/html" :: T.Text), "output" .= ("<p>hi</p>" :: T.Text)]

        it "compactOutputs below threshold emits the {mime,output} object" $ do
            store <- mkStore
            v <- compactOutputs store [OutputItem MimePlain "hello"]
            v `shouldBe` toJSON [Inline MimePlain "hello"]

    describe "stashed wire shape (summarizeForLLM keys)" $ do
        it "Stashed toJSON has the handle keys" $ do
            hs <- newHandleStore
            let big = T.unlines (map (T.pack . show) [1 :: Int .. 100])
            out <- storeLargeResult hs big
            objectKeys (toJSON out)
                `shouldBe` ["handleId", "hint", "summary", "totalBytes", "totalLines"]

        it "compactOutputs above threshold emits the handle keys" $ do
            store <- mkStore
            let big = T.unlines (map (T.pack . show) [1 :: Int .. 100])
            v <- compactOutputs store [OutputItem MimePlain big]
            case v of
                A.Array arr
                    | [one] <- foldr (:) [] arr ->
                        objectKeys one
                            `shouldBe` ["handleId", "hint", "summary", "totalBytes", "totalLines"]
                _ -> expectationFailure "expected a single-element array"

    describe "both paths share one inline shape"
        $ it
            "compactOutputs and compactMaybeText emit the SAME inline shape for the same text"
        $ do
            store <- mkStore
            let text = "the same body"
            outsV <- compactOutputs store [OutputItem MimePlain text]
            maybeV <- compactMaybeText store (Just text)
            let oneOf (Array arr) = case foldr (:) [] arr of [x] -> x; _ -> Null
                oneOf _ = Null
            oneOf outsV `shouldBe` maybeV

    describe "inlineOrStash re-tags the MIME" $
        it "carries the supplied MimeType into the inline shape" $ do
            store <- mkStore
            out <- inlineOrStash store MimeHtml "x"
            out `shouldBe` Inline MimeHtml "x"
