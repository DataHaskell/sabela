{-# LANGUAGE OverloadedStrings #-}

{- | R10-T5 execution-output distillation invariants: over generated cell
outputs of varying size and shape (including a multi-thousand-char ANSI wall
like barChart-on turn-27) every execution result crosses the model surface as
ONE bounded envelope — @outcome@ + @outputCount@ + escape-stripped head — under
the declared budget, one declared per-output schema, with no raw ANSI or
control byte passing through. A value with no output-shaped array is unchanged.
The CAVEAT (product cohesion) pins that the human-visible 'OutputItem' wire
encoding is NOT capped by this model-context-only transform.
-}
module Test.OutcomeDistillSpec (outcomeDistillSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Sabela.AI.LeakShape (controlCharred)
import Sabela.Model (MimeType (..), OutputItem (..))
import Siza.Agent.OutcomeDistill (
    distillOutcome,
    outcomeCharBudget,
    outcomeHeadBudget,
    stripEscapes,
 )

-- | A multi-thousand-char ANSI-escape wall (the barChart turn-27 shape).
ansiWall :: Text
ansiWall = T.concat (replicate 2000 "\ESC[95m\9608\9608\ESC[0m block ")

-- | Generated outputs of varying size and shape: (mime, body).
outputBodies :: [(Text, Text)]
outputBodies =
    [ ("text/plain", "42")
    , ("text/plain", T.replicate 300 "x")
    , ("text/plain", ansiWall)
    , ("text/html", "<svg>" <> T.replicate 4000 "y" <> "</svg>")
    , ("image/svg+xml", T.replicate 9000 "z")
    , ("text/plain", "line1\nline2\ttabbed")
    ]

outItem :: (Text, Text) -> Value
outItem (mime, body) = object ["oiMime" .= mime, "oiOutput" .= body]

-- | An execution outcome (CellResult wire shape) with the given outputs.
execOutcome :: Int -> [(Text, Text)] -> Value
execOutcome cid bodies =
    object
        [ "outcome" .= object ["tag" .= ("Succeeded" :: Text)]
        , "ok" .= True
        , "cellId" .= cid
        , "warnings" .= ([] :: [Value])
        , "outputs" .= map outItem bodies
        ]

serialisedChars :: Value -> Int
serialisedChars = T.length . TE.decodeUtf8 . LBS.toStrict . encode

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

arrayOf :: Value -> [Value]
arrayOf (Array a) = foldr (:) [] a
arrayOf _ = []

intOf :: Maybe Value -> Int
intOf (Just (Number n)) = round n
intOf _ = -1

keySet :: Value -> [Text]
keySet (Object o) = sort (map K.toText (KM.keys o))
keySet _ = []

hasEsc :: Value -> Bool
hasEsc = T.any (== '\ESC') . TE.decodeUtf8 . LBS.toStrict . encode

outcomeDistillSpec :: Spec
outcomeDistillSpec = describe "R10-T5 execution-output distillation" $ do
    let full = execOutcome 0 outputBodies
        distilled = distillOutcome full
        items = arrayOf (fromMaybe Null (field "outputs" distilled))

    describe "GENERAL-INVARIANT over generated outputs" $ do
        it "the whole distilled envelope is under the declared budget" $
            serialisedChars distilled `shouldSatisfy` (<= outcomeCharBudget)

        it "records outputCount = the original number of outputs" $
            intOf (field "outputCount" distilled) `shouldBe` length outputBodies

        it "shown outputs + omittedOutputs reconcile to outputCount" $ do
            let shown = length items
                omitted = intOf (field "omittedOutputs" distilled)
                omitted' = max omitted 0
            shown + omitted' `shouldBe` intOf (field "outputCount" distilled)

        it "every surviving output decodes on ONE declared schema" $
            forM_ items $ \i ->
                keySet i `shouldBe` ["chars", "oiMime", "oiOutput"]

        it "every head is escape-stripped and within the head budget" $
            forM_ items $ \i -> do
                let head' = maybe "" textOf (field "oiOutput" i)
                T.length head' `shouldSatisfy` (<= outcomeHeadBudget)
                controlCharred head' `shouldBe` False

        it "NO raw ANSI escape or control byte passes through (R3.10)" $ do
            hasEsc distilled `shouldBe` False
            controlCharred (TE.decodeUtf8 (LBS.toStrict (encode distilled)))
                `shouldBe` False

    describe "the outcome frame is preserved" $ do
        it "keeps the outcome tag and ok flag" $ do
            field "outcome" distilled `shouldBe` field "outcome" full
            field "ok" distilled `shouldBe` Just (Bool True)
        it "keeps warnings untouched (diagnostics are load-bearing)" $
            field "warnings" distilled `shouldBe` Just (Array mempty)

        it "hard-bounds an execution carrying a warning wall" $ do
            let hostile =
                    object
                        [ "outcome" .= object ["tag" .= ("Raised" :: Text)]
                        , "ok" .= False
                        , "cellId" .= (4 :: Int)
                        , "warnings" .= [T.replicate 20000 "warning "]
                        , "outputs" .= [outItem ("text/plain", "small")]
                        ]
                bounded = distillOutcome hostile
            serialisedChars bounded `shouldSatisfy` (<= outcomeCharBudget)
            field "outcome" bounded `shouldBe` field "outcome" hostile
            field "ok" bounded `shouldBe` Just (Bool False)
            hasEsc bounded `shouldBe` False

        it "hard-bounds an execution with empty outputs and a warning wall" $ do
            let hostile =
                    object
                        [ "outcome" .= object ["tag" .= ("Raised" :: Text)]
                        , "ok" .= False
                        , "cellId" .= (5 :: Int)
                        , "warnings" .= [T.replicate 20000 "warning "]
                        , "outputs" .= ([] :: [Value])
                        ]
                bounded = distillOutcome hostile
            serialisedChars bounded `shouldSatisfy` (<= outcomeCharBudget)
            field "outcome" bounded `shouldBe` field "outcome" hostile
            field "ok" bounded `shouldBe` Just (Bool False)

    describe "the ANSI wall specifically (barChart turn-27)" $ do
        let one = distillOutcome (execOutcome 0 [("text/plain", ansiWall)])
            outs = arrayOf (fromMaybe Null (field "outputs" one))
        it "renders as one Succeeded output, bounded, no escape wall" $ do
            field "outputCount" one `shouldBe` Just (Number 1)
            serialisedChars one `shouldSatisfy` (<= outcomeCharBudget)
            hasEsc one `shouldBe` False
        it "the head still discloses the size via chars" $
            forM_ outs $ \i ->
                intOf (field "chars" i) `shouldBe` T.length ansiWall

    describe "a value with no output-shaped array is unchanged" $ do
        let discover =
                object
                    [ "query" .= ("plot" :: Text)
                    , "state" .= ("found" :: Text)
                    , "hits" .= ([] :: [Value])
                    ]
        it "passes a discover envelope through as identity" $
            distillOutcome discover `shouldBe` discover
        it "passes a check_type reply through as identity" $ do
            let ct = object ["result" .= ("Int -> Int" :: Text)]
            distillOutcome ct `shouldBe` ct

    describe "stripEscapes" $ do
        it "removes CSI colour sequences" $
            stripEscapes "\ESC[95mred\ESC[0m" `shouldBe` "red"
        it "keeps newline and tab, drops other control bytes" $
            stripEscapes "a\nb\tc\rd\ETX" `shouldBe` "a\nb\tcd"

    describe "CAVEAT: the human-visible product artifact is NOT capped" $ do
        it "OutputItem's own wire encoding preserves the full output verbatim" $ do
            let humanWire = toJSON (OutputItem MimePlain ansiWall)
            maybe "" textOf (field "oiOutput" humanWire)
                `shouldBe` ansiWall
        it "only the model-context distillation caps; the two paths differ" $ do
            let humanLen =
                    T.length
                        (maybe "" textOf (field "oiOutput" (toJSON (OutputItem MimePlain ansiWall))))
                modelItem =
                    head
                        ( arrayOf
                            ( fromMaybe
                                Null
                                (field "outputs" (distillOutcome (execOutcome 0 [("text/plain", ansiWall)])))
                            )
                        )
                modelLen = T.length (maybe "" textOf (field "oiOutput" modelItem))
            humanLen `shouldSatisfy` (> outcomeHeadBudget)
            modelLen `shouldSatisfy` (<= outcomeHeadBudget)

textOf :: Value -> Text
textOf (String s) = s
textOf _ = ""
