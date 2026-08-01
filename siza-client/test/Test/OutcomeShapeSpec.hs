{-# LANGUAGE OverloadedStrings #-}

{- | C1-15b: the shape of a result must survive distillation. The bounded
view of an output is chosen from its own mime tag, and the keys that carry a
result's shape, its diagnostics and its guidance are never shed.
-}
module Test.OutcomeShapeSpec (outcomeShapeSpec) where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec
import Test.QuickCheck

import Siza.Agent.OutcomeDistill (distillOutcome, outcomeCharBudget, stripEscapes)

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

arrayOf :: Maybe Value -> [Value]
arrayOf (Just (Array a)) = foldr (:) [] a
arrayOf _ = []

textOf :: Maybe Value -> Maybe Text
textOf (Just (String s)) = Just s
textOf _ = Nothing

serialisedChars :: Value -> Int
serialisedChars = T.length . TE.decodeUtf8 . LBS.toStrict . encode

data Body = Body {bMime :: Text, bRaw :: Text}
    deriving (Show)

genBody :: Gen Body
genBody =
    oneof
        [ Body "application/json" . jsonBody <$> listOf1 genKey
        , Body "text/plain" <$> genPlain
        , Body "text/html" <$> genHtml
        , Body "image/svg+xml" <$> genHtml
        ]

genKey :: Gen Text
genKey = elements ["containers", "text", "aeson", "mtl", "bytestring", "zzfrob"]

jsonBody :: [Text] -> Text
jsonBody ks =
    TE.decodeUtf8 (LBS.toStrict (encode (object [(K.fromText k, String k) | k <- ks])))

genPlain :: Gen Text
genPlain = do
    n <- choose (1, 60 :: Int)
    ls <- vectorOf n (elements ["alpha beta", "gamma", T.replicate 200 "x", ""])
    pure (T.unlines ls)

genHtml :: Gen Text
genHtml = do
    tag <- elements ["svg", "table", "div", "figure"]
    n <- choose (0, 4000 :: Int)
    pure ("<" <> tag <> ">" <> T.replicate n "y" <> "</" <> tag <> ">")

envelopeOf :: [Body] -> [Text] -> Value
envelopeOf bodies vals =
    object
        [ "outcome" .= object ["tag" .= ("Succeeded" :: Text)]
        , "ok" .= True
        , "cellId" .= (7 :: Int)
        , "values" .= vals
        , "guidance" .= ["check the shape before writing" :: Text]
        , "outputs" .= [object ["oiMime" .= bMime b, "oiOutput" .= bRaw b] | b <- bodies]
        ]

{- | Echoes wide enough to put the envelope under real budget pressure, which
is the only regime in which shedding decides what the model still knows.
-}
genValues :: Gen [Text]
genValues = do
    n <- choose (1, 40 :: Int)
    vectorOf n $ do
        name <- genKey
        width <- choose (1, 120 :: Int)
        pure (name <> " :: Frame = <" <> T.replicate width "c" <> ">")

genEnvelope :: Gen Value
genEnvelope = envelopeOf <$> listOf1 genBody <*> genValues

{- | A write ack: the execution summary, and everything the write learned, sit
one level down under @execution@.
-}
genWriteAck :: Gen Value
genWriteAck = do
    inner <- genEnvelope
    pure
        ( object
            [ "cellId" .= (7 :: Int)
            , "status" .= ("completed" :: Text)
            , "execution" .= inner
            ]
        )

outcomeShapeSpec :: Spec
outcomeShapeSpec = describe "outcome shape survives distillation (C1-15b)" $ do
    it "keeps the values echo under every budget pressure" $
        property $
            forAll genEnvelope $ \env ->
                let d = distillOutcome env
                 in counterexample (show d) (isJust (field "values" d))

    it "keeps a write ack's nested execution summary and its values" $
        property $
            forAll genWriteAck $ \env ->
                let d = distillOutcome env
                    inner = field "execution" d
                 in counterexample (show d) $
                        isJust inner && isJust (inner >>= field "values")

    it "keeps guidance, which is never shed" $
        property $
            forAll genEnvelope $ \env ->
                let d = distillOutcome env
                 in counterexample (show d) (isJust (field "guidance" d))

    it "stays within the declared budget" $
        property $
            forAll genEnvelope $ \env ->
                serialisedChars (distillOutcome env) <= outcomeCharBudget

    it "shows a prefix of the real output, never a rewritten one" $
        property $
            forAll genEnvelope $ \env ->
                let d = distillOutcome env
                    shown = mapMaybe (textOf . field "oiOutput") (arrayOf (field "outputs" d))
                    raws = map stripEscapes (rawsOf env)
                 in counterexample (show (shown, take 1 raws)) $
                        and [any (h `T.isPrefixOf`) raws | h <- shown]

    it "discloses a JSON output's top-level keys instead of a blind prefix" $
        property $
            forAll (envelopeOf <$> listOf1 (Body "application/json" . jsonBody <$> listOf1 genKey) <*> genValues) $ \env ->
                let d = distillOutcome env
                    outs = arrayOf (field "outputs" d)
                 in counterexample (show d) $
                        not (null outs) ==> all (isJust . field "jsonKeys") outs

    it "reports only keys the JSON output really has" $
        property $
            forAll genEnvelope $ \env ->
                let d = distillOutcome env
                    reported = concatMap (keysOf . field "jsonKeys") (arrayOf (field "outputs" d))
                    raws = rawsOf env
                 in counterexample (show (reported, raws)) $
                        and [any (k `T.isInfixOf`) raws | k <- reported]

    it "reconciles shown outputs with omittedOutputs" $
        property $
            forAll genEnvelope $ \env ->
                let d = distillOutcome env
                    shown = length (arrayOf (field "outputs" d))
                    omitted = case field "omittedOutputs" d of
                        Just (Number n) -> round n
                        _ -> 0 :: Int
                    total = case field "outputCount" d of
                        Just (Number n) -> round n
                        _ -> shown
                 in counterexample (show d) (shown + omitted === total)

keysOf :: Maybe Value -> [Text]
keysOf v = mapMaybe (textOf . Just) (arrayOf v)

rawsOf :: Value -> [Text]
rawsOf env = mapMaybe (textOf . field "oiOutput") (arrayOf (field "outputs" env))
