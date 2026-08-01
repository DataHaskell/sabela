{-# LANGUAGE OverloadedStrings #-}

{- | C2-10e: the two elision layers held opposite policies on a refused write,
and the marker they leave named a tool the surface did not offer. One
protection rule, and a retrieval reference drawn from the offered catalogue.
-}
module Test.CompactProtectSpec (compactProtectSpec) where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAsciiLower)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec
import Test.QuickCheck

import Siza.Agent.Compact (compactSeed)
import Siza.Agent.EmitLedger (dedupText, emptyEmitLedger)
import Siza.Agent.Stack (Surface (..))
import Siza.Agent.Tools (catalogueFor)
import Test.TruthGen (genGhcDiagnostic, genPrefixed)

compactProtectSpec :: Spec
compactProtectSpec = describe "what no elision path may touch" $ do
    it "never elides a refusal, whichever field names it (C2-10e)" $
        property $
            forAll genRefusalShape $ \payload ->
                let seeded = contentOf (compactSeed [toolMsgV "insert_cell" payload])
                    (_, led) = dedupText 1 payload emptyEmitLedger
                    (repeated, _) = dedupText 2 payload led
                 in counterexample "compaction elided it" (seeded === Just payload)
                        .&&. counterexample
                            "the ledger back-referenced it"
                            (repeated === payload)

    it "names a retrieval tool the eliding surface offers (C2-10e)" $
        property $
            forAll genPlainPayload $ \payload ->
                let stub =
                        fromMaybe "" (contentOf (compactSeed [toolMsgV "discover" payload]))
                    named = [n | n <- chatCatalogueNames, n `T.isInfixOf` stub]
                 in counterexample (T.unpack stub) $
                        counterexample "no catalogued tool named" (named =/= [])
                            .&&. conjoin
                                [ counterexample (T.unpack tok) (tok `elem` chatCatalogueNames)
                                | tok <- toolShapedTokens stub
                                ]

{- | The tools the chat surface — the only surface that elides — is served. An
elision marker may name a retrieval tool only from this list.
-}
chatCatalogueNames :: [Text]
chatCatalogueNames =
    [ n
    | Object o <- catalogueFor ChatSurface
    , Just (Object f) <- [KM.lookup "function" o]
    , Just (String n) <- [KM.lookup "name" f]
    ]

{- | Tokens shaped like a tool name. A generated payload's words carry no
underscore, so such a token in a stub was written by the harness.
-}
toolShapedTokens :: Text -> [Text]
toolShapedTokens =
    filter (\w -> T.any (== '_') w && T.all snakeChar w)
        . T.split (not . snakeChar)
  where
    snakeChar c = isAsciiLower c || c == '_'

{- | An elidable result whose own bytes carry no underscore, so every
tool-shaped token in its stub was written by the harness.
-}
genPlainPayload :: Gen Text
genPlainPayload = do
    filler <- genWord
    body <- T.take 300 . T.replicate 300 <$> genWord
    n <- choose (1, 4)
    ks <- vectorOf n genWord
    vs <- vectorOf n genWord
    pure . encodeText . object $
        (K.fromText filler .= body)
            : zipWith (\k v -> K.fromText k .= v) ks vs
  where
    genWord = T.pack <$> ((:) <$> lower <*> vectorOf 6 lower)
    lower = elements ['a' .. 'z']

{- | A refused write named by only one of the fields a refusal carries, at a
length that straddles the stub floor. Neither elision path may touch it.
-}
genRefusalShape :: Gen Text
genRefusalShape = do
    kind <- genPrefixed "k"
    steps <- choose (1, 6) >>= \n -> vectorOf n (genPrefixed "g")
    diag <- genGhcDiagnostic
    pad <- T.take 300 . T.replicate 300 <$> genPrefixed "p"
    elements
        [ encodeText (object ["notCommitted" .= kind, "padding" .= pad])
        , encodeText (object ["guidance" .= steps, "padding" .= pad])
        , encodeText
            ( object
                ["notCommitted" .= kind, "guidance" .= steps, "padding" .= pad]
            )
        , encodeText
            (object ["diagnostic" .= diag, "guidance" .= steps, "padding" .= pad])
        ]

toolMsgV :: Text -> Text -> Value
toolMsgV name content =
    object ["role" .= ("tool" :: Text), "tool_name" .= name, "content" .= content]

-- | The content of the first compacted message, if there is one.
contentOf :: ([Value], a) -> Maybe Text
contentOf (msgs, _) = case listToMaybe msgs of
    Just (Object o) -> case KM.lookup (K.fromText "content") o of
        Just (String s) -> Just s
        _ -> Nothing
    _ -> Nothing

encodeText :: Value -> Text
encodeText = TE.decodeUtf8 . LBS.toStrict . encode
