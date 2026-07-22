{-# LANGUAGE OverloadedStrings #-}

{- | R9-T4 write boundary: a top-level signature with no equation is a
diagnostic-class property ('Sabela.Parse.signatureWithoutEquation') answered as
a PROPOSAL — the same cell completed with a @name = _@ typed-hole body
('holeBodyCompletion') handed back through the existing error envelope, never
committed as the session-damming red cell. The proposal forbids no correct move
and suggests no search (R5.8).
-}
module Test.WriteBoundarySpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.Admit (
    conflictJson,
    holeBodyCompletion,
    signatureBodyProposal,
    violationJson,
 )
import Sabela.AI.WriteAck (
    AckEnvelope (..),
    AckStatus (..),
    BusyAck (..),
    RefusalAck (..),
    WriteAck (..),
    busyAckJson,
    parseAckEnvelope,
    writeAckJson,
 )
import Sabela.Handlers (DefConflict (..), NotebookViolation (..))
import Sabela.Parse.Declared (signatureWithoutEquation)

names :: [Text]
names = ["topMonth", "revenueTotal", "f"]

-- One grid case: a source and the names it declares-without-equation.
type Case = (Text, [Text])

{- | {sig-only, sig+equation, equation-only, sig+equation-for-other-name,
multi-decl mixed} x a spread of spellings.
-}
grid :: [Case]
grid =
    concat
        [ [ (n <> " :: String", [n])
          , (n <> " :: String\n" <> n <> " = \"x\"", [])
          , (n <> " = \"x\"", [])
          , (n <> " :: Int\nother_" <> n <> " = 1", [n])
          , ("ok_" <> n <> " :: Int\nok_" <> n <> " = 1\n" <> n <> " :: Bool", [n])
          ]
        | n <- names
        ]

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

textOf :: Value -> Text
textOf (String s) = s
textOf _ = ""

{- | Phrases the proposal must never contain: forbidding the write or steering
back to search (R5.8).
-}
banned :: [Text]
banned =
    [ "do not"
    , "don't"
    , "cannot add"
    , "search again"
    , "try another"
    , "rephrase"
    , "discover"
    , "find_by_type"
    ]

spec :: Spec
spec = describe "write-boundary signature-without-equation proposal (R9-T4)" $ do
    describe "signatureWithoutEquation fires iff a sig lacks its equation" $
        it "matches the expected names across the generated grid" $
            forM_ grid $ \(src, want) ->
                (src, signatureWithoutEquation src) `shouldBe` (src, want)

    describe "holeBodyCompletion offers the typed-hole body" $ do
        it "appends `name = _` for a single undefined signature" $
            holeBodyCompletion ["topMonth"] "topMonth :: String"
                `shouldBe` "topMonth :: String\ntopMonth = _"
        it "appends a hole body per undefined name" $
            holeBodyCompletion ["a", "b"] "a :: Int\nb :: Bool"
                `shouldBe` "a :: Int\nb :: Bool\na = _\nb = _"

    describe "signatureBodyProposal is a proposal, not a forbid" $ do
        let prop = signatureBodyProposal ["topMonth"] "topMonth :: String"
        it "names the undefined binding" $
            maybe "" textOf (field "error" prop)
                `shouldSatisfy` T.isInfixOf "`topMonth`"
        it "carries the completed source with the hole body" $ do
            maybe "" textOf (field "suggestedSource" prop)
                `shouldBe` "topMonth :: String\ntopMonth = _"
        it "carries the needsBody names for the client to route on" $
            field "needsBody" prop `shouldBe` Just (toJSON (["topMonth"] :: [Text]))
        it "never forbids the move nor suggests further search (R5.8)" $ do
            let msg = T.toLower (maybe "" textOf (field "error" prop))
            forM_ banned $ \p ->
                (p, T.isInfixOf p msg) `shouldBe` (p, False)

    envelopeSweepSpec

{- | R3.6 envelope validator sweep: the proposal and every refusal class decode
against the ONE write-ack envelope validator ('parseAckEnvelope') — the same
shape that decodes a settled write and an own-write bounce. No mutation-path
response is undecodable, and none serialises execution into a string.
-}
envelopeSweepSpec :: Spec
envelopeSweepSpec = describe "R3.6: every mutation outcome decodes against the write-ack envelope" $ do
    let okWrite =
            writeAckJson
                (WriteAck 3 AckCompleted (Just "h") (Just (toJSON True)) False Nothing)
        executing = writeAckJson (WriteAck 3 AckExecuting Nothing Nothing False Nothing)
        busy = busyAckJson (BusyAck 2 100)
        proposal = signatureBodyProposal ["topMonth"] "topMonth :: String"
        pending = violationJson (VPendingError 4 "boom")
        conflict = conflictJson (DefConflict "x" 7)
    it "a settled write decodes as a write ack on its cell" $
        case parseAckEnvelope okWrite of
            Just (EnvWrite wa) -> waCellId wa `shouldBe` 3
            other -> expectationFailure ("not a write ack: " <> show other)
    it "an executing write decodes as a write ack" $
        case parseAckEnvelope executing of
            Just (EnvWrite wa) -> waStatus wa `shouldBe` AckExecuting
            other -> expectationFailure ("not a write ack: " <> show other)
    it "an own-write bounce decodes as busy" $
        parseAckEnvelope busy `shouldBe` Just (EnvBusy (BusyAck 2 100))
    it "the sig-without-body proposal decodes as a needs-body refusal" $
        case parseAckEnvelope proposal of
            Just (EnvRefusal ra) -> raKind ra `shouldBe` "needs-body"
            other -> expectationFailure ("not a refusal: " <> show other)
    it "the pending-error refusal decodes and names the blocking cell" $
        case parseAckEnvelope pending of
            Just (EnvRefusal ra) -> (raKind ra, raCell ra) `shouldBe` ("pending-error", Just 4)
            other -> expectationFailure ("not a refusal: " <> show other)
    it "the duplicate-def conflict decodes and names the owning cell" $
        case parseAckEnvelope conflict of
            Just (EnvRefusal ra) -> (raKind ra, raCell ra) `shouldBe` ("duplicate-def", Just 7)
            other -> expectationFailure ("not a refusal: " <> show other)
    it "no mutation-path response is undecodable (every class is Just)" $
        forM_ [okWrite, executing, busy, proposal, pending, conflict] $ \v ->
            (v, isNothing (parseAckEnvelope v)) `shouldBe` (v, False)
