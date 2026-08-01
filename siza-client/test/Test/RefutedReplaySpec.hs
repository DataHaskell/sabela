{-# LANGUAGE OverloadedStrings #-}

{- | C1-17b: the disproven-repair ledger is read. A write whose normalised
source was already refused says so on its own payload, for every member of the
whitespace-equivalence class @normaliseSource@ defines.
-}
module Test.RefutedReplaySpec (refutedReplaySpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.HistoryGuard (guardDiscover, newSearchLedger)
import Siza.Agent.Discover.Ledger (normaliseSource)
import Test.TruthGen (genGhcDiagnostic, genSubstantiveSource)

refutedReplaySpec :: Spec
refutedReplaySpec = describe "a refused source is remembered and said (C1-17b)" $ do
    it "a reflowed resubmission of a refused source names the earlier refusal" $
        property $
            forAll genSubstantiveSource $ \src ->
                forAll (genReflow src) $ \src' ->
                    forAll genGhcDiagnostic $ \diag -> ioProperty $ do
                        ref <- newSearchLedger
                        let call s = guardDiscover ref (rejectWith diag) (write s)
                        _ <- call src
                        out <- call src'
                        pure $
                            counterexample (show out) $
                                conjoin
                                    [ property (normaliseSource src == normaliseSource src')
                                    , property (namesRepeat out)
                                    ]
    it "a source never refused before carries no repeat note" $
        property $
            forAll genSubstantiveSource $ \src ->
                forAll genGhcDiagnostic $ \diag -> ioProperty $ do
                    ref <- newSearchLedger
                    out <- guardDiscover ref (rejectWith diag) (write src)
                    pure (counterexample (show out) (not (namesRepeat out)))
    it "the note says whether the diagnostic changed, and never repeats it" $
        property $
            forAll genSubstantiveSource $ \src ->
                forAll ((,) <$> genGhcDiagnostic <*> genGhcDiagnostic) $
                    \(d1, d2) -> ioProperty $ do
                        ref <- newSearchLedger
                        _ <- guardDiscover ref (rejectWith d1) (write src)
                        out <- guardDiscover ref (rejectWith d2) (write src)
                        let note = repeatNote out
                            expected = if d1 == d2 then "unchanged" else "changed"
                        pure $
                            counterexample (T.unpack note) $
                                conjoin
                                    [ property (expected `T.isInfixOf` note)
                                    , property (not (d1 `T.isInfixOf` note))
                                    ]

    it "a commit is claimed only from the acknowledgement that states it" $
        property $
            forAll genSubstantiveSource $ \src ->
                forAll ((,) <$> genGhcDiagnostic <*> choose (1, 99)) $
                    \(diag, cid) -> ioProperty $ do
                        ref <- newSearchLedger
                        _ <- guardDiscover ref (rejectWith diag) (write src)
                        out <- guardDiscover ref (commitAs cid) (write src)
                        let note = repeatNote out
                        pure $
                            counterexample (T.unpack note) $
                                conjoin
                                    [ property (cellPhrase cid `T.isInfixOf` note)
                                    , property ("refused earlier" `T.isInfixOf` note)
                                    ]
    it "a success payload carrying a rejection is never called a commit" $
        property $
            forAll genSubstantiveSource $ \src ->
                forAll genGhcDiagnostic $ \diag -> ioProperty $ do
                    ref <- newSearchLedger
                    _ <- guardDiscover ref (rejectWith diag) (write src)
                    out <- guardDiscover ref (okButRefused diag) (write src)
                    let note = repeatNote out
                    pure $
                        counterexample (T.unpack note) $
                            conjoin
                                [ property (not ("committed" `T.isInfixOf` note))
                                , property ("unchanged" `T.isInfixOf` note)
                                ]

cellPhrase :: Int -> Text
cellPhrase cid = "cell " <> T.pack (show cid)

{- | The acknowledgement a landed write returns: 'Sabela.AI.WriteAck' reads a
cellId, a status and an execution block off it.
-}
commitAs :: Int -> ToolCall -> IO (Either Text ToolOutcome)
commitAs cid _ =
    pure . Right . ToolOk $
        object
            [ "cellId" .= cid
            , "status" .= ("completed" :: Text)
            , "execution" .= object ["ok" .= True]
            ]

-- | A refusal that arrived on the success constructor.
okButRefused :: Text -> ToolCall -> IO (Either Text ToolOutcome)
okButRefused diag _ =
    pure . Right . ToolOk $
        object
            [ "notCommitted" .= ("compile-gate" :: Text)
            , "diagnostic" .= diag
            ]

-- | Whitespace reflowings: the equivalence class 'normaliseSource' defines.
genReflow :: Text -> Gen Text
genReflow src = do
    pad <- elements ["  ", "\n", "\t", " \n "]
    lead <- elements ["", "  ", "\n"]
    pure (lead <> T.intercalate pad (T.words src))

write :: Text -> ToolCall
write src = ToolCall "insert_cell" (object ["source" .= src])

rejectWith :: Text -> ToolCall -> IO (Either Text ToolOutcome)
rejectWith diag _ =
    pure
        ( Right
            ( ToolErr
                ( object
                    [ "notCommitted" .= ("compile-gate" :: Text)
                    , "diagnostic" .= diag
                    ]
                )
            )
        )

repeatNote :: Either Text ToolOutcome -> Text
repeatNote (Right (ToolErr (Object o))) = textAt "repeatOf" o
repeatNote (Right (ToolOk (Object o))) = textAt "repeatOf" o
repeatNote _ = ""

namesRepeat :: Either Text ToolOutcome -> Bool
namesRepeat = not . T.null . repeatNote

textAt :: Text -> KM.KeyMap Value -> Text
textAt k o = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
