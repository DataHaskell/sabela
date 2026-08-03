{-# LANGUAGE OverloadedStrings #-}

{- | W6C item 3: what the last line of an episode is allowed to claim. A reply
the harness never received leaves the notebook unknown; a refusal it read does
not. Only the turn budget makes a reply provably the last one read.
-}
module Test.UnconfirmedWriteSpec (unconfirmedWriteSpec) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.AI.WriteAck (refusalAck)
import Sabela.Api (errorJson)
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..))
import Siza.Agent.Check (CheckResult (..))
import Siza.Agent.Loop (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    runEpisodeSeeded,
 )
import Siza.Agent.Loop.WrapUp (
    BudgetView (..),
    budgetView,
    countUnconfirmed,
    finalTurnMarker,
    isWrapUpNudge,
    wrapUpDue,
    wrapUpMarker,
    wrapUpMsg,
 )
import Test.DiscoverFixtures (textField)

unconfirmedWriteSpec :: Spec
unconfirmedWriteSpec = describe "W6C: the last line reports the end state" $ do
    reflectedSpec
    unconfirmedSpec
    refusalSpec
    promiseSpec

assistant :: Text -> Value
assistant t = object ["role" .= ("assistant" :: Text), "content" .= t]

writeTurn :: Text -> Turn
writeTurn src =
    Turn (assistant "") "" [ToolCall "insert_cell" (object ["source" .= src])]

proseTurn :: Text -> Turn
proseTurn t = Turn (assistant t) t []

bump :: IORef Int -> IO Int
bump ref = atomicModifyIORef' ref (\n -> (n + 1, n + 1))

runWith ::
    ([Value] -> IO (Either Text Turn)) ->
    (ToolCall -> IO (Either Text ToolOutcome)) ->
    Int ->
    IO AgentRun
runWith chat dispatch maxRepairs =
    runEpisodeSeeded
        []
        (const (pure ()))
        GrammarOff
        EpisodeBudget{ebMaxRepairs = maxRepairs, ebDeadlineSecs = 1 / 0}
        Driver
            { drvChat = chat
            , drvDispatch = dispatch
            , drvNow = pure 0
            , drvVerify = const (pure (CheckFailed, Just "not yet"))
            }
        "sum the numbers"
        60

{- | Reproduces the reported episode shape: the budget runs out one round
after the model writes, and the final line must describe that write.
-}
reflectedSpec :: Spec
reflectedSpec = describe "a write on the last admitted turn" $
    it "is reflected in the final line when the repair budget ends" $ do
        turns <- newIORef (0 :: Int)
        cells <- newIORef (0 :: Int)
        let chat _ = do
                n <- bump turns
                pure . Right $
                    if odd n
                        then writeTurn ("total" <> tShow n <> " = 1 + 2")
                        else proseTurn "still working"
            dispatch tc = case tcName tc of
                "insert_cell" -> do
                    cid <- bump cells
                    pure . Right . ToolOk $
                        object
                            [ "cellId" .= cid
                            , "execution" .= object ["ok" .= True]
                            ]
                _ -> pure (Right (ToolOk (object [])))
        run <- runWith chat dispatch 2
        arStopped run `shouldBe` "repair_budget"
        arFinal run `shouldSatisfy` T.isInfixOf "2 cell(s) written"
        arFinal run `shouldSatisfy` not . T.isInfixOf "no cell was written"

{- | What a dispatched write can come back as. Only the two silent shapes
leave the harness without an answer about the notebook.
-}
data WriteEcho = TransportLost | BlindAck | GateRefusal | Landed
    deriving (Bounded, Enum, Eq, Show)

unheard :: WriteEcho -> Bool
unheard e = e == TransportLost || e == BlindAck

{- | The refusal shape production returns: 'Sabela.AI.WriteAck.refusalAck' is
what every compile-gate rejection is built with.
-}
echoOutcome :: Int -> WriteEcho -> Either Text ToolOutcome
echoOutcome _ TransportLost = Left "[infra] transport failure (<<timeout>>)"
echoOutcome _ BlindAck = Right (ToolOk (object ["queued" .= True]))
echoOutcome cid GateRefusal = Right (ToolErr (gateRejection "compile-gate" cid))
echoOutcome cid Landed =
    Right . ToolOk $
        object ["cellId" .= cid, "execution" .= object ["ok" .= True]]

gateRejection :: Text -> Int -> Value
gateRejection kind cid =
    refusalAck
        kind
        (Just cid)
        (errorJson "This candidate does not compile, so nothing was committed.")

writeStep :: Int -> WriteEcho -> (ToolCall, Either Text ToolOutcome)
writeStep cid e =
    ( ToolCall "insert_cell" (object ["source" .= ("total = 1 + 2" :: Text)])
    , echoOutcome cid e
    )

{- | The transport can time out after the server has applied the write, so an
unanswered write leaves the harness with no basis for either claim.
-}
unconfirmedSpec :: Spec
unconfirmedSpec = describe "a dispatched write that never came back" $ do
    it "is not reported as an empty notebook (transport error)" $ do
        run <- runWith (const (pure (Right (writeTurn "total = 1 + 2")))) errDispatch 20
        arStopped run `shouldBe` "kernel_error"
        assertHonest (arFinal run)
    it "is not reported as an empty notebook (ack without a cell)" $ do
        run <-
            runWith (const (pure (Right (writeTurn "total = 1 + 2")))) blindDispatch 20
        assertHonest (arFinal run)
    it "counts exactly the writes no answer came back for" $
        property $
            forAll (listOf (elements [minBound .. maxBound])) $ \echoes ->
                countUnconfirmed (zipWith writeStep [1 ..] echoes)
                    === length (filter unheard echoes)
  where
    errDispatch tc = case tcName tc of
        "insert_cell" -> pure (Left "<<timeout>>")
        _ -> pure (Right (ToolOk (object [])))
    blindDispatch tc = case tcName tc of
        "insert_cell" -> pure (Right (ToolOk (object ["queued" .= True])))
        _ -> pure (Right (ToolOk (object [])))
    assertHonest final = do
        final `shouldSatisfy` not . T.isInfixOf "no cell was written"
        final `shouldSatisfy` T.isInfixOf "unknown"

{- | A refusal the harness read is an observation: the payload states that
nothing was committed, so the empty notebook is known, not unknown.
-}
refusalSpec :: Spec
refusalSpec = describe "a write the server refused" $ do
    it "is never counted as unheard, for any refusal payload" $
        property $
            forAll (listOf genRefusal) $ \rs ->
                countUnconfirmed
                    [ ( ToolCall "insert_cell" (object ["source" .= src])
                      , Right (ToolErr v)
                      )
                    | (src, v) <- rs
                    ]
                    === 0
    it "leaves the final line saying no cell was written" $ do
        run <-
            runWith
                (const (pure (Right (writeTurn "total = 1 + 2"))))
                refuseDispatch
                20
        arFinal run `shouldSatisfy` T.isInfixOf "no cell was written"
        arFinal run `shouldSatisfy` not . T.isInfixOf "unknown"
    it "does not inflate the count when an unheard write sits beside it" $
        property $
            forAll (listOf (elements [GateRefusal, Landed])) $ \answered ->
                countUnconfirmed
                    (zipWith writeStep [1 ..] (TransportLost : answered))
                    === 1
  where
    refuseDispatch tc = case tcName tc of
        "insert_cell" -> pure (echoOutcome 1 GateRefusal)
        _ -> pure (Right (ToolOk (object [])))

genRefusal :: Gen (Text, Value)
genRefusal = do
    kind <- elements ["compile-gate", "trial-blocked", "pending-error"]
    cid <- oneof [pure Nothing, Just <$> choose (1, 50)]
    src <- elements ["total = 1 + 2", "x = undefinedName", ""]
    pure (src, refusalAck kind cid (errorJson "nothing was committed"))

{- | Only the turn budget can promise the next reply is the last one read: a
repair or time budget leaves the loop free to read many more.
-}
promiseSpec :: Spec
promiseSpec = describe "the wrap-up promises only what the loop guarantees" $ do
    it "claims finality exactly when the turn budget makes it true" $
        property $
            forAll genDue $ \bv ->
                claimsFinality (content bv) === (bvTurnsLeft bv <= 1)
    it "always names the budget that fired" $
        property $
            forAll genDue $ \bv ->
                counterexample (T.unpack (content bv)) $
                    "budget" `T.isInfixOf` content bv
    it "opens with the marker its own trigger admits" $
        property $
            forAll genDue $ \bv ->
                counterexample (T.unpack (content bv)) $
                    wrapUpMarker bv `T.isPrefixOf` content bv
    it "reserves the final-turn marker for the turn budget" $
        property $
            forAll genDue $ \bv ->
                (wrapUpMarker bv == finalTurnMarker) === (bvTurnsLeft bv <= 1)
    it "is a recognisable nudge under every trigger" $
        property $
            forAll genDue (isWrapUpNudge . content)
    it "every generated view really is a wrap-up trigger" $
        property $
            forAll genDue wrapUpDue
  where
    content = textField "content" . wrapUpMsg []

claimsFinality :: Text -> Bool
claimsFinality c = any (`T.isInfixOf` c) [finalTurnMarker, "last reply"]

{- | One generator per wrap-up trigger, so the repair and time arms always
carry turns to spare and the finality law has something to falsify.
-}
genDue :: Gen BudgetView
genDue = oneof [turnDue, repairDue, timeDue]
  where
    turnDue = do
        maxTurns <- choose (2, 200)
        left <- choose (0, 1)
        deadline <- choose (60, 3600)
        pure (budgetView maxTurns (maxTurns - left) 20 0 0 deadline)
    repairDue = do
        cap <- choose (1, 20)
        spent <- choose (max 1 (cap - 1), cap)
        pure (budgetView 500 0 cap spent 0 3600)
    timeDue = do
        deadline <- choose (60, 3600)
        elapsed <- choose (0.95 * deadline, deadline)
        pure (budgetView 500 0 20 0 elapsed deadline)

tShow :: Int -> Text
tShow = T.pack . show
