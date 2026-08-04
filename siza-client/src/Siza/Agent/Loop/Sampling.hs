{- | Rejection sampling on a write. When a tool call that writes source comes
back red, re-ask the model up to @k-1@ times against grounded context and keep
the first replacement that lands healthy; if none does, the original stands.
-}
module Siza.Agent.Loop.Sampling (
    dispatchCall,
) where

import Control.Monad (void, when)
import Data.Aeson (Value)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.CellResult (CellId)
import Sabela.AI.Types (ToolOutcome)
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..))
import Siza.Agent.Loop.Support (
    callActs,
    groundingMsgs,
    replaceCall,
    sampleK,
    writeSource,
 )
import Siza.Agent.Loop.Types (Driver (..))
import Siza.Agent.Owned (ownedCellOutcome)
import Siza.Agent.Sample (SampleVerify (..), sampleVerifyOne)
import Siza.Agent.Stack (StackSession)
import Siza.Agent.Stack.Call (CallResult (..), runToolCall)

dispatchCall :: StackSession -> Driver -> [Value] -> ToolCall -> IO CallResult
dispatchCall sess driver msgs call = do
    k <- sampleK
    if k > 1 && callActs call && isJust (writeSource call)
        then rejectionDispatch driver msgs k call
        else runToolCall sess (drvDispatch driver) call

rejectionDispatch :: Driver -> [Value] -> Int -> ToolCall -> IO CallResult
rejectionDispatch driver msgs k call = do
    o0 <- drvDispatch driver call
    case ownedCellOutcome call o0 of
        Just (cid, False) -> do
            ground <-
                groundingMsgs
                    (drvDispatch driver)
                    (fromMaybe "" (writeSource call))
            let msgs' = msgs ++ ground
            winRef <- newIORef Nothing
            let sv =
                    SampleVerify
                        { svSample = const (fromMaybe "" <$> reAskSource driver msgs')
                        , svRollout = rolloutReplace driver winRef cid
                        , svInsert = const (pure ())
                        }
            _ <- sampleVerifyOne (k - 1) sv
            mWin <- readIORef winRef
            case mWin of
                Just win -> pure win
                Nothing -> restoreOriginal driver cid call o0
        _ -> pure (CallResult call o0 [])

rolloutReplace ::
    Driver -> IORef (Maybe CallResult) -> CellId -> Text -> IO Bool
rolloutReplace driver winRef cid src
    | T.null (T.strip src) = pure False
    | otherwise = do
        let rc = replaceCall cid src
        o <- drvDispatch driver rc
        let ok = maybe False snd (ownedCellOutcome rc o)
        when ok (writeIORef winRef (Just (CallResult rc o [])))
        pure ok

restoreOriginal ::
    Driver -> CellId -> ToolCall -> Either Text ToolOutcome -> IO CallResult
restoreOriginal driver cid call o0 = do
    _ <-
        maybe
            (pure ())
            (void . drvDispatch driver . replaceCall cid)
            (writeSource call)
    pure (CallResult call o0 [])

reAskSource :: Driver -> [Value] -> IO (Maybe Text)
reAskSource driver msgs = do
    r <- drvChat driver msgs
    pure $ case r of
        Right t ->
            listToMaybe
                [s | c <- turnCalls t, callActs c, Just s <- [writeSource c]]
        Left _ -> Nothing
