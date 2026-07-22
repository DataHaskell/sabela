{-# LANGUAGE OverloadedStrings #-}

{- | The @eval_live@ tool: evaluate a single, read-only Haskell expression
against the LIVE notebook kernel and return its type and (for a pure, Showable
expression) its value. The piece scratchpad cannot do — it sees the notebook's
in-scope bindings (a cell's @df@) that the isolated scratchpad session cannot.

Guarded to stay read-only: the input must bind no top-level name (so nothing
persists in the session), and an @IO@-typed expression returns its type only,
never run. A timeout bounds a pure-but-diverging expression.
-}
module Sabela.AI.Capabilities.EvalLive (execEvalLive) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value, object, (.=))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)

import Sabela.AI.Capabilities.Util (fieldText)
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.Api (errorJson)
import Sabela.Parse (cellNames)
import Sabela.SessionTypes (SessionBackend (..))
import Sabela.State (App (..))
import Sabela.State.SessionManager (getHaskellSession)

{- | How long a read-only probe may run before it is abandoned, so a pure but
diverging expression (@length [1..]@) cannot wedge the tool.
-}
evalTimeoutMicros :: Int
evalTimeoutMicros = 30 * 1000000

-- | Cap on returned value text so a huge structure does not flood context.
valueCap :: Int
valueCap = 4000

execEvalLive :: App -> Value -> IO ToolOutcome
execEvalLive app input
    | T.null expr =
        pure
            ( errOutcome
                ( errorJson
                    "expression required (one Haskell expression, e.g. columnAsList \"x\" df)"
                )
            )
    | Just name <- boundName expr =
        pure
            ( errOutcome
                ( errorJson
                    ( "eval_live evaluates ONE expression and must define no name (got '"
                        <> name
                        <> "'). Use scratchpad to define helpers, or insert_cell to add a cell."
                    )
                )
            )
    | otherwise = do
        mBackend <- getHaskellSession (appSessions app)
        case mBackend of
            Nothing ->
                pure
                    ( errOutcome
                        (errorJson "No live Haskell session — run a cell first to start GHCi.")
                    )
            Just backend -> probe backend
  where
    expr = T.strip (fieldText "expression" input)
    probe backend = do
        ty <- T.strip <$> sbQueryType backend expr
        if not (typeResolved ty)
            then pure (okOutcome (object ["expression" .= expr, "error" .= ty]))
            else
                if isIOType ty
                    then pure (okOutcome (object (base ty <> [ioNote])))
                    else evalValue backend ty
    evalValue backend ty = do
        r <- timeout evalTimeoutMicros (tryRun backend)
        pure (okOutcome (object (base ty <> resultField r)))
    resultField r = case r of
        Nothing ->
            ["error" .= ("evaluation timed out (30s); kernel_restart if wedged" :: Text)]
        Just (Left e) -> ["error" .= T.pack (show (e :: SomeException))]
        Just (Right (out, err))
            | not (T.null (T.strip err)) -> ["error" .= cap err]
            | otherwise -> ["value" .= cap out]
    tryRun backend =
        try (sbRunBlock backend expr) :: IO (Either SomeException (Text, Text))
    base ty = ["expression" .= expr, "type" .= ty]
    ioNote =
        "note"
            .= ( "IO-typed — not evaluated (eval_live is read-only). Use scratchpad to run effects." ::
                    Text
               )
    cap t =
        let s = T.strip t
         in if T.length s > valueCap then T.take valueCap s <> "…(truncated)" else s
    boundName e = case S.toList (fst (cellNames e)) of
        (n : _) -> Just n
        [] -> Nothing

-- | True when @:type@ actually resolved (not a not-in-scope / error reply).
typeResolved :: Text -> Bool
typeResolved t =
    let lt = T.toLower t
     in not (T.null (T.strip t))
            && not ("not in scope" `T.isInfixOf` lt)
            && not ("error:" `T.isInfixOf` lt)

-- | Whether the resolved type's head (after any @=>@ context) is @IO@.
isIOType :: Text -> Bool
isIOType ty =
    let rhs = T.strip (snd (T.breakOnEnd "::" ty))
        afterCtx = T.strip (snd (T.breakOnEnd "=>" rhs))
     in "IO " `T.isPrefixOf` afterCtx || afterCtx == "IO"
