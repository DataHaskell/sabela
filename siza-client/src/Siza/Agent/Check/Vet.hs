{-# LANGUAGE OverloadedStrings #-}

{- | Compile-vetting for covering checks (C2 task 1): a proposal is trialled
off-notebook BEFORE the user is asked to accept it. live_test19 displayed
@length ys == 629@ for acceptance and only discovered at run time that it does
not compile, which reads to the user as a test that ran.
-}
module Siza.Agent.Check.Vet (
    vetCheckWith,
    vetProposal,
) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome)
import Siza.Agent.Check (CheckResult (..), classifyCheck, markerSrc)
import Siza.Agent.Tools (renderOutcome)

-- | How a tool call is made; injected so vetting is testable without a server.
type Call = ToolName -> Value -> IO (Either Text ToolOutcome)

{- | Trial a proposed check through @try@. It runs the same marker the live run
would, so the vet and the run cannot disagree about whether the check compiles.
-}
vetCheckWith :: Call -> Text -> IO CheckResult
vetCheckWith call check
    | T.null (T.strip check) = pure CheckNotApplicable
    | otherwise = do
        out <-
            renderOutcome
                <$> call
                    Try
                    (object ["code" .= markerSrc check, "language" .= ("Haskell" :: Text)])
        pure (classifyCheck out)

{- | Drop a proposal that cannot compile, returning the empty check so the
caller falls through to its no-check path. The discard is disclosed: a silently
dropped check would leave the user believing one was offered.
-}
vetProposal :: Call -> Text -> IO Text
vetProposal call proposed = do
    verdict <- vetCheckWith call proposed
    if verdict == CheckUncheckable
        then do
            TIO.putStrLn
                ("  \9888 discarded a check that does not compile: " <> proposed)
            pure ""
        else pure proposed
