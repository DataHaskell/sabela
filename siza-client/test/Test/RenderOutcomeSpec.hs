{-# LANGUAGE OverloadedStrings #-}

module Test.RenderOutcomeSpec (renderOutcomeSpec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Tools (renderOutcome)

renderOutcomeSpec :: Spec
renderOutcomeSpec = describe "renderOutcome labels a ToolErr by what actually happened" $ do
    it "labels a compile-gate rejection as a code issue, not a tool failure" $
        startsWith "CODE ISSUE:" (renderOutcome (Right (ToolErr gateRejection)))

    it "labels any diagnostic verdict as a code issue" $
        startsWith "CODE ISSUE:" (renderOutcome (Right (ToolErr diagnosticOutcome)))

    it "labels a could-not-run verdict as not run, not a tool failure" $
        startsWith "NOT RUN:" (renderOutcome (Right (ToolErr couldNotRunOutcome)))

    it "labels a no-verdict-infra verdict as a tool error — the accurate case" $
        startsWith "TOOL ERROR:" (renderOutcome (Right (ToolErr infraOutcome)))

    it "falls back to tool error when there is no verdict at all" $
        startsWith "TOOL ERROR:" (renderOutcome (Right (ToolErr cellNotFound)))

    it "leaves ToolOk output unlabelled" $
        renderOutcome (Right (ToolOk (object ["cellId" .= (1 :: Int)])))
            `shouldSatisfy` (not . T.isInfixOf "TOOL ERROR")

startsWith :: Text -> Text -> Expectation
startsWith prefix actual = actual `shouldSatisfy` (prefix `T.isPrefixOf`)

gateRejection :: Value
gateRejection =
    object
        [ "notCommitted" .= ("compile-gate" :: Text)
        , "verdict" .= ("diagnostic" :: Text)
        , "diagnostic" .= ("Variable not in scope: foo" :: Text)
        ]

diagnosticOutcome :: Value
diagnosticOutcome =
    object ["verdict" .= ("diagnostic" :: Text), "outcome" .= ("timed_out" :: Text)]

couldNotRunOutcome :: Value
couldNotRunOutcome =
    object ["verdict" .= ("could-not-run" :: Text), "outcome" .= ("unavailable" :: Text)]

infraOutcome :: Value
infraOutcome = object ["verdict" .= ("no-verdict-infra" :: Text)]

cellNotFound :: Value
cellNotFound = object ["error" .= ("Cell not found: 4" :: Text)]
