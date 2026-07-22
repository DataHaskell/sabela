{-# LANGUAGE OverloadedStrings #-}

-- | Default-on Path-2 vetting for AI value-cell writes.
module Sabela.AI.Capabilities.Edit.ValueGate (prewriteValueVeto) where

import Data.Aeson ((.=))
import Data.Text (Text)

import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.Types (ToolOutcome, errOutcome)
import Sabela.Api (errorJsonWith)
import Sabela.Model (CellType (..))
import Sabela.Session.Query (
    TypecheckInput (..),
    TypecheckResult (..),
    classifyTypecheckInput,
    typecheckValueWith,
 )
import Sabela.SessionTypes (CellLang (..))
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), getHaskellSession)

-- | Hard AI write gate for exactly the proven value subset. Declaration forms
-- keep the established route; unavailable is explicit, never a false OK.
prewriteValueVeto :: App -> CellLang -> CellType -> Text -> IO (Maybe ToolOutcome)
prewriteValueVeto app Haskell CodeCell src = do
    enabled <- featureEnabled "SABELA_TYPECHECK_PRIMITIVE"
    if not enabled || classifyTypecheckInput src == OutsideValueSubset
        then pure Nothing
        else do
            mBackend <- getHaskellSession (appSessions app)
            case mBackend of
                Nothing -> pure (Just (refusal "unavailable" "live Haskell session unavailable"))
                Just backend -> do
                    result <-
                        typecheckValueWith
                            (ST.sbQueryType backend)
                            (ST.sbQueryBindings backend)
                            src
                    pure $
                        if tcSucceeded result
                            then Nothing
                            else Just (refusal "rejected" (tcDiagnostics result))
  where
    refusal :: Text -> Text -> ToolOutcome
    refusal verdict diagnostics =
        errOutcome $
            errorJsonWith
                ("Proposed value cell was not committed: " <> diagnostics)
                [ "typecheckMode" .= ("path2-value" :: Text)
                , "typecheckVerdict" .= verdict
                , "diagnostics" .= diagnostics
                ]
prewriteValueVeto _ _ _ _ = pure Nothing
