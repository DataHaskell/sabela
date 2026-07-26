{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.CellResult (
    Diagnostic,
    AbortReason (..),
    CellOutcome (..),
    CellResult (..),
    CellId,
    OwnedCells,
    okCellResult,
    notebookHealthy,
    toCellResult,
    toToolOutcome,
    mergeToolOk,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value (..),
    object,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
 )
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair, Parser)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Types (
    ExecutionResult (..),
    ToolOutcome (..),
 )
import Sabela.Model (CellError, OutputItem)

type Diagnostic = CellError

data AbortReason = Interrupted | Superseded | TimedOut
    deriving (Eq, Show)

data CellOutcome
    = Succeeded
    | Raised !Text
    | Rejected ![Diagnostic]
    | Aborted !AbortReason
    deriving (Eq, Show)

data CellResult = CellResult
    { crOutcome :: CellOutcome
    , crOutputs :: [OutputItem]
    , crWarnings :: [Diagnostic]
    }
    deriving (Eq, Show)

okCellResult :: CellResult -> Bool
okCellResult cr = crOutcome cr == Succeeded

type CellId = Int

type OwnedCells = Set CellId

notebookHealthy :: [CellResult] -> Bool
notebookHealthy = all okCellResult

toCellResult :: Either Text ExecutionResult -> [OutputItem] -> CellResult
toCellResult res outputs =
    CellResult (outcomeOf res) outputs (warningsOf res)
  where
    outcomeOf (Left msg) = Aborted (abortReason msg)
    outcomeOf (Right er)
        | Just e <- erError er = Raised e
        | not (null (erErrors er)) = Rejected (erErrors er)
        | otherwise = Succeeded
    warningsOf (Left _) = []
    warningsOf (Right er) = erWarnings er

abortReason :: Text -> AbortReason
abortReason msg
    | msg == "Cancelled" = Interrupted
    | "Request superseded" `T.isPrefixOf` msg = Superseded
    | "Cell execution timed out" `T.isPrefixOf` msg = TimedOut
    | otherwise = Interrupted

toToolOutcome :: CellResult -> ToolOutcome
toToolOutcome = ToolOk . toJSON

mergeToolOk :: CellResult -> [Pair] -> ToolOutcome
mergeToolOk cr extra = case toToolOutcome cr of
    ToolOk (Object o) -> ToolOk (Object (KM.union o (KM.fromList extra)))
    other -> other

instance ToJSON AbortReason where
    toJSON Interrupted = "Interrupted"
    toJSON Superseded = "Superseded"
    toJSON TimedOut = "TimedOut"

instance FromJSON AbortReason where
    parseJSON = withText "AbortReason" $ \t -> case t of
        "Interrupted" -> pure Interrupted
        "Superseded" -> pure Superseded
        "TimedOut" -> pure TimedOut
        _ -> fail ("unknown abort reason: " <> show t)

instance ToJSON CellOutcome where
    toJSON Succeeded = object ["tag" .= ("Succeeded" :: Text)]
    toJSON (Raised msg) =
        object ["tag" .= ("Raised" :: Text), "message" .= msg]
    toJSON (Rejected ds) =
        object ["tag" .= ("Rejected" :: Text), "errors" .= ds]
    toJSON (Aborted r) =
        object ["tag" .= ("Aborted" :: Text), "reason" .= r]

instance FromJSON CellOutcome where
    parseJSON = withObject "CellOutcome" $ \o -> do
        tag <- o .: "tag" :: Parser Text
        case tag of
            "Succeeded" -> pure Succeeded
            "Raised" -> Raised <$> o .: "message"
            "Rejected" -> Rejected . fromMaybe [] <$> o .:? "errors"
            "Aborted" -> Aborted <$> o .: "reason"
            _ -> fail ("unknown outcome tag: " <> show tag)

instance ToJSON CellResult where
    toJSON cr =
        object
            [ "outcome" .= crOutcome cr
            , "outputs" .= crOutputs cr
            , "warnings" .= crWarnings cr
            , "ok" .= okCellResult cr
            ]

instance FromJSON CellResult where
    parseJSON = withObject "CellResult" $ \o -> do
        outcome <- o .: "outcome"
        outputs <- o .: "outputs"
        warnings <- o .: "warnings"
        pure (CellResult outcome outputs warnings)
