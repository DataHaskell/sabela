{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.ErrorIndex (
    GhcCode (..),
    renderGhcCode,
    errorIndexUrl,
    Remediation (..),
    remediationFor,
    ErrorInfo (..),
    errorInfoOf,
    errorInfoForCell,
    errorInfoPairs,
    withErrorInfo,
) where

import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.List (nubBy)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.CellResult (CellOutcome (..), CellResult (..))
import Sabela.Model (CellError (..))

newtype GhcCode = GhcCode Int
    deriving (Eq, Ord, Show)

renderGhcCode :: GhcCode -> Text
renderGhcCode (GhcCode n) =
    "GHC-" <> T.justifyRight 5 '0' (T.pack (show n))

errorIndexUrl :: GhcCode -> Text
errorIndexUrl c =
    "https://errors.haskell.org/messages/" <> renderGhcCode c <> "/"

data Remediation
    = AutoRepaired Text
    | ModelCandidates
    | ManualFix
    deriving (Eq, Show)

remediationFor :: GhcCode -> Remediation
remediationFor (GhcCode n) = case n of
    87110 -> AutoRepaired "dependency"
    35235 -> AutoRepaired "dependency"
    88464 -> ModelCandidates
    39999 -> ModelCandidates
    83865 -> ManualFix
    59692 -> ManualFix
    40910 -> ManualFix
    _ -> ManualFix

data ErrorInfo = ErrorInfo
    { eiCode :: GhcCode
    , eiReference :: Text
    , eiRemediation :: Remediation
    }
    deriving (Eq, Show)

errorInfoOf :: CellError -> Maybe ErrorInfo
errorInfoOf ce = do
    n <- ceCode ce
    let code = GhcCode n
    pure (ErrorInfo code (errorIndexUrl code) (remediationFor code))

errorInfoForCell :: CellResult -> [ErrorInfo]
errorInfoForCell = dedupByCode . mapMaybe errorInfoOf . rejectedDiags . crOutcome
  where
    rejectedDiags (Rejected ds) = ds
    rejectedDiags _ = []
    dedupByCode = nubBy (\a b -> eiCode a == eiCode b)

errorInfoPairs :: [ErrorInfo] -> [Pair]
errorInfoPairs [] = []
errorInfoPairs eis = ["diagnostics" .= eis]

withErrorInfo :: CellResult -> Value -> Value
withErrorInfo cr v = case (v, errorInfoForCell cr) of
    (Object o, eis@(_ : _)) -> Object (KM.insert "diagnostics" (toJSON eis) o)
    _ -> v

instance ToJSON ErrorInfo where
    toJSON ei =
        object
            [ "code" .= renderGhcCode (eiCode ei)
            , "reference" .= eiReference ei
            , "remediation" .= eiRemediation ei
            ]

instance ToJSON Remediation where
    toJSON (AutoRepaired f) =
        object ["kind" .= ("automatic" :: Text), "fixer" .= f]
    toJSON ModelCandidates = object ["kind" .= ("candidates" :: Text)]
    toJSON ManualFix = object ["kind" .= ("manual" :: Text)]
