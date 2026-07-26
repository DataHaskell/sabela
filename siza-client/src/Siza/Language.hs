module Siza.Language (
    Severity (..),
    Diagnostic (..),
    Language (..),
    renderDiagnostic,
) where

import Data.Text (Text)
import qualified Data.Text as T

data Severity = Error | Warning
    deriving (Eq, Show)

data Diagnostic = Diagnostic
    { dgSeverity :: Severity
    , dgLine :: Maybe Int
    , dgCol :: Maybe Int
    , dgMessage :: Text
    }
    deriving (Eq, Show)

data Language = Language
    { langName :: Text
    , langParse :: Text -> Either [Diagnostic] ()
    }

renderDiagnostic :: Diagnostic -> Text
renderDiagnostic d =
    T.concat [pos, sev, ": ", dgMessage d]
  where
    sev = case dgSeverity d of
        Error -> "error"
        Warning -> "warning"
    pos = case (dgLine d, dgCol d) of
        (Just l, Just c) -> T.pack (show l) <> ":" <> T.pack (show c) <> ": "
        (Just l, Nothing) -> T.pack (show l) <> ": "
        _ -> ""
