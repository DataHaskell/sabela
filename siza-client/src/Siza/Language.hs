{- | The pre-flight capability seam: parse / security / annotate behind one
record. Each notebook language plugs in a 'Language'; the 'Diagnostic' it
emits is the common currency the 'Siza.Preflight' pipeline collects.

This is ordinary internal modularity for siza's own sake — one place that
hides @ghc-lib-parser@ from transport, the CLI, and provenance. It is not a
generalisation point for other notebooks (see the redesign's Part 8).
-}
module Siza.Language (
    Severity (..),
    Diagnostic (..),
    Language (..),
    renderDiagnostic,
) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Diagnostic severity. Errors fail pre-flight; warnings ride alongside.
data Severity = Error | Warning
    deriving (Eq, Show)

{- | A located finding from a pre-flight stage. @dgLine@/@dgCol@ are
1-based; 'Nothing' when the stage cannot attribute a position (a whole-cell
or import-level finding).
-}
data Diagnostic = Diagnostic
    { dgSeverity :: Severity
    , dgLine :: Maybe Int
    , dgCol :: Maybe Int
    , dgMessage :: Text
    }
    deriving (Eq, Show)

{- | A notebook language's pre-flight capability. 'langParse' is the only
field today; the security scan and the @:type@ annotate pull plug in here
without changing the seam.
-}
data Language = Language
    { langName :: Text
    , langParse :: Text -> Either [Diagnostic] ()
    }

-- | A compact one-line rendering, @line:col: severity: message@.
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
