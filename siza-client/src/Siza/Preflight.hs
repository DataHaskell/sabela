{- | The 'Vetted' seam: source that has passed pre-flight, and the only way
to obtain it.

'Vetted' has no exported data constructor. The sole producer is 'preflight',
so an unparseable or policy-failing cell cannot be sent as a mutation by
construction (redesign 6.4). The mutation transport ('replaceCellSource',
'insertCell', 'proposeEdit') takes a 'Vetted', not a raw 'Text'.

'preflight' is parse then scan: it parses the cell to its AST (a local
syntax error, with no kernel round-trip) and then runs the 'Policy' capability
scan over that AST. The scan is defence in depth, NOT a sandbox — see
'Siza.Security'.
-}
module Siza.Preflight (
    Vetted,
    vettedSource,
    Policy (..),
    Mode (..),
    advisoryPolicy,
    strictPolicy,
    preflight,
    replaceCellSource,
    insertCell,
    proposeEdit,
) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Sabela.AI.Capabilities.ToolName (
    ToolName (InsertCell, ProposeEdit, ReplaceCellSource),
 )
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Lang.Haskell (parseModuleE)
import Siza.Language (Diagnostic, Severity (Error), dgSeverity)
import Siza.Security (
    Mode (..),
    Policy (..),
    advisoryPolicy,
    scanModule,
    strictPolicy,
 )
import Siza.Transport (Conn, callTool)

{- | Source that has passed pre-flight. The constructor is unexported: the
only way to obtain a 'Vetted' is 'preflight', so a raw 'Text' cannot reach
a mutation by construction.
-}
newtype Vetted = Vetted Text

-- | Read back the vetted source (e.g. for logging). There is no way in.
vettedSource :: Vetted -> Text
vettedSource (Vetted t) = t

{- | Run pre-flight: parse the source, then run the 'Policy' security scan
over its AST. @Left ds@ when the source does not parse, or when a 'Block'
policy finds a denied capability; @Right Vetted@ otherwise. In 'Advise' mode
findings are 'Warning' diagnostics that never fail the gate.
-}
preflight :: Policy -> Text -> IO (Either [Diagnostic] Vetted)
preflight policy src =
    pure $ case parseModuleE src of
        Left ds -> Left ds
        Right m ->
            let findings = scanModule policy m
                fatal = filter ((== Error) . dgSeverity) findings
             in if null fatal then Right (Vetted src) else Left fatal

-- ---------------------------------------------------------------------------
-- Mutation transport: only a Vetted may be sent
-- ---------------------------------------------------------------------------

{- | @replace_cell_source@. Takes a 'Vetted', so un-vetted text is
unrepresentable at the call site.
-}
replaceCellSource ::
    Conn -> Text -> Int -> Maybe Text -> Vetted -> IO (Either Text ToolOutcome)
replaceCellSource conn base cellId mhash (Vetted src) =
    callTool conn base ReplaceCellSource $
        withHash mhash $
            object ["cell_id" .= cellId, "source" .= src]

-- | @insert_cell@. Takes a 'Vetted'.
insertCell ::
    Conn -> Text -> Maybe Int -> Text -> Vetted -> IO (Either Text ToolOutcome)
insertCell conn base mafter kind (Vetted src) =
    callTool conn base InsertCell $
        object $
            ["kind" .= kind, "source" .= src]
                <> maybe [] (\a -> ["after_cell_id" .= a]) mafter

-- | @propose_edit@. Takes a 'Vetted'.
proposeEdit ::
    Conn -> Text -> Int -> Maybe Text -> Vetted -> IO (Either Text ToolOutcome)
proposeEdit conn base cellId mhash (Vetted src) =
    callTool conn base ProposeEdit $
        withHash mhash $
            object ["cell_id" .= cellId, "source" .= src]

-- | Splice an optional @expected_hash@ into a tool-input object.
withHash :: Maybe Text -> Value -> Value
withHash Nothing v = v
withHash (Just h) (A.Object o) = A.Object (KM.insert "expected_hash" (A.String h) o)
withHash _ v = v
