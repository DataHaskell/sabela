{- |
Technique: client parse/security gate [Gating/Repair].
Guarantee: only a 'Vetted' (constructor unexported) can be submitted to the wire.
Entry: 'preflight'. Trap: Siza.Agent.ProviderCheck is an unrelated Ollama reachability check.
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

newtype Vetted = Vetted Text

vettedSource :: Vetted -> Text
vettedSource (Vetted t) = t

preflight :: Policy -> Text -> IO (Either [Diagnostic] Vetted)
preflight policy src =
    pure $ case parseModuleE src of
        Left ds -> Left ds
        Right m ->
            let findings = scanModule policy m
                fatal = filter ((== Error) . dgSeverity) findings
             in if null fatal then Right (Vetted src) else Left fatal

replaceCellSource ::
    Conn -> Text -> Int -> Maybe Text -> Vetted -> IO (Either Text ToolOutcome)
replaceCellSource conn base cellId mhash (Vetted src) =
    callTool conn base ReplaceCellSource $
        withHash mhash $
            object ["cell_id" .= cellId, "source" .= src]

insertCell ::
    Conn -> Text -> Maybe Int -> Text -> Vetted -> IO (Either Text ToolOutcome)
insertCell conn base mafter kind (Vetted src) =
    callTool conn base InsertCell $
        object $
            ["kind" .= kind, "source" .= src]
                <> maybe [] (\a -> ["after_cell_id" .= a]) mafter

proposeEdit ::
    Conn -> Text -> Int -> Maybe Text -> Vetted -> IO (Either Text ToolOutcome)
proposeEdit conn base cellId mhash (Vetted src) =
    callTool conn base ProposeEdit $
        withHash mhash $
            object ["cell_id" .= cellId, "source" .= src]

withHash :: Maybe Text -> Value -> Value
withHash Nothing v = v
withHash (Just h) (A.Object o) = A.Object (KM.insert "expected_hash" (A.String h) o)
withHash _ v = v
