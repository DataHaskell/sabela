{-# LANGUAGE ScopedTypeVariables #-}

{- | A capability policy over the parsed @ghc-lib-parser@ AST: a static
deny-list scan of a cell's imports, foreign declarations, and call sites.

DEFENCE IN DEPTH, NOT A SANDBOX. This scan reads generated code with a
parser before it runs and surfaces (or, in 'Block' mode, stops) an obvious
foot-gun — a shell-out, @unsafePerformIO@, a raw file write — at the cheapest
point. It is NOT a security boundary: the kernel still executes whatever a
'Siza.Preflight.Vetted' cell contains, and the real trust boundary for
untrusted code remains the hub proxy. Do not mistake an 'Advise' pass for a
safety guarantee.

The scan is robust to formatting because it walks the AST, not the text:
qualified, aliased, and hiding imports all resolve to the same capability,
and a call site is matched on its bare @OccName@ so a qualified
@System.Process.callCommand@ is caught even without the import.

KNOWN HOLES (stated so the 'careless generation' claim is not overstated):

  * A static scan cannot follow a re-export chain through a third module: it
    catches the direct import and the bare call, not a capability re-exported
    under a fresh name.
  * A Template Haskell splice that GENERATES a dangerous call (@$(genCall)@)
    hides it from the AST walk — the call does not exist until the splice
    runs. A TH quote with a visible name (@[| system "ls" |]@) IS caught.
  * The name table is a deny-LIST, not exhaustive: a capability reachable only
    through a name not listed here (a niche alias, a fresh @System.Posix.*@
    entry point) passes clean. New foot-guns are added to the table as found.
-}
module Siza.Security (
    Capability (..),
    Mode (..),
    Policy (..),
    advisoryPolicy,
    strictPolicy,
    defaultDenied,
    scanModule,
    scanFindings,
    scanSource,
) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import qualified GHC.Hs as Hs
import GHC.Parser.Annotation (getLocA)
import GHC.Types.SrcLoc (unLoc)
import Sabela.Parse.Ast (collectUses)

import Siza.Lang.Haskell (cellSpanPos, parseModuleE)
import Siza.Language (Diagnostic (..), Severity (Error, Warning))

-- | A dangerous capability the scan can detect in a cell.
data Capability
    = {- | @System.Process@ / @System.Posix.Process@; @system@, @callCommand@,
      @forkProcess@, @executeFile@
      -}
      ProcessExec
    | -- | @System.IO.Unsafe@, @unsafePerformIO@
      UnsafeIO
    | -- | @Unsafe.Coerce@, @unsafeCoerce@
      UnsafeCoerce
    | {- | raw file/handle/directory IO: @System.Directory@, @readFile@,
      @openFile@, @removeFile@, and friends (handle and path level)
      -}
      RawFileIO
    | -- | @System.Environment@, @getEnv@
      EnvAccess
    | -- | a @foreign import@ or @foreign export@ / FFI declaration
      ForeignImport
    | -- | @Network.*@ socket / HTTP imports
      Network
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | How a denied finding is handled: surfaced, or fatal to pre-flight.
data Mode = Advise | Block
    deriving (Eq, Show)

{- | The pre-flight security policy: the set of denied capabilities and how
a hit is handled. 'Advise' turns a hit into a warning diagnostic; 'Block'
makes it an error that fails pre-flight, so no 'Siza.Preflight.Vetted' is
produced.
-}
data Policy = Policy
    { denied :: Set Capability
    , mode :: Mode
    }
    deriving (Eq, Show)

-- | Every capability the scan knows about — the default deny-set.
defaultDenied :: Set Capability
defaultDenied = S.fromList [minBound .. maxBound]

-- | The default: deny everything dangerous, but only advise.
advisoryPolicy :: Policy
advisoryPolicy = Policy{denied = defaultDenied, mode = Advise}

-- | @--strict@: deny everything dangerous and block on a hit.
strictPolicy :: Policy
strictPolicy = Policy{denied = defaultDenied, mode = Block}

{- | Scan a parsed module against a policy. Returns the diagnostics for the
denied capabilities found; 'Error' severity in 'Block' mode (fatal to
pre-flight) and 'Warning' in 'Advise' mode (surfaced only).
-}
scanModule :: Policy -> Hs.HsModule Hs.GhcPs -> [Diagnostic]
scanModule policy m =
    map (toDiagnostic (mode policy)) (filter denyHit (scanFindings m))
  where
    denyHit (cap, _, _) = cap `S.member` denied policy

{- | Parse a cell and scan it: @Left ds@ on a syntax error, else the
policy's diagnostics. The all-in-one entry the CLI uses to surface advisories.
-}
scanSource :: Policy -> Text -> Either [Diagnostic] [Diagnostic]
scanSource policy src = scanModule policy <$> parseModuleE src

{- | Every capability the scan finds in a module, regardless of policy, with
its location. The policy-independent core, useful for provenance.
-}
scanFindings :: Hs.HsModule Hs.GhcPs -> [(Capability, Maybe Int, Maybe Int)]
scanFindings m = importHits m <> foreignHits m <> callSiteHits m

-- ---------------------------------------------------------------------------
-- Imports
-- ---------------------------------------------------------------------------

{- | Map each import to the capability its module grants, located at the
import line. Qualified, aliased, and hiding imports all resolve here; the
@as@ alias and the hiding list do not change the capability the module name
implies.
-}
importHits :: Hs.HsModule Hs.GhcPs -> [(Capability, Maybe Int, Maybe Int)]
importHits m =
    [ (cap, line, col)
    | li <- Hs.hsmodImports m
    , let modName = T.pack (Hs.moduleNameString (unLoc (Hs.ideclName (unLoc li))))
    , let (line, col) = cellSpanPos (getLocA li)
    , cap <- capabilityForModule modName
    ]

{- | The capability a module name implies, by exact name or prefix. A
notebook cell rarely imports more than one dangerous module, but a name can
map to more than one capability, so this returns a list.
-}
capabilityForModule :: Text -> [Capability]
capabilityForModule modName =
    [cap | (matches, cap) <- moduleTable, matches modName]
  where
    moduleTable =
        [ (under "System.Process", ProcessExec)
        , (under "System.Posix.Process", ProcessExec)
        , (is "System.IO.Unsafe", UnsafeIO)
        , (is "GHC.IO.Unsafe", UnsafeIO)
        , (is "Unsafe.Coerce", UnsafeCoerce)
        , (under "System.Directory", RawFileIO)
        , (under "System.Environment", EnvAccess)
        , (under "Foreign", ForeignImport)
        , (under "Network", Network)
        ]
    is name n = n == name
    under pfx n = n == pfx || (pfx <> ".") `T.isPrefixOf` n

-- ---------------------------------------------------------------------------
-- Foreign imports
-- ---------------------------------------------------------------------------

{- | Any @foreign@ declaration — both @foreign import@ and @foreign export@ —
is the 'ForeignImport' (FFI) capability. A @foreign export ccall@ exposes a
Haskell binding to C and is FFI surface just as an import is.
-}
foreignHits :: Hs.HsModule Hs.GhcPs -> [(Capability, Maybe Int, Maybe Int)]
foreignHits m =
    [ (ForeignImport, line, col)
    | ld <- Hs.hsmodDecls m
    , let (line, col) = cellSpanPos (getLocA ld)
    , Hs.ForD{} <- [unLoc ld]
    ]

-- ---------------------------------------------------------------------------
-- Call sites
-- ---------------------------------------------------------------------------

{- | Bare-name call sites, caught even with no matching import (a re-export
or a Prelude name). 'collectUses' yields each used identifier's bare
@OccName@, so a qualified @System.Process.system@ reduces to @system@ and is
matched here. The position is unknown at this granularity.
-}
callSiteHits :: Hs.HsModule Hs.GhcPs -> [(Capability, Maybe Int, Maybe Int)]
callSiteHits m =
    [ (cap, Nothing, Nothing)
    | name <- S.toList (collectUses m)
    , Just cap <- [capabilityForName name]
    ]

-- | The capability a bare call-site name implies.
capabilityForName :: Text -> Maybe Capability
capabilityForName name = lookup name nameTable
  where
    nameTable =
        [ ("system", ProcessExec)
        , ("callCommand", ProcessExec)
        , ("callProcess", ProcessExec)
        , ("readProcess", ProcessExec)
        , ("createProcess", ProcessExec)
        , ("spawnProcess", ProcessExec)
        , ("rawSystem", ProcessExec)
        , ("forkProcess", ProcessExec)
        , ("executeFile", ProcessExec)
        , ("unsafePerformIO", UnsafeIO)
        , ("unsafeDupablePerformIO", UnsafeIO)
        , ("unsafeInterleaveIO", UnsafeIO)
        , ("unsafeCoerce", UnsafeCoerce)
        , ("readFile", RawFileIO)
        , ("writeFile", RawFileIO)
        , ("appendFile", RawFileIO)
        , ("openFile", RawFileIO)
        , ("withFile", RawFileIO)
        , ("withBinaryFile", RawFileIO)
        , ("openBinaryFile", RawFileIO)
        , ("hGetContents", RawFileIO)
        , ("hPutStr", RawFileIO)
        , ("hPutStrLn", RawFileIO)
        , ("removeFile", RawFileIO)
        , ("renameFile", RawFileIO)
        , ("copyFile", RawFileIO)
        , ("removeDirectory", RawFileIO)
        , ("removeDirectoryRecursive", RawFileIO)
        , ("removePathForcibly", RawFileIO)
        , ("createDirectory", RawFileIO)
        , ("createDirectoryIfMissing", RawFileIO)
        , ("renameDirectory", RawFileIO)
        , ("getEnv", EnvAccess)
        , ("lookupEnv", EnvAccess)
        , ("getEnvironment", EnvAccess)
        , ("setEnv", EnvAccess)
        ]

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

{- | Render a located finding as a 'Diagnostic'. 'Block' mode makes it an
'Error' (fatal to pre-flight); 'Advise' makes it a 'Warning'.
-}
toDiagnostic :: Mode -> (Capability, Maybe Int, Maybe Int) -> Diagnostic
toDiagnostic md (cap, line, col) =
    Diagnostic
        { dgSeverity = case md of Block -> Error; Advise -> Warning
        , dgLine = line
        , dgCol = col
        , dgMessage = "denied capability: " <> capabilityLabel cap
        }

-- | A short human label for a capability, used in the diagnostic message.
capabilityLabel :: Capability -> Text
capabilityLabel = \case
    ProcessExec -> "ProcessExec (shelling out to the OS)"
    UnsafeIO -> "UnsafeIO (unsafePerformIO and friends)"
    UnsafeCoerce -> "UnsafeCoerce"
    RawFileIO -> "RawFileIO (raw file/handle/directory IO)"
    EnvAccess -> "EnvAccess (environment variables)"
    ForeignImport -> "ForeignImport (FFI)"
    Network -> "Network (sockets/HTTP)"
