{-# LANGUAGE ScopedTypeVariables #-}

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

data Capability
    = ProcessExec
    | UnsafeIO
    | UnsafeCoerce
    | RawFileIO
    | EnvAccess
    | ForeignImport
    | Network
    deriving (Bounded, Enum, Eq, Ord, Show)

data Mode = Advise | Block
    deriving (Eq, Show)

data Policy = Policy
    { denied :: Set Capability
    , mode :: Mode
    }
    deriving (Eq, Show)

defaultDenied :: Set Capability
defaultDenied = S.fromList [minBound .. maxBound]

advisoryPolicy :: Policy
advisoryPolicy = Policy{denied = defaultDenied, mode = Advise}

strictPolicy :: Policy
strictPolicy = Policy{denied = defaultDenied, mode = Block}

scanModule :: Policy -> Hs.HsModule Hs.GhcPs -> [Diagnostic]
scanModule policy m =
    map (toDiagnostic (mode policy)) (filter denyHit (scanFindings m))
  where
    denyHit (cap, _, _) = cap `S.member` denied policy

scanSource :: Policy -> Text -> Either [Diagnostic] [Diagnostic]
scanSource policy src = scanModule policy <$> parseModuleE src

scanFindings :: Hs.HsModule Hs.GhcPs -> [(Capability, Maybe Int, Maybe Int)]
scanFindings m = importHits m <> foreignHits m <> callSiteHits m

importHits :: Hs.HsModule Hs.GhcPs -> [(Capability, Maybe Int, Maybe Int)]
importHits m =
    [ (cap, line, col)
    | li <- Hs.hsmodImports m
    , let modName = T.pack (Hs.moduleNameString (unLoc (Hs.ideclName (unLoc li))))
    , let (line, col) = cellSpanPos (getLocA li)
    , cap <- capabilityForModule modName
    ]

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

foreignHits :: Hs.HsModule Hs.GhcPs -> [(Capability, Maybe Int, Maybe Int)]
foreignHits m =
    [ (ForeignImport, line, col)
    | ld <- Hs.hsmodDecls m
    , let (line, col) = cellSpanPos (getLocA ld)
    , Hs.ForD{} <- [unLoc ld]
    ]

callSiteHits :: Hs.HsModule Hs.GhcPs -> [(Capability, Maybe Int, Maybe Int)]
callSiteHits m =
    [ (cap, Nothing, Nothing)
    | name <- S.toList (collectUses m)
    , Just cap <- [capabilityForName name]
    ]

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

toDiagnostic :: Mode -> (Capability, Maybe Int, Maybe Int) -> Diagnostic
toDiagnostic md (cap, line, col) =
    Diagnostic
        { dgSeverity = case md of Block -> Error; Advise -> Warning
        , dgLine = line
        , dgCol = col
        , dgMessage = "denied capability: " <> capabilityLabel cap
        }

capabilityLabel :: Capability -> Text
capabilityLabel = \case
    ProcessExec -> "ProcessExec (shelling out to the OS)"
    UnsafeIO -> "UnsafeIO (unsafePerformIO and friends)"
    UnsafeCoerce -> "UnsafeCoerce"
    RawFileIO -> "RawFileIO (raw file/handle/directory IO)"
    EnvAccess -> "EnvAccess (environment variables)"
    ForeignImport -> "ForeignImport (FFI)"
    Network -> "Network (sockets/HTTP)"
