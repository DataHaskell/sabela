{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | The @read_source@ executor: resolve the owning package and a version,
obtain the sdist, find the module file, and answer with the definition's
source or the module's outline, always stating the release it read.
-}
module Sabela.AI.Capabilities.ReadSource (
    execReadSource,
    readSourceOutcome,
    VersionSource (..),
    versionSourceText,
    resolveVersion,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)

import Sabela.AI.Capabilities.CapabilityApi (hoogleFor)
import Sabela.AI.Capabilities.ReadSource.Miss (missJson, severalOwners)
import Sabela.AI.HackageFacts (factsVersion, moduleOwners)
import Sabela.AI.HoogleClient (HoogleHit (..))
import Sabela.AI.ReadSourceArgs (
    ReadSourceReq (..),
    parseReadSourceArgs,
    readSourceCallText,
 )
import Sabela.AI.Sdist (acquireSdist, cachedVersions)
import Sabela.AI.Sdist.Locate (LocateMiss (..), locateModuleFile)
import Sabela.AI.SourceLocate.Imports (aliasesJson)

import Sabela.AI.SourceLocate (
    DeclSlice (..),
    Located (..),
    Outline (..),
    declSlice,
    exportsName,
    importedModules,
    moduleOutline,
    nearest,
 )
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.Api (errorJson, errorJsonWith)
import Sabela.State (App (..))

execReadSource :: App -> Value -> IO ToolOutcome
execReadSource app = readSourceOutcome (appHttpMgr app)

-- | Content past this is cut at a line boundary and disclosed as cut.
sourceCharCap :: Int
sourceCharCap = 8000

-- | An outline names this many definitions before it counts the rest.
outlineDeclCap :: Int
outlineDeclCap = 120

data VersionSource = VsRequested | VsFactsIndex | VsHoogleDocs | VsCabalCache
    deriving (Eq, Show)

versionSourceText :: VersionSource -> Text
versionSourceText = \case
    VsRequested -> "requested"
    VsFactsIndex -> "facts-index"
    VsHoogleDocs -> "hoogle-docs"
    VsCabalCache -> "cabal-cache"

{- | The disclosed version ladder: the caller's word, then the facts index,
then the release hoogle documented, then the newest sdist cabal has cached.
-}
resolveVersion ::
    ReadSourceReq ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    Maybe (Text, VersionSource)
resolveVersion req facts hoogle cached =
    pick VsRequested (rsVersion req)
        `orElse` pick VsFactsIndex facts
        `orElse` pick VsHoogleDocs hoogle
        `orElse` pick VsCabalCache (listToMaybe cached)
  where
    pick tag = fmap (,tag)
    orElse (Just x) _ = Just x
    orElse Nothing y = y

readSourceOutcome :: Maybe Manager -> Value -> IO ToolOutcome
readSourceOutcome mMgr input = case parseReadSourceArgs input of
    Left e -> pure (errOutcome (errorJson e))
    Right req -> do
        ePkg <- resolvePackage req
        case ePkg of
            Left err -> pure (errOutcome err)
            Right pkg -> do
                eVer <- versionFor req pkg
                case eVer of
                    Left err -> pure (errOutcome err)
                    Right ver -> answer mMgr req pkg ver

-- | The owning package: the caller's word, or the facts index's one owner.
resolvePackage :: ReadSourceReq -> IO (Either Value Text)
resolvePackage req = case rsPackage req of
    Just pkg -> pure (Right pkg)
    Nothing -> do
        owners <- moduleOwners (rsModule req)
        pure $ case map fst owners of
            [pkg] -> Right pkg
            [] ->
                Left . errorJson $
                    "no package in the Hackage facts index exposes `"
                        <> rsModule req
                        <> "`; find the owner with discover {module: \""
                        <> rsModule req
                        <> "\"}"
            pkgs -> Left (severalOwners (rsModule req) pkgs)

versionFor ::
    ReadSourceReq -> Text -> IO (Either Value (Text, VersionSource))
versionFor req pkg = do
    facts <- factsVersion pkg
    case resolveVersion req facts Nothing [] of
        Just v -> pure (Right v)
        Nothing -> do
            hoogle <- hoogleVersion pkg (rsModule req)
            cached <- cachedVersions pkg
            pure $ case resolveVersion req facts hoogle cached of
                Just v -> Right v
                Nothing ->
                    Left . errorJson $
                        "Hackage knows `"
                            <> pkg
                            <> "` but no release could be determined; \
                               \pass it: "
                            <> readSourceCallText
                                [ ("module", rsModule req)
                                , ("version", "<release>")
                                ]

-- | Best-effort: the release the hoogle index documented, or nothing.
hoogleVersion :: Text -> Text -> IO (Maybe Text)
hoogleVersion pkg m = do
    r <- try (hoogleFor pkg m) :: IO (Either SomeException [HoogleHit])
    pure $ case r of
        Right hits -> listToMaybe [hhVersion h | h <- hits, not (T.null (hhVersion h))]
        Left _ -> Nothing

answer ::
    Maybe Manager ->
    ReadSourceReq ->
    Text ->
    (Text, VersionSource) ->
    IO ToolOutcome
answer mMgr req pkg (ver, vsrc) = do
    eBytes <- acquireSdist mMgr pkg ver
    pure $ case eBytes of
        Left e -> errOutcome (errorJsonWith e ["package" .= pkg, "version" .= ver])
        Right (_, bytes) -> case locateModuleFile bytes (rsModule req) of
            Left miss -> errOutcome (missJson req pkg ver miss)
            Right (path, src) ->
                render req pkg ver vsrc (stripSdistDir pkg ver path) src

render ::
    ReadSourceReq -> Text -> Text -> VersionSource -> Text -> Text -> ToolOutcome
render req pkg ver vsrc path src = case rsName req of
    Just name -> case declSlice src name of
        Left _
            | exportsName src name ->
                errOutcome $
                    errorJsonWith
                        ( "`"
                            <> name
                            <> "` is a re-export: "
                            <> rsModule req
                            <> " ("
                            <> pkg
                            <> "-"
                            <> ver
                            <> ") exports it but one of its imports defines \
                               \it — retry with one of `imports`; add \
                               \`package` only when the defining module \
                               \belongs to this package"
                        )
                        ["imports" .= take 8 (importedModules src)]
        Left cands ->
            errOutcome $
                errorJsonWith
                    ( "`"
                        <> name
                        <> "` is not a top-level name in "
                        <> rsModule req
                        <> " ("
                        <> pkg
                        <> "-"
                        <> ver
                        <> ")"
                    )
                    ["candidates" .= cands]
        Right s ->
            okOutcome . object $
                common (dsHow s)
                    <> [ "name" .= name
                       , "lines"
                            .= object ["from" .= dsFrom s, "to" .= dsTo s]
                       ]
                    <> capped (dsText s)
                    <> aliasPairs (dsText s)
    Nothing ->
        okOutcome . object $
            common (oHow outline)
                <> [ "header" .= oHeader outline
                   , "decls" .= map declJson shown
                   , "shown" .= length shown
                   , "count" .= length (oDecls outline)
                   ]
                <> aliasPairs
                    (oHeader outline <> T.unlines [t | (_, _, Just t) <- shown])
  where
    outline = moduleOutline src
    shown = take outlineDeclCap (oDecls outline)
    aliasPairs shownText =
        maybe [] (\v -> ["aliases" .= v]) (aliasesJson src shownText)
    declJson (n, l, mSig) =
        object $
            ["name" .= n, "line" .= l] <> ["type" .= s | Just s <- [mSig]]
    common how =
        [ "source" .= ("hackage-sdist" :: Text)
        , "package" .= pkg
        , "version" .= ver
        , "versionSource" .= versionSourceText vsrc
        , "module" .= rsModule req
        , "path" .= path
        , "located" .= locatedText how
        , "note" .= note how
        ]
    note how =
        pkg
            <> "-"
            <> ver
            <> " as released on Hackage; a newer release may differ."
            <> scannedNote how
    scannedNote Scanned =
        " The file did not parse (CPP or an unsupported extension); spans \
        \are lexical and may include neighbouring lines."
    scannedNote Parsed = ""

locatedText :: Located -> Text
locatedText Parsed = "parsed"
locatedText Scanned = "scanned"

-- | Content, cut whole-line at the cap and disclosed as cut when it was.
capped :: Text -> [Pair]
capped t
    | T.length t <= sourceCharCap = ["content" .= t]
    | otherwise =
        [ "content" .= T.unlines (cutLines (T.lines t))
        , "truncated" .= True
        ]
  where
    cutLines = go 0
      where
        go _ [] = []
        go n (l : ls)
            | n + T.length l + 1 > sourceCharCap = []
            | otherwise = l : go (n + T.length l + 1) ls

stripSdistDir :: Text -> Text -> FilePath -> Text
stripSdistDir pkg ver path =
    let p = T.pack path
        prefix = pkg <> "-" <> ver <> "/"
     in fromMaybe p (T.stripPrefix prefix p)
