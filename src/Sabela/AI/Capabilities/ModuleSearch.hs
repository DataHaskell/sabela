{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.ModuleSearch (
    execFindFunction,
    usableCard,
    resolveNameToModules,
    interesting,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isUpper)
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.BrowseCard (browseCardFor)
import Sabela.AI.Capabilities.ModuleCard (
    browseFailed,
    hiddenModuleCard,
    hiddenPackageCard,
    matchesOutcomeWithDocs,
 )
import Sabela.AI.Capabilities.Resolve (lookupByName)
import Sabela.AI.Capabilities.Util (fieldText)
import Sabela.AI.Capability (
    Capability (..),
    Hit (..),
    defaultSynonyms,
    parseCapabilities,
    searchCapabilities,
 )
import Sabela.AI.RepairDispatch (DiagClass (ClassHiddenPackage), diagClassText)
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.Api (errorJson)
import Sabela.Model (Cell (..), Notebook (..))
import Sabela.SessionTypes (SessionBackend (..))
import Sabela.State (App (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)
import System.Environment (lookupEnv)

execFindFunction :: App -> Value -> IO ToolOutcome
execFindFunction app input =
    if T.null q
        then
            pure
                ( errOutcome
                    (errorJson "query required (a keyword or a module name)")
                )
        else do
            mBackend <- getHaskellSession (appSessions app)
            case mBackend of
                Nothing -> storeOnlyAnswer q
                Just backend -> do
                    mods <- sbQueryComplete backend "import "
                    if scopedToModule || not (q `elem` mods || looksLikeModule q)
                        then keywordSearch backend mods
                        else do
                            raw <- sbQueryBrowse backend q
                            let card = browseCardFor (Just q) q raw
                            if browseUnusable raw card
                                then
                                    hiddenModuleCard (Just q) q
                                        >>= maybe (keywordSearch backend mods) (pure . okOutcome)
                                else pure (okOutcome card)
  where
    keywordSearch backend _mods
        | not (T.null qModule) && qModule /= q = do
            caps <- buildIndex backend [qModule]
            case searchCapabilities defaultSynonyms caps q of
                [] -> moduleExports backend qModule q
                hits -> matchesOutcomeWithDocs backend q hits
    keywordSearch backend mods = do
        surfacing <- isJust <$> lookupEnv "SABELA_INSTANCE_SURFACING"
        builtin <- filter interesting <$> sbQueryComplete backend "import Sabela"
        let baseMods = filter interesting mods
        toBrowse <-
            if surfacing
                then do
                    nss <- notebookNamespaces app
                    extra <- namespaceSubmodules backend nss
                    pure (nub (baseMods ++ filter interesting extra))
                else pure (take maxIndexModules baseMods)
        caps <- buildIndex backend (nub (builtin ++ toBrowse))
        let hits = searchCapabilities defaultSynonyms caps q
        if any (namesExactly q) hits
            then matchesOutcomeWithDocs backend q hits
            else
                hiddenPackageCard q
                    >>= maybe
                        (matchesOutcomeWithDocs backend q hits)
                        (pure . okOutcome)
    q =
        let qq = fieldText "query" input
         in if T.null qq then fieldText "module" input else qq
    qModule = fieldText "module" input
    -- A module the caller named outranks a query that merely looks like one:
    -- `{module: Bluefin.State, query: State}` must browse Bluefin.State, not
    -- resolve `State` to whichever package's namesake sorts first.
    scopedToModule = not (T.null qModule) && qModule /= q

resolveNameToModules :: App -> Text -> IO [Capability]
resolveNameToModules app name = do
    mBackend <- getHaskellSession (appSessions app)
    case mBackend of
        Nothing -> pure []
        Just backend -> do
            builtin <- sbQueryComplete backend "import Sabela"
            nss <- notebookNamespaces app
            extra <- namespaceSubmodules backend nss
            let mods = take maxIndexModules (nub (filter interesting (builtin ++ extra)))
            caps <- buildIndex backend mods
            pure (lookupByName name caps)

looksLikeModule :: Text -> Bool
looksLikeModule t =
    not (T.any (== ' ') t)
        && maybe False (isUpper . fst) (T.uncons t)

moduleExports :: SessionBackend -> Text -> Text -> IO ToolOutcome
moduleExports backend modName q = do
    raw <- sbQueryBrowse backend modName
    let card = browseCardFor (Just q) modName raw
    if browseUnusable raw card
        then
            maybe (missOutcome modName q) (okOutcome . noteMiss q)
                <$> hiddenModuleCard (Just q) modName
        else pure (okOutcome (noteMiss q card))

{- | Record that the query matched no export — unless the card being annotated
in fact exports it. The session index can miss where the store card hits, and
"no export matched 'State'" beside `State :: State s e` reads as a denial.
-}
noteMiss :: Text -> Value -> Value
noteMiss q v@(Object o)
    | exportsMention q o = v
    | otherwise =
        Object (KM.insert "matched" (String ("no export matched '" <> q <> "'")) o)
noteMiss _ v = v

exportsMention :: Text -> KM.KeyMap Value -> Bool
exportsMention q o = case KM.lookup "exports" o of
    Just (Array es) -> any mentions es
    _ -> False
  where
    mentions (String s) = q `T.isInfixOf` s
    mentions _ = False

missOutcome :: Text -> Text -> ToolOutcome
missOutcome modName q =
    okOutcome
        ( object
            [ "module" .= modName
            , "matched" .= ("no export matched '" <> q <> "'")
            , "exports" .= ([] :: [Text])
            ]
        )

{- | What the store can answer without a kernel. Package and module facts come
from ghc-pkg, which needs no live session, so gating them behind one reported an
installed package as absent whenever the kernel was cold.
-}
storeOnlyAnswer :: Text -> IO ToolOutcome
storeOnlyAnswer q = do
    mPkg <- hiddenPackageCard q
    case mPkg of
        Just c -> pure (okOutcome c)
        Nothing -> maybe noSession okOutcome <$> hiddenModuleCard (Just q) q
  where
    noSession =
        errOutcome
            (errorJson "No live Haskell session — run a cell first to start GHCi.")

usableCard :: Value -> Bool
usableCard (Object o) = case KM.lookup "status" o of
    Just (String st) -> st `notElem` ["error", diagClassText ClassHiddenPackage]
    _ -> False
usableCard _ = False

{- | A browse that cannot answer: the probe printed a diagnostic rather than a
listing, or the card names a wall. One recognizer serves the store probe too, so
the surfaces cannot disagree about whether an error page is a listing.
-}
browseUnusable :: Text -> Value -> Bool
browseUnusable raw card = isJust (browseFailed raw) || not (usableCard card)

maxIndexModules :: Int
maxIndexModules = 120

notebookNamespaces :: App -> IO [Text]
notebookNamespaces app = do
    nb <- readNotebook (appNotebook app)
    let imports =
            [ m
            | c <- nbCells nb
            , l <- T.lines (cellSource c)
            , Just m <- [importedModule l]
            ]
    pure (nub (map (T.takeWhile (/= '.')) imports))

importedModule :: Text -> Maybe Text
importedModule line = case T.words (T.strip line) of
    ("import" : rest) -> case dropWhile (== "qualified") rest of
        (m : _) | not (T.null m) && isUpper (T.head m) -> Just m
        _ -> Nothing
    _ -> Nothing

namespaceSubmodules :: SessionBackend -> [Text] -> IO [Text]
namespaceSubmodules backend namespaces =
    concat <$> mapM (\ns -> sbQueryComplete backend ("import " <> ns)) namespaces

buildIndex :: SessionBackend -> [Text] -> IO [Capability]
buildIndex backend mods =
    concat <$> mapM (\m -> parseCapabilities m <$> sbQueryBrowse backend m) mods

interesting :: Text -> Bool
interesting m = T.takeWhile (/= '.') m `notElem` baseNamespaces

baseNamespaces :: [Text]
baseNamespaces =
    [ "GHC"
    , "Data"
    , "Control"
    , "System"
    , "Text"
    , "Type"
    , "Foreign"
    , "Numeric"
    , "Prelude"
    , "Debug"
    , "Unsafe"
    , "Language"
    ]

{- | Ground truth about an installed package outranks a fuzzy leaf match. An
exact symbol name does not: `blue` must not answer for `bluefin`, but `map`
must still answer for `map`.
-}
namesExactly :: Text -> Hit -> Bool
namesExactly q h = capName (hitCap h) == q
