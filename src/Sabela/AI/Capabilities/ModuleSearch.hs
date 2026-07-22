{-# LANGUAGE OverloadedStrings #-}

{- | The @find_function@ tool: a Hoogle-style search over the live session's
installed modules. A keyword query ranks every exposed function with
"Sabela.AI.Capability"; an exact module-name query returns that module's raw
@:browse@ listing (all exports — values, types, classes — not just value bindings).
-}
module Sabela.AI.Capabilities.ModuleSearch (
    execFindFunction,
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

import Sabela.AI.Capabilities.BrowseCard (browseCard)
import Sabela.AI.Capabilities.Resolve (lookupByName)
import Sabela.AI.Capabilities.Util (fieldText)
import Sabela.AI.Capability (
    Capability (..),
    Hit (..),
    Match (..),
    defaultSynonyms,
    parseCapabilities,
    searchCapabilities,
 )
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.Api (errorJson)
import Sabela.Model (Cell (..), Notebook (..))
import Sabela.SessionTypes (SessionBackend (..))
import Sabela.State (App (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)
import System.Environment (lookupEnv)

{- | @query@ is a keyword (@animate@) or a module name (which returns that
module's raw exports). Ranks the installed modules' functions; returns nothing
on a true miss — no misleading near-match. To search by TYPE use @find_by_type@.
-}
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
                Nothing ->
                    pure
                        ( errOutcome
                            (errorJson "No live Haskell session — run a cell first to start GHCi.")
                        )
                Just backend -> do
                    mods <- sbQueryComplete backend "import "
                    -- A module-name query browses that module DIRECTLY — :browse works
                    -- on any installed module even when absent from the import-completion
                    -- list. Applicability is decided by TRYING the oracle: any
                    -- module-shaped query attempts :browse, and only a usable card is
                    -- kept — an unusable one falls back to keyword search.
                    if q `elem` mods || looksLikeModule q
                        then do
                            raw <- sbQueryBrowse backend q
                            let card = browseCard q raw
                            if browseEmpty raw || not (usableCard card)
                                then keywordSearch backend mods
                                else pure (okOutcome card)
                        else keywordSearch backend mods
  where
    keywordSearch backend _mods
        | not (T.null qModule) && qModule /= q = do
            -- module+query together SCOPE the keyword search to that module
            -- instead of being silently ignored (measured: a refinement call
            -- had zero effect and no signal).
            caps <- buildIndex backend [qModule]
            pure (matchesOutcome q (searchCapabilities defaultSynonyms caps q))
    keywordSearch backend mods = do
        surfacing <- isJust <$> lookupEnv "SABELA_INSTANCE_SURFACING"
        let baseMods = filter interesting mods
        -- ON: also index the submodules of the session's own namespaces (e.g.
        -- DataFrame.*), enumerated via prefixed :complete — the bare import-completion
        -- caps at ~250 and omits RE-EXPORTED modules, so the generic `fit` is otherwise
        -- undiscoverable by keyword. OFF: the capped bare list (the status-quo baseline).
        toBrowse <-
            if surfacing
                then do
                    nss <- notebookNamespaces app
                    extra <- namespaceSubmodules backend nss
                    pure (nub (baseMods ++ filter interesting extra))
                else pure (take maxIndexModules baseMods)
        caps <- buildIndex backend toBrowse
        pure (matchesOutcome q (searchCapabilities defaultSynonyms caps q))
    q =
        let qq = fieldText "query" input
         in if T.null qq then fieldText "module" input else qq
    qModule = fieldText "module" input

{- | Installed modules exporting @name@, over the same bounded browse-index
@find_function@ uses. Lets the add-import repair resolve an unimported name
(e.g. @Picture@) without the model naming its module.
-}
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

{- | An uppercase-headed token with no spaces — a module-shaped query worth
ATTEMPTING to browse (@DataFrame.Model@, but also dotless umbrella modules
like @DataFrame@ or @Granite@). The oracle decides: an unusable browse falls
back to keyword search, so a capitalized non-module costs one cheap query.
-}
looksLikeModule :: Text -> Bool
looksLikeModule t =
    not (T.any (== ' ') t)
        && maybe False (isUpper . fst) (T.uncons t)

-- | A card the model can act on; an @error@ card means fall back instead.
usableCard :: Value -> Bool
usableCard (Object o) = KM.lookup "status" o /= Just (String "error")
usableCard _ = False

-- | A @:browse@ that resolved to nothing usable (empty or a scope/parse error).
browseEmpty :: Text -> Bool
browseEmpty raw =
    let s = T.toLower (T.strip raw)
     in T.null s || "not in scope" `T.isInfixOf` s || "error:" `T.isInfixOf` s

{- | Cap on modules browsed per keyword search (the OFF / baseline arm), to bound
the round-trip cost. Surfacing ON lifts it to index every importable module.
-}
maxIndexModules :: Int
maxIndexModules = 120

{- | The top-level namespaces the NOTEBOOK imports (the first dotted component of
each @import@ in any cell — e.g. @DataFrame@ from @import DataFrame@). The bare
import-completion only lists boot/base modules, so these come from the notebook's
own imports, not from completion. No module names are hardcoded.
-}
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

-- | The module a one-line @import [qualified] M ...@ brings in, if any.
importedModule :: Text -> Maybe Text
importedModule line = case T.words (T.strip line) of
    ("import" : rest) -> case dropWhile (== "qualified") rest of
        (m : _) | not (T.null m) && isUpper (T.head m) -> Just m
        _ -> Nothing
    _ -> Nothing

{- | Submodules under each given namespace, enumerated via prefixed @:complete@
(e.g. @import DataFrame@ → every @DataFrame.*@). A namespace-prefixed completion
returns RE-EXPORTED modules the bare list omits, so @find_function@ can discover
a re-exported verb like @fit@.
-}
namespaceSubmodules :: SessionBackend -> [Text] -> IO [Text]
namespaceSubmodules backend namespaces =
    concat <$> mapM (\ns -> sbQueryComplete backend ("import " <> ns)) namespaces

{- | Browse each module once and parse its value bindings into capabilities: the
per-call index the keyword search ranks over.
-}
buildIndex :: SessionBackend -> [Text] -> IO [Capability]
buildIndex backend mods =
    concat <$> mapM (\m -> parseCapabilities m <$> sbQueryBrowse backend m) mods

{- | Keep the discovery-relevant modules: drop the base/boot namespaces the model
already knows, by first path component (keeps @DataFrame.*@, @Granite.*@,
@Sabela.Notebook.*@ and declared deps; drops @Data.*@, @Control.*@, @GHC.*@).
-}
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

-- | Shape ranked hits into @{query, matches: [{module, name, type, via}]}@.
matchesOutcome :: Text -> [Hit] -> ToolOutcome
matchesOutcome q hits =
    okOutcome $ object ["query" .= q, "matches" .= map hitJSON hits]
  where
    hitJSON h =
        object
            [ "module" .= capModule (hitCap h)
            , "name" .= capName (hitCap h)
            , "type" .= capType (hitCap h)
            , "via" .= matchName (hitVia h)
            ]

matchName :: Match -> Text
matchName ByName = "name"
matchName ByType = "type"
matchName BySynonym = "synonym"
matchName ByModule = "module"
