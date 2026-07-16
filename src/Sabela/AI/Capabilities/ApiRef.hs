{-# LANGUAGE OverloadedStrings #-}

{- | The @api_reference@ tool. Given a module substring, name, or type, it
gathers signatures from three LOCAL sources — the offline @hoogle@ CLI
('Sabela.AI.HoogleResolve'), the live session's @:browse@ ('sbQueryBrowse'), and
typed-hole fits ('sbQueryHoleFits') — and merges them, preferring the dynamic
results. The curated static card ('sliceApiReference') is the offline FALLBACK,
used only when every dynamic source is empty. Hoogle + the static card need no
live session, so the tool still answers before any cell has run; @:browse@ and
hole fits enrich the result when a kernel exists.
-}
module Sabela.AI.Capabilities.ApiRef (
    execApiReference,

    -- * Pure merge core
    ApiRefSources (..),
    ArgKind (..),
    argKind,
    mergeApiRef,
) where

import Data.Aeson (Value, object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Util (fieldText)
import Sabela.AI.HoogleResolve (HoogleHit (..), hoogleQuery)
import Sabela.AI.ReferenceCard (sliceApiReference)
import Sabela.AI.Types (ToolOutcome, okOutcome)
import Sabela.SessionTypes (SessionBackend (..))
import Sabela.State (App (..))
import Sabela.State.SessionManager (getHaskellSession)

-- | How many Hoogle hits to pull before scoping to the requested module/name.
apiRefHoogleK :: Int
apiRefHoogleK = 40

-- | Whether the arg reads as a type (route to hole fits) or a module/name.
data ArgKind = AsType | AsModuleOrName
    deriving (Eq, Show)

argKind :: Text -> ArgKind
argKind t0
    | "->" `T.isInfixOf` t || "::" `T.isInfixOf` t || "_" `T.isPrefixOf` t = AsType
    | otherwise = AsModuleOrName
  where
    t = T.strip t0

{- | The four inputs 'mergeApiRef' folds into one reference: scoped Hoogle hits,
the live @:browse@ text, the typed-hole-fits text, and the static-card slice.
The @Maybe Text@ live sources are 'Nothing' when no kernel was available.
-}
data ApiRefSources = ApiRefSources
    { arsArg :: Text
    , arsHoogle :: [HoogleHit]
    , arsBrowse :: Maybe Text
    , arsHoles :: Maybe Text
    , arsStatic :: Text
    }

execApiReference :: App -> Value -> IO ToolOutcome
execApiReference app input = do
    let arg = T.strip (apiRefArg input)
        static = sliceApiReference arg
    if T.null arg
        then pure (okOutcome (mergeApiRef (ApiRefSources "" [] Nothing Nothing static)))
        else do
            hoogle <- scopedHoogle arg
            mBackend <- getHaskellSession (appSessions app)
            (browse, holes) <-
                maybe (pure (Nothing, Nothing)) (gatherLive arg) mBackend
            pure (okOutcome (mergeApiRef (ApiRefSources arg hoogle browse holes static)))

-- | The tool arg: the @module@ field, falling back to a bare @query@.
apiRefArg :: Value -> Text
apiRefArg input =
    let m = fieldText "module" input
     in if T.null m then fieldText "query" input else m

{- | Local Hoogle hits, scoped to the arg for a module/name query (Hoogle ranks
across the ecosystem, so keep only hits in a matching module or name); a type
query keeps Hoogle's own type ranking untouched.
-}
scopedHoogle :: Text -> IO [HoogleHit]
scopedHoogle arg = do
    hits <- hoogleQuery apiRefHoogleK arg
    pure $ case argKind arg of
        AsType -> hits
        AsModuleOrName -> filter scoped hits
  where
    needle = T.toLower arg
    scoped h =
        needle `T.isInfixOf` T.toLower (hhModule h)
            || needle `T.isInfixOf` T.toLower (hhName h)

{- | Live-session enrichment: a type arg asks for typed-hole fits; a single-token
module/name arg is @:browse@d directly (a multi-word keyword is left to Hoogle).
-}
gatherLive :: Text -> SessionBackend -> IO (Maybe Text, Maybe Text)
gatherLive arg backend = case argKind arg of
    AsType -> do
        fits <- sbQueryHoleFits backend (asHole arg)
        pure (Nothing, Just fits)
    AsModuleOrName -> do
        raw <-
            if T.any (== ' ') arg
                then pure Nothing
                else Just <$> sbQueryBrowse backend arg
        pure (raw, Nothing)
  where
    asHole g = if "_" `T.isPrefixOf` g then g else "_ :: " <> g

{- | Merge the sources into one reference value. Prefer the dynamic sources
(Hoogle / @:browse@ / hole fits); use the static card only when all are empty.
-}
mergeApiRef :: ApiRefSources -> Value
mergeApiRef s
    | not (hasDynamic s) =
        object
            [ "module" .= arsArg s
            , "source" .= ("static" :: Text)
            , "reference" .= arsStatic s
            ]
    | otherwise =
        object $
            ["module" .= arsArg s, "source" .= ("live" :: Text)]
                <> ["hoogle" .= map hoogleJSON hs | let hs = arsHoogle s, not (null hs)]
                <> ["exports" .= b | Just b <- [usableText (arsBrowse s)]]
                <> ["holeFits" .= h | Just h <- [usableText (arsHoles s)]]

hasDynamic :: ApiRefSources -> Bool
hasDynamic s =
    not (null (arsHoogle s))
        || present (arsBrowse s)
        || present (arsHoles s)
  where
    present = isJust . usableText

hoogleJSON :: HoogleHit -> Value
hoogleJSON h =
    object
        [ "name" .= hhName h
        , "module" .= hhModule h
        , "package" .= hhPackage h
        , "type" .= hhType h
        ]

{- | A live-source text stripped, or 'Nothing' when it is blank or a
@:browse@/hole-fits failure (a not-in-scope / error line), so a wall counts as
"no dynamic result" and the static fallback wins.
-}
usableText :: Maybe Text -> Maybe Text
usableText Nothing = Nothing
usableText (Just t)
    | T.null s || any (`T.isInfixOf` s) unusable = Nothing
    | otherwise = Just (T.strip t)
  where
    s = T.toLower (T.strip t)
    -- A @:browse@ of an un-imported (hidden) package, or any diagnostic, is not
    -- a usable reference — fall back to the curated card instead of shipping it.
    unusable =
        [ "not in scope"
        , "error:"
        , "could not load module"
        , "hidden package"
        , "\"severity\":\"error\""
        ]
