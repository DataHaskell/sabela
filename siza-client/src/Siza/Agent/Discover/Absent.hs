{-# LANGUAGE OverloadedStrings #-}

{- | What the Hackage index states for a package nothing installed can speak
for. The session and Hoogle databases hold installed packages, so without this
their silence about an absent one reads as the package not existing.
-}
module Siza.Agent.Discover.Absent (
    absentKnownHits,
    absentScopeNote,
    entryModule,
    withIndexFacts,
) where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.ModuleResolve (namesFragment)
import Siza.Agent.Discover.CabalFacts (PkgFacts (..))
import Siza.Agent.Discover.Guidance (absentScopePackage, cabalLine)
import Siza.Agent.Discover.Interpret (stripVersion)
import Siza.Agent.Discover.ModuleList (shownModules)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    Interpreted (..),
    MatchKind (..),
    Scope (..),
    SourceAnswer (..),
 )

{- | The hits the index alone supports: the package a query names, and the
package that exposes a module nothing installed does.
-}
absentKnownHits :: Interpreted -> [SourceAnswer] -> HackageInfo -> [DHit]
absentKnownHits interp answers hk = namedPackage ++ moduleOwners
  where
    {- Version and type stay uncomputed rather than stood in for; the modules
    and homepage are the index's own words. -}
    namedPackage =
        [ absentHit pkg (entryModule facts) MkExact facts
        | iShape interp `elem` ["name", "package"]
        , let pkg = stripVersion (iName interp)
        , pkg `elem` hiKnown hk
        , pkg `notElem` answered
        , let facts = lookup pkg (hiFacts hk)
        ]
    moduleOwners =
        [ absentHit pkg (Just m) MkModule (Just f)
        | iShape interp == "module"
        , (pkg, f) <- hiFacts hk
        , pkg `notElem` answered
        , m <- namedModules (iName interp) f
        ]
    answered = [dhPackage h | a <- answers, h <- saHits a]

{- | The modules a module-shaped query names in a package: the one it names
exactly, or — failing that — the real names whose namespace it opens. A fragment
is never handed back as though it were a module the package exposes.
-}
namedModules :: Text -> PkgFacts -> [Text]
namedModules asked f
    | asked `elem` pfModules f = [asked]
    | otherwise = shownModules f{pfModules = matched}
  where
    matched = [m | m <- pfModules f, namesFragment asked m]

absentHit :: Text -> Maybe Text -> MatchKind -> Maybe PkgFacts -> DHit
absentHit pkg mMod kind facts =
    DHit
        { dhName = fromMaybe pkg mMod
        , dhType = ""
        , dhModule = fromMaybe "" mMod
        , dhPackage = pkg
        , dhVersion = ""
        , dhInstall = InstAbsentKnown
        , dhKind = kind
        , dhOrigin = "hackage"
        , dhCabal = Just (cabalLine pkg)
        , dhUse = ("import " <>) <$> mMod
        , dhClash = Nothing
        , dhFacts = facts
        }

{- | What the index states about the package a hit already stands for, so a
Hoogle answer naming one does not cost the caller its only description. Where
the session can speak it is the authority, and the index stays silent.
-}
withIndexFacts :: HackageInfo -> DHit -> DHit
withIndexFacts hk h
    | dhName h /= dhPackage h = h
    | dhInstall h `notElem` [InstAbsentKnown, InstAbsentUnknown] = h
    | Just f <- lookup (dhPackage h) (hiFacts hk) =
        withEntryModule (entryModule (Just f)) h{dhFacts = dhFacts h <|> Just f}
    | otherwise = h

{- | The entry point, where the hit states no module of its own. A blank module
is unknown, so filling it states what the index states and nothing more.
-}
withEntryModule :: Maybe Text -> DHit -> DHit
withEntryModule (Just m) h
    | T.null (dhModule h) =
        h{dhModule = m, dhUse = dhUse h <|> Just ("import " <> m)}
withEntryModule _ h = h

{- | The module a caller reaches for first, when the package states one that
stands above the rest. A package with several roots names no entry point, so
none is invented for it.
-}
entryModule :: Maybe PkgFacts -> Maybe Text
entryModule facts = case facts of
    Nothing -> Nothing
    Just f -> case shownModules f of
        (m : rest) | all (T.isPrefixOf (m <> ".")) rest -> Just m
        _ -> Nothing

{- | What a scope on a package no local index covers states. The session and
Hoogle databases hold installed packages, so their silence about one that is
not installed is the reach of the index, never the package lacking the name.
-}
absentScopeNote :: Scope -> [SourceAnswer] -> HackageInfo -> Maybe Text
absentScopeNote scope answers hk = do
    p <- absentScopePackage scope answers hk
    f <- lookup p (hiFacts hk)
    pure (maybe (packageNote p f) (moduleNote p) (scModule scope))
  where
    packageNote p f =
        "package="
            <> p
            <> " is not installed, so the session and Hoogle indexes \
               \hold nothing from it; Hackage states it exposes "
            <> T.pack (show (length (pfModules f)))
            <> " modules — discover {package=\""
            <> p
            <> "\"} lists them"
    {- A module scope names a package as surely as a package scope does. Left
    unsaid, the caller reads the empty result as the module lacking the name
    and varies the query, which is the loop the hodatime episode ran. -}
    moduleNote p m =
        "module="
            <> m
            <> " is exposed by "
            <> p
            <> ", which is not installed, so the session and Hoogle \
               \indexes hold nothing from it — "
            <> cabalLine p
            <> " makes it searchable"
