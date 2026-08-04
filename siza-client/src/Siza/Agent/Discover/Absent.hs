{-# LANGUAGE OverloadedStrings #-}

{- | What the Hackage index states for a package nothing installed can speak
for. The session and Hoogle databases hold installed packages, so without this
their silence about an absent one reads as the package not existing.
-}
module Siza.Agent.Discover.Absent (
    absentKnownHits,
    absentScopeNote,
    entryModule,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.CabalFacts (PkgFacts (..))
import Siza.Agent.Discover.Guidance (cabalLine)
import Siza.Agent.Discover.Interpret (stripVersion)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    Interpreted (..),
    MatchKind (..),
    Scope (..),
    SourceAnswer (..),
    shownModules,
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
        [ absentHit pkg (Just asked) MkModule (Just f)
        | iShape interp == "module"
        , let asked = iName interp
        , (pkg, f) <- hiFacts hk
        , asked `elem` pfModules f
        , pkg `notElem` answered
        ]
    answered = [dhPackage h | a <- answers, h <- saHits a]

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
absentScopeNote scope answers hk = case scPackage scope of
    Just p
        | Just f <- lookup p (hiFacts hk)
        , p `notElem` map fst (concatMap saPkgModules answers) ->
            Just
                ( "package="
                    <> p
                    <> " is not installed, so the session and Hoogle indexes \
                       \hold nothing from it; Hackage states it exposes "
                    <> T.pack (show (length (pfModules f)))
                    <> " modules — discover {package=\""
                    <> p
                    <> "\"} lists them"
                )
    _ -> Nothing
