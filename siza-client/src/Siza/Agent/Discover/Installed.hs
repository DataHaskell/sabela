{-# LANGUAGE OverloadedStrings #-}

{- | What the session established about a module, and about which package. An
install state is a claim about this session, so it is only ever as strong as
the session's own evidence, and evidence recorded for one package says nothing
about a hit that names another.
-}
module Siza.Agent.Discover.Installed (
    SessionFact (..),
    moduleState,
    packageState,
    sessionFacts,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Types (
    DHit (..),
    InstallState (..),
    SourceAnswer (..),
 )

-- | A module the session reached, the package it reached it under, and how.
data SessionFact = SessionFact
    { sfModule :: Text
    , sfPackage :: Text
    , sfState :: InstallState
    }
    deriving (Eq, Show)

{- | The session's own answers, read as facts. A hit the session returned for a
hidden package is evidence that the module is present but not loaded, which is
a weaker fact than a module it browsed.
-}
sessionFacts :: [SourceAnswer] -> [SessionFact]
sessionFacts answers = hitFacts ++ cardFacts
  where
    hitFacts =
        [ SessionFact (dhModule h) (dhPackage h) (stateOf h)
        | a <- answers
        , saSource a == "session"
        , h <- saHits a
        , not (T.null (dhModule h))
        ]
    stateOf h
        | dhInstall h == InstHidden = InstHidden
        | otherwise = InstInstalled
    cardFacts =
        [ SessionFact m (textAt "package" o) InstInstalled
        | a <- answers
        , Just (Object o) <- [saCard a]
        , Just (String "ok") <- [KM.lookup "status" o]
        , Just (String m) <- [KM.lookup "module" o]
        , not (T.null m)
        ]

-- | The state the session established for this hit's own module.
moduleState :: [(Text, [Text])] -> [SessionFact] -> DHit -> Maybe InstallState
moduleState pkgModules facts h
    | T.null (dhModule h) = Nothing
    | otherwise =
        strongest
            [ sfState f
            | f <- facts
            , sfModule f == dhModule h
            , attributable (owners pkgModules (sfModule f)) (sfPackage f) (dhPackage h)
            ]

{- | The state the session established for a package, through a module an index
attributes to it. The attribution is the index's, so it carries no more than
the evidence for that module did, and only for the package it names.
-}
packageState ::
    [SessionFact] -> [(Text, [Text])] -> DHit -> Maybe InstallState
packageState facts pkgModules h
    | T.null (dhPackage h) = Nothing
    | otherwise =
        strongest
            [ sfState f
            | (p, mods) <- pkgModules
            , p == dhPackage h
            , m <- mods
            , f <- facts
            , sfModule f == m
            , attributable (owners pkgModules m) (sfPackage f) p
            ]

{- | Evidence speaks for a hit when they name the same package, or when the hit
names none. A fact recorded under no package reaches the hit through its
module's owner, and a module two packages claim has no owner to reach it by.
-}
attributable :: [Text] -> Text -> Text -> Bool
attributable moduleOwners evidence hit
    | evidence == hit = True
    | T.null hit = True
    | T.null evidence = length moduleOwners <= 1
    | otherwise = False

-- | The distinct packages an index attributes a module to.
owners :: [(Text, [Text])] -> Text -> [Text]
owners pkgModules m =
    nub [p | (p, mods) <- pkgModules, m `elem` mods, not (T.null p)]

-- | The strongest state among the matching evidence; 'InstallState' orders it.
strongest :: [InstallState] -> Maybe InstallState
strongest [] = Nothing
strongest sts = Just (minimum sts)

textAt :: Text -> KM.KeyMap Value -> Text
textAt k o = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
