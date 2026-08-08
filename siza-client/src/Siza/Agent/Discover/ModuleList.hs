{-# LANGUAGE OverloadedStrings #-}

{- | How much of a package's module list a hit states, and the field names it
states it under. The emitter and the schema live together so a field cannot be
emitted under a name the declared schema does not know.
-}
module Siza.Agent.Discover.ModuleList (
    ModuleView (..),
    factKeys,
    factRows,
    repoSlugOf,
    shownModules,
) where

import Data.Aeson ((.=))
import Data.Aeson.Types (Pair)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.ModuleResolve (boundedModules)
import Siza.Agent.Discover.CabalFacts (PkgFacts (..))

{- | How much of a package's module list a hit states. Asked about the package,
the list is the answer; landed on in a wider search, it is a decoration, and
spending the envelope on it costs a hit the caller asked for.
-}
data ModuleView = ModuleLead | ModuleWhole
    deriving (Eq, Show)

-- | The fields 'factRows' emits, for the schema that must declare them.
factKeys :: [Text]
factKeys = ["homepage", "repo", "modules"]

{- | What the index states about a package nothing installed can speak for.
The module list is the answer to "what do I import", so it is bounded and
led by the modules a caller reaches for first.
-}
factRows :: ModuleView -> Maybe PkgFacts -> [Pair]
factRows _ Nothing = []
factRows view (Just f) =
    ["homepage" .= pfHomepage f | not (T.null (pfHomepage f))]
        <> ["repo" .= r | Just r <- [repoSlugOf (pfHomepage f)]]
        <> ["modules" .= viewModules view f | not (null (pfModules f))]

{- | The @owner\/name@ a GitHub homepage states, in the form @list_files@ and
@read_file@ take. Nothing for any other host, and nothing for a path deeper
than the repository, which names a page in it rather than the repository.
-}
repoSlugOf :: Text -> Maybe Text
repoSlugOf url = case pathSegments of
    [owner, name]
        | onGitHub
        , not (T.null owner)
        , not (T.null name) ->
            Just (owner <> "/" <> name)
    _ -> Nothing
  where
    bare = dropScheme (T.strip url)
    dropScheme t = case T.breakOn "://" t of
        (_, rest) | not (T.null rest) -> T.drop 3 rest
        _ -> t
    host = T.takeWhile (/= '/') bare
    onGitHub = host `elem` ["github.com", "www.github.com"]
    pathSegments =
        map
            dropGitSuffix
            (filter (not . T.null) (T.splitOn "/" (T.drop (T.length host) bare)))
    dropGitSuffix s = fromMaybe s (T.stripSuffix ".git" s)

viewModules :: ModuleView -> PkgFacts -> [Text]
viewModules ModuleLead = shownModules
viewModules ModuleWhole =
    boundedModules wholeModuleCap . sortOn moduleRank . pfModules

{- | What a package asked about by name may spend on its own structure. Above it
the list collapses to namespaces, so the whole package is still accounted for.
-}
wholeModuleCap :: Int
wholeModuleCap = 25

-- | At most this many modules ride on a hit; the rest are a scope away.
shownModuleCap :: Int
shownModuleCap = 6

{- | Public roots first, then depth, then name: the entry point a caller wants
leads, and an internal module never displaces it.
-}
shownModules :: PkgFacts -> [Text]
shownModules = take shownModuleCap . sortOn moduleRank . pfModules

moduleRank :: Text -> (Int, Int, Text)
moduleRank m = (if isInternalModule m then 1 else 0, T.count "." m, m)

isInternalModule :: Text -> Bool
isInternalModule m = "Internal" `elem` T.splitOn "." m
