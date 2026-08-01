{-# LANGUAGE OverloadedStrings #-}

{- | One law, wherever a hit is spoken for by evidence recorded elsewhere: a
blank field is unknown, so it stands for no package and no module rather than
for all of them.
-}
module Siza.Agent.Discover.ScopeFilter (
    absentTailNote,
    attributeFrom,
    attributedKeep,
    capAbsentKnown,
    contestedNote,
    maxAbsentShown,
    removedByScope,
    sameEntity,
    scopeRemovedNote,
) where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Request (scopeActive, scopeText)
import Siza.Agent.Discover.Types (
    DHit (..),
    InstallState (..),
    Scope (..),
    SourceAnswer (..),
 )

{- | Two records name the same entity when they share a name and specific
evidence agrees: the same package, or the same module. Keyed on the name alone
this admitted every homonym, each inheriting the scoped namesake's module.
-}
sameEntity :: DHit -> DHit -> Bool
sameEntity a b =
    dhName a == dhName b
        && (agrees (dhPackage a) (dhPackage b) || agrees (dhModule a) (dhModule b))
  where
    agrees x y = not (T.null x) && x == y

attributedKeep :: [DHit] -> Scope -> DHit -> Bool
attributedKeep union (Scope m p) h =
    maybe True moduleOk m && maybe True packageOk p
  where
    siblings = [g | g <- union, sameEntity g h]
    moduleOk f = any (moduleMatch f) (dhModule h : map dhModule siblings)
    moduleMatch f mo = mo == f || (f <> ".") `T.isPrefixOf` mo
    packageOk f = f `elem` (dhPackage h : map dhPackage siblings)

{- | The package a hit's own module resolves to, when the sources agree on one
owner. Two owners is not an attribution: it is a question, and answering it by
first listing would state something no source computed.
-}
attributeFrom :: [(Text, [Text])] -> DHit -> DHit
attributeFrom pkgModules h
    | not (T.null (dhPackage h)) = h
    | [p] <- moduleOwners pkgModules (dhModule h) = h{dhPackage = p}
    | otherwise = h

moduleOwners :: [(Text, [Text])] -> Text -> [Text]
moduleOwners pkgModules m
    | T.null m = []
    | otherwise =
        nub [p | (p, mods) <- pkgModules, m `elem` mods, not (T.null p)]

{- | Name a module the sources gave more than one owner. A hit left blank there
reads as "nothing known" when two things are known and they disagree.
-}
contestedNote :: [SourceAnswer] -> [DHit] -> Maybe Text
contestedNote answers hits = case contested of
    [] -> Nothing
    ((m, ps) : rest) ->
        Just
            ( m
                <> " is claimed by "
                <> T.intercalate ", " ps
                <> ", so the package is left unattributed"
                <> (if null rest then "" else " (and " <> tShow (length rest) <> " more)")
            )
  where
    pkgModules = concatMap saPkgModules answers
    contested =
        [ (m, ps)
        | m <- nub [dhModule h | h <- hits, T.null (dhPackage h)]
        , let ps = moduleOwners pkgModules m
        , length ps > 1
        ]

removedByScope :: [DHit] -> Scope -> [DHit]
removedByScope union scope =
    [h | h <- union, not (attributedKeep union scope h)]

{- | Name the candidates a scope excluded, not just how many modules they came
from: only the name and type let the caller decide whether the filter cost it
the answer. A clause with nothing to say is omitted, never left dangling.
-}
scopeRemovedNote :: Scope -> [DHit] -> Maybe Text
scopeRemovedNote scope removed
    | null removed || not (scopeActive scope) = Nothing
    | otherwise = Just (headline <> excerpt)
  where
    headline =
        "filter "
            <> scopeText scope
            <> " removed "
            <> tShow (length removed)
            <> " candidate"
            <> (if length removed == 1 then "" else "s")
    excerpt
        | null shown = ""
        | otherwise =
            " — excluded: "
                <> T.intercalate ", " shown
                <> (if length described > maxNamed then ", …" else "")
    described = filter (not . T.null) (map describe removed)
    shown = take maxNamed described
    describe h =
        T.unwords (filter (not . T.null) [dhName h, sigOf h, inModule h])
    sigOf h
        | T.null (dhType h) = ""
        | otherwise = ":: " <> clip 60 (dhType h)
    inModule h
        | T.null (dhModule h) = ""
        | otherwise = "(" <> dhModule h <> ")"
    clip n t = if T.length t > n then T.take n t <> "…" else t

maxNamed :: Int
maxNamed = 3

maxAbsentShown :: Int
maxAbsentShown = 2

capAbsentKnown :: Int -> [DHit] -> [DHit]
capAbsentKnown limit kept = take limit (go [] kept)
  where
    go _ [] = []
    go pkgs (h : rest)
        | dhInstall h == InstAbsentKnown
        , dhPackage h `notElem` pkgs =
            if length pkgs >= maxAbsentShown
                then go pkgs rest
                else h : go (dhPackage h : pkgs) rest
        | otherwise = h : go pkgs rest

absentTailNote :: [DHit] -> [DHit] -> Maybe Text
absentTailNote kept shown
    | suppressed <= 0 = Nothing
    | otherwise =
        Just $
            tShow suppressed
                <> " more exact matches across "
                <> tShow (length pkgs)
                <> " not-installed packages (counted in omitted)"
  where
    absentIn hs = [h | h <- hs, dhInstall h == InstAbsentKnown]
    suppressed = length (absentIn kept) - length (absentIn shown)
    pkgs = nub [dhPackage h | h <- absentIn kept]

tShow :: Int -> Text
tShow = T.pack . show
