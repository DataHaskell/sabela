{-# LANGUAGE OverloadedStrings #-}

{- | The post-union scope predicate (search-api.md section 3.3) and the
bounded absent-known tail (section 7): filters run at the merge over each
hit's ATTRIBUTED modules/packages, and every removal is disclosed.
-}
module Siza.Agent.Discover.ScopeFilter (
    absentTailNote,
    attributedKeep,
    capAbsentKnown,
    maxAbsentShown,
    removedByScope,
    scopeRemovedNote,
) where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Request (scopeActive, scopeText)
import Siza.Agent.Discover.Types (DHit (..), InstallState (..), Scope (..))

{- | Scope filters judged over a hit's ATTRIBUTED modules/packages (3.3): a
name in a re-exporting and a defining module of ONE package satisfies a
filter naming either; same-name hits in another package stay homonyms.
-}
attributedKeep :: [DHit] -> Scope -> DHit -> Bool
attributedKeep union (Scope m p) h =
    maybe True moduleOk m && maybe True packageOk p
  where
    siblings =
        [ g
        | g <- union
        , dhName g == dhName h
        , samePkg (dhPackage g) (dhPackage h)
        ]
    samePkg a b = a == b || T.null a || T.null b
    moduleOk f = any (moduleMatch f) (dhModule h : map dhModule siblings)
    moduleMatch f mo = mo == f || (f <> ".") `T.isPrefixOf` mo
    packageOk f = f `elem` (dhPackage h : map dhPackage siblings)

-- | The union hits a scope filter excludes (for conservation disclosure).
removedByScope :: [DHit] -> Scope -> [DHit]
removedByScope union scope =
    [h | h <- union, not (attributedKeep union scope h)]

{- | The R3.3 disclosure of what a filter removed: count plus the removed
hits' attributed modules — a scoped answer can never silently hide where the
unscoped union's hits live.
-}
scopeRemovedNote :: Scope -> [DHit] -> Maybe Text
scopeRemovedNote scope removed
    | null removed || not (scopeActive scope) = Nothing
    | otherwise =
        Just $
            "filter "
                <> scopeText scope
                <> " removed "
                <> tShow (length removed)
                <> " candidate"
                <> (if length removed == 1 then "" else "s")
                <> " — attributed to: "
                <> T.intercalate ", " (take 4 mods)
                <> (if length mods > 4 then ", …" else "")
  where
    mods = nub [dhModule h | h <- removed, not (T.null (dhModule h))]

-- | At most this many not-installed packages contribute absent-known rows.
maxAbsentShown :: Int
maxAbsentShown = 2

{- | The limit plus the CROSS-PACKAGE absent-known cap (section 7): rows from
beyond the first 'maxAbsentShown' not-installed packages stay in @omitted@
(counts reconcile); one absent package's own export list is not the wall.
-}
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

-- | The counted-tail disclosure for absent-known rows the cap suppressed.
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
