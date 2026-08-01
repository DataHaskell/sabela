{-# LANGUAGE OverloadedStrings #-}

{- | How a caller reaches a hit: the import that puts it in scope, and the
warning that another module answers to the same bare name. Stamped once, at
the merge, so every source's hits carry it.
-}
module Siza.Agent.Discover.Affordance (
    clashBudget,
    clashNote,
    importable,
    markClashes,
    moduleShaped,
    packageStub,
    scopeUse,
    withCardClashes,
    writableName,
) where

import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.List (mapAccumL, nub)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Types (
    DHit (..),
    InstallState (..),
    NotebookEnv (..),
 )

{- | Two surviving hits with one bare name and different modules collide at
the use site. Every one of them is offered the qualified import — that is the
action — and the first of each colliding name counts the others and names one,
which is the whole decision the list of names used to cost per hit.
A hit whose own module the harness did not compute is left alone: it is not
known to differ from anything, and there is no qualified import to offer.
-}
markClashes :: [DHit] -> [DHit]
markClashes hs = snd (mapAccumL mark (Set.empty, 0) hs)
  where
    mark acc h
        | not (importable h) = (acc, h)
        | otherwise = case others h of
            [] -> (acc, h)
            ms -> spend acc h ms
    spend (seen, spent) h ms
        | dhName h `Set.member` seen = ((seen, spent), qual h)
        | spent + T.length note > clashBudget = ((seen', spent), qual h)
        | otherwise =
            ((seen', spent + T.length note), (qual h){dhClash = Just note})
      where
        seen' = Set.insert (dhName h) seen
        note = clashNote ms
    others h =
        nub
            [ dhModule o
            | o <- hs
            , dhName o == dhName h
            , dhModule o /= dhModule h
            , moduleShaped (dhModule o)
            ]
    qual h
        | importShaped (dhUse h) =
            h{dhUse = Just ("import qualified " <> dhModule h <> importList h)}
        | otherwise = h
    importShaped = maybe True ("import " `T.isPrefixOf`)

{- | Count first, then one name: the caller's decision is whether to qualify,
which the count settles, and the shown hits carry the rest of the modules.
-}
clashNote :: [Text] -> Text
clashNote ms = case ms of
    [] -> ""
    (m : _) ->
        T.pack (show (length ms))
            <> " other module"
            <> (if length ms > 1 then "s" else "")
            <> " export this name, incl. "
            <> m

{- | The characters one answer may spend saying its names collide. Marks are
laid down in ranked order until the next would exceed it.
-}
clashBudget :: Int
clashBudget = 120

{- | The exports a card offers that are already spoken for in the notebook.
Two unqualified names cannot both win, and the caller cannot see the collision
from the export list alone.
-}
withCardClashes :: NotebookEnv -> Value -> Value
withCardClashes env (Object c) = case clashes of
    [] -> Object c
    ns -> Object (KM.insert "clashesInScope" (toJSON (T.intercalate ", " ns)) c)
  where
    inScope = neBindings env ++ neBuiltins env
    clashes = nub [n | n <- exportNames, n `elem` inScope]
    exportNames = case KM.lookup "exports" c of
        Just (Array es) -> [baseName e | String e <- toList es]
        _ -> []
    baseName = T.takeWhile (\ch -> ch /= ' ' && ch /= ':')
withCardClashes _ v = v

{- | The import that puts a hit in scope, or the statement that the notebook
already has it. Stamped for every hit the merge sees, so a name a source
returned is never left without a way to call it.
-}
scopeUse :: [Text] -> [(Text, Text)] -> DHit -> Maybe Text
scopeUse importTargets aliases h
    | dhInstall h == InstBuiltin =
        Just "in scope at session start — no import needed"
    | not (importable h) = Nothing
    | m `elem` importTargets = Just (importedNote m)
    | otherwise = Just ("import " <> m <> importList h)
  where
    m = dhModule h
    importedNote mm = case [a | (a, m') <- aliases, m' == mm] of
        (a : _) -> "already imported as " <> a <> " (notebook import)"
        [] -> "already imported by the notebook"

{- | A hit an import line can be written for: a module an import declaration
can name, and a name Haskell can write. A hit failing either is left without a
`use` rather than handed a line that does not parse.
-}
importable :: DHit -> Bool
importable h = moduleShaped (dhModule h) && writableName (dhName h)

{- | A module name an import declaration can name: alphanumeric, dotted, and
Capitalised. Rules out the "(not installed)" stand-in a package-only hit carries.
-}
moduleShaped :: Text -> Bool
moduleShaped m =
    not (T.null m)
        && all component (T.splitOn "." m)
  where
    component seg = case T.uncons seg of
        Just (c, rest) ->
            c `elem` ['A' .. 'Z'] && T.all (`elem` identChars) rest
        Nothing -> False

{- | A name Haskell can write where an import expects one: an identifier or an
operator. A package name is neither — @import M (some-pkg)@ does not parse.
-}
writableName :: Text -> Bool
writableName n = identShaped n || isOperatorName n
  where
    identShaped t = case T.uncons t of
        Just (c, rest) ->
            (c `elem` (['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_"))
                && T.all (`elem` identChars) rest
        Nothing -> False

identChars :: String
identChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_'"

{- | A hit that stands for a package rather than for one of its exports: it
carries the package's own name and no source returned a signature for it.
-}
packageStub :: DHit -> Bool
packageStub h =
    T.null (dhType h)
        && not (T.null (dhPackage h))
        && norm (dhName h) == norm (dhPackage h)
  where
    norm = T.toLower . T.filter (/= '-')

{- | The explicit import list a hit is entitled to. A package-level stub names
none: its name is the package's, which the module does not export.
-}
importList :: DHit -> Text
importList h
    | packageStub h = ""
    | otherwise = explicitList (dhName h)

{- | The explicit import list for a value name, elided for a type-level or
constructor name where @import M (T)@ would not mean what the caller wants.
-}
explicitList :: Text -> Text
explicitList n
    | T.null n = ""
    | isOperatorName n = " ((" <> n <> "))"
    | Just (c, _) <- T.uncons n
    , c `elem` ['a' .. 'z'] || c == '_' =
        " (" <> n <> ")"
    | otherwise = ""

isOperatorName :: Text -> Bool
isOperatorName n = T.all (`elem` opChars) n && not (T.null n)
  where
    opChars = ":!#$%&*+./<=>?@\\^|-~" :: String
