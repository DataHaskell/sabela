{-# LANGUAGE OverloadedStrings #-}

{- | When an import fails, every use of a name it provides fails "not in
scope" too. Only the import failure is the candidate's defect; the knock-ons
send a reader (or a repair loop) chasing a scope problem that does not exist.
-}
module Sabela.Diagnose.KnockOn (dropImportKnockOns) where

import Data.Char (isSpace)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Diagnose.Parse (afterInfix, couldNotFindModules, quotedToken)

{- | Drop the not-in-scope diagnostics a failed import explains, keeping the
import failure and every unrelated diagnostic. Suppression is scoped to what
the failed import states (an explicit list or a qualified alias), never more.
-}
dropImportKnockOns :: Text -> Text -> Text
dropImportKnockOns src blob
    | null provisions || not (any knockOn broken) = blob
    | otherwise =
        T.stripEnd (T.intercalate "\n" (concat (filter (not . knockOn) broken)))
  where
    failedMods =
        nub (couldNotFindModules blob <> couldNotLoadModules blob)
    provisions = mapMaybe (importProvision failedMods) (T.lines src)
    broken = chunks blob
    knockOn chunk =
        case scopeErrorName (T.intercalate "\n" chunk) of
            Just name -> any (provides name) provisions
            Nothing -> False

-- | What one import of a failed module puts in scope, as far as it states.
data Provision = Provision
    { pNames :: [Text]
    , pAlias :: Maybe Text
    }

provides :: Text -> Provision -> Bool
provides name p = case T.breakOnEnd "." name of
    ("", bare) -> bare `elem` pNames p
    (qual, bare) ->
        Just (T.dropEnd 1 qual) == pAlias p
            || bare `elem` pNames p

importProvision :: [Text] -> Text -> Maybe Provision
importProvision failedMods line = case T.words beforeList of
    ("import" : ws) -> case dropWhile (== "qualified") ws of
        (m : rest)
            | m `elem` failedMods ->
                Just
                    Provision
                        { pNames = listNames
                        , pAlias = aliasOf m rest
                        }
        _ -> Nothing
    _ -> Nothing
  where
    (beforeList, listPart) = T.breakOn "(" line
    aliasOf m rest = case rest of
        ("as" : a : _) -> Just a
        _
            | "qualified" `elem` T.words beforeList -> Just m
            | otherwise -> Nothing
    listNames
        | T.null listPart = []
        | "hiding" `T.isInfixOf` beforeList = []
        | otherwise =
            filter
                (not . T.null)
                (map itemName (T.splitOn "," inner))
    inner =
        let t = T.strip listPart
         in if T.length t >= 2 then T.init (T.drop 1 t) else ""
    itemName item =
        let t = dropKeyword (T.strip item)
         in if "(" `T.isPrefixOf` t
                then T.dropAround (`elem` juncture) t
                else T.strip (T.takeWhile (/= '(') t)
    dropKeyword t = case T.words t of
        (w : rest) | w `elem` ["type", "pattern"] -> T.unwords rest
        _ -> t

{- | The name a scope diagnostic complains about, covering the "Variable not
in scope", "Not in scope" and constructor variants.
-}
scopeErrorName :: Text -> Maybe Text
scopeErrorName chunk = do
    rest <- afterInfix "ot in scope:" chunk
    let tok = T.takeWhile (not . isSpace) (T.stripStart rest)
        name = T.dropAround (`elem` juncture) tok
    if T.null name then Nothing else Just name

juncture :: [Char]
juncture = ['\8216', '\8217', '(', ')']

couldNotLoadModules :: Text -> [Text]
couldNotLoadModules err =
    nub
        [ m
        | seg <- drop 1 (T.splitOn "Could not load module " err)
        , Just m <- [quotedToken seg]
        ]

{- | One GHC message per chunk: a group opens at each non-indented line that
carries an error or warning header.
-}
chunks :: Text -> [[Text]]
chunks blob = go (T.lines blob)
  where
    go [] = []
    go (l : ls) =
        let (body, rest) = break headerLike ls
         in (l : body) : go rest
    headerLike l =
        not (T.null l)
            && not (isSpace (T.head l))
            && (": error" `T.isInfixOf` l || ": warning" `T.isInfixOf` l)
