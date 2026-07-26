{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.NormalizeProposals (
    bindingKeywords,
    confusableHyphens,
    foldCabalComments,
    proposedRename,
    renameKeywordBindings,
) where

import Data.Char (isAlphaNum)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

confusableHyphens :: [Char]
confusableHyphens = ['\x2010' .. '\x2015'] ++ ['\x2212']

bindingKeywords :: [Text]
bindingKeywords =
    [ "case"
    , "class"
    , "data"
    , "default"
    , "deriving"
    , "do"
    , "else"
    , "foreign"
    , "if"
    , "import"
    , "in"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "module"
    , "newtype"
    , "of"
    , "then"
    , "type"
    , "where"
    ]

proposedRename :: Text -> Text
proposedRename kw = kw <> "'"

foldCabalComments :: Text -> (Text, [Text])
foldCabalComments src
    | null noteList = (src, [])
    | otherwise = (rebuildLines src (map fst results), noteList)
  where
    results = map foldLine (T.lines src)
    noteList = concatMap snd results
    foldLine l
        | isCabalComment folded
        , folded' /= l =
            (folded', [cabalNote l folded'])
        | otherwise = (l, [])
      where
        folded = foldHyphens l
        folded' = canonicalKey folded
    cabalNote old new =
        "Rewrote the cabal comment `" <> old <> "` to `" <> new <> "`."

foldHyphens :: Text -> Text
foldHyphens = T.map (\c -> if c `elem` confusableHyphens then '-' else c)

rebuildLines :: Text -> [Text] -> Text
rebuildLines src ls =
    T.intercalate "\n" ls
        <> (if "\n" `T.isSuffixOf` src then "\n" else "")

isCabalComment :: Text -> Bool
isCabalComment l =
    "--" `T.isPrefixOf` s
        && "cabal:" `T.isPrefixOf` T.stripStart (T.dropWhile (== '-') s)
  where
    s = T.stripStart l

canonicalKey :: Text -> Text
canonicalKey l = case T.breakOn "cabal:" l of
    (before, rest)
        | not (T.null rest) ->
            let body = T.drop 6 rest
                (keyPart, colonRest) = T.breakOn ":" body
                lead = T.takeWhile (== ' ') keyPart
                key = T.strip keyPart
             in if not (T.null colonRest)
                    && length (T.words keyPart) == 1
                    && isBuildDependsClass key
                    && key /= "build-depends"
                    then before <> "cabal:" <> lead <> "build-depends" <> colonRest
                    else l
    _ -> l

isBuildDependsClass :: Text -> Bool
isBuildDependsClass key =
    "builddep" `T.isPrefixOf` T.filter (/= '-') (T.toLower key)

renameKeywordBindings :: Text -> (Text, [Text])
renameKeywordBindings src
    | null kws = (src, [])
    | otherwise =
        ( rebuildLines src (map (renameLine kws) (T.lines src))
        , map note kws
        )
  where
    kws = nub [k | l <- T.lines src, Just k <- [boundKeyword l]]
    note k =
        "Renamed the binding `"
            <> k
            <> "` to `"
            <> proposedRename k
            <> "` — `"
            <> k
            <> "` is a reserved word and cannot name a binding."

boundKeyword :: Text -> Maybe Text
boundKeyword l = case T.words l of
    (w : op : _)
        | not (startsIndented l)
        , w `elem` bindingKeywords
        , op `elem` ["=", "::"] ->
            Just w
    _ -> Nothing
  where
    startsIndented t = case T.uncons t of
        Just (c, _) -> c == ' ' || c == '\t'
        Nothing -> True

renameLine :: [Text] -> Text -> Text
renameLine kws = go
  where
    identChar c = isAlphaNum c || c == '_' || c == '\''
    go t = case T.uncons t of
        Nothing -> ""
        Just ('"', _) ->
            let (str, rest) = spanString t in str <> go rest
        Just (c, _)
            | "--" `T.isPrefixOf` t -> t
            | identChar c ->
                let (run, rest) = T.span identChar t
                 in swap run <> go rest
            | otherwise -> T.singleton c <> go (T.drop 1 t)
    swap run
        | run `elem` kws = proposedRename run
        | otherwise = run
    spanString t = case T.uncons t of
        Just ('"', body) ->
            let (inner, rest) = closeString body
             in ("\"" <> inner, rest)
        _ -> (t, "")
    closeString t = case T.break (`elem` ("\"\\" :: String)) t of
        (pre, rest) -> case T.uncons rest of
            Just ('"', more) -> (pre <> "\"", more)
            Just ('\\', more) -> case T.uncons more of
                Just (e, more') ->
                    let (inner, r) = closeString more'
                     in (pre <> T.cons '\\' (T.cons e inner), r)
                Nothing -> (pre <> "\\", "")
            _ -> (pre <> rest, "")
