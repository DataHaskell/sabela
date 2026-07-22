{-# LANGUAGE OverloadedStrings #-}

{- | Write-boundary normalizer GENERATORS (R6-T1), keyed on diagnostic class,
never on a library name; pure proposers vetted by the ONE acceptance law in
'Sabela.AI.NormalizeGate' before any output is kept.
-}
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

-- | The Unicode hyphen confusables of the task class: U+2010..U+2015, U+2212.
confusableHyphens :: [Char]
confusableHyphens = ['\x2010' .. '\x2015'] ++ ['\x2212']

-- | Reserved words that cannot name a binding (@let@ has its own generator).
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

-- | The rename a keyword-binding proposal offers: the primed identifier.
proposedRename :: Text -> Text
proposedRename kw = kw <> "'"

{- | Fold every @-- cabal:@ comment line: confusable hyphens to ASCII and a
build-depends-class key misspelling to the canonical key. Non-cabal lines are
untouched (the fold is scoped to the cabal-comment diagnostic class).
-}
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

-- | Confusable hyphens to ASCII @-@.
foldHyphens :: Text -> Text
foldHyphens = T.map (\c -> if c `elem` confusableHyphens then '-' else c)

-- | Reassemble edited lines, preserving the original trailing newline.
rebuildLines :: Text -> [Text] -> Text
rebuildLines src ls =
    T.intercalate "\n" ls
        <> (if "\n" `T.isSuffixOf` src then "\n" else "")

-- | A comment line whose content is a @cabal:@ directive.
isCabalComment :: Text -> Bool
isCabalComment l =
    "--" `T.isPrefixOf` s
        && "cabal:" `T.isPrefixOf` T.stripStart (T.dropWhile (== '-') s)
  where
    s = T.stripStart l

{- | Canonicalise a @-- cabal:@ line's single-token key when it falls in the
build-depends misspelling class (@build-dep*@ modulo hyphens); the values are
preserved verbatim — the fold can correct a key, never a package name.
-}
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

-- | The misspelling class: @build-dep@-stemmed keys, hyphens ignored.
isBuildDependsClass :: Text -> Bool
isBuildDependsClass key =
    "builddep" `T.isPrefixOf` T.filter (/= '-') (T.toLower key)

{- | Rename every reserved word used in binding position (@kw =@ \/ @kw ::@
at column 0) to its primed proposal, across the cell's code — strings and
comments untouched. No binding-position keyword: the source is unchanged.
-}
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

-- | The keyword a column-0 @kw =@ \/ @kw ::@ line tries to bind, if any.
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

{- | Rename identifier occurrences of the keywords in ONE line's code:
string literals verbatim, everything after a code-position @--@ verbatim.
-}
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
