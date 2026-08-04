{-# LANGUAGE OverloadedStrings #-}

{- | What Hackage's index states about a package, read from its @.cabal@ file.
This is the only source that can describe a package the session has not
installed, so it is what an absent-known hit has to speak from.
-}
module Siza.Agent.Discover.CabalFacts (
    PkgFacts (..),
    emptyFacts,
    hasFacts,
    parseCabalFacts,
    renderFactsRow,
    parseFactsRow,
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

{- | Where to read about a package, what it is for, and what a dependent may
import from it.
-}
data PkgFacts = PkgFacts
    { pfHomepage :: !Text
    , pfSynopsis :: !Text
    , pfModules :: ![Text]
    }
    deriving (Eq, Show)

emptyFacts :: PkgFacts
emptyFacts = PkgFacts "" "" []

-- | Whether a row carries anything beyond the package's own name.
hasFacts :: PkgFacts -> Bool
hasFacts f = f /= emptyFacts

-- | The synopsis is a card row, not prose; a long one is cut to fit.
synopsisCap :: Int
synopsisCap = 200

parseCabalFacts :: Text -> PkgFacts
parseCabalFacts src =
    PkgFacts
        { pfHomepage = firstBlock "homepage" topLines
        , pfSynopsis = T.take synopsisCap (firstBlock "synopsis" topLines)
        , pfModules = dedup (concatMap moduleNames exposedBlocks)
        }
  where
    sectioned = sections (T.lines src)
    topLines = [l | (SecTop, l) <- sectioned]
    libLines = [l | (SecPublicLib, l) <- sectioned]
    exposedBlocks = fieldBlocks "exposed-modules" libLines
    firstBlock name ls = case fieldBlocks name ls of
        (b : _) -> b
        [] -> ""

-- --- stanza segmentation ----------------------------------------------------

{- | The part of a cabal file a line belongs to. Only the public library's
modules are importable by a dependent, so a named sublibrary is not it.
-}
data Section = SecTop | SecPublicLib | SecOther
    deriving (Eq, Show)

-- | Tag every line with the section it falls in, order preserved.
sections :: [Text] -> [(Section, Text)]
sections = go SecTop
  where
    go _ [] = []
    go cur (l : rest) = case stanzaSection l of
        Just next -> (next, l) : go next rest
        Nothing -> (cur, l) : go cur rest

{- | The section a stanza header opens, or 'Nothing' for any other line. A
header is unindented, so a conditional inside a stanza never closes it.
-}
stanzaSection :: Text -> Maybe Section
stanzaSection l
    | indent l /= 0 = Nothing
    | T.null word = Nothing
    | word == "library" =
        Just (if isBare rest then SecPublicLib else SecOther)
    | word `elem` stanzaWords = Just SecOther
    | otherwise = Nothing
  where
    s = T.strip l
    word = T.toLower (T.takeWhile (not . isSpace') s)
    rest = T.strip (T.dropWhile (not . isSpace') s)
    isBare r = T.null r || "--" `T.isPrefixOf` r

stanzaWords :: [Text]
stanzaWords =
    [ "executable"
    , "test-suite"
    , "benchmark"
    , "foreign-library"
    , "common"
    , "source-repository"
    , "flag"
    , "custom-setup"
    ]

-- --- field blocks -----------------------------------------------------------

{- | Every occurrence of a field, each folded to one line. A block ends at the
next field, stanza or conditional, so a neighbouring field is never read as
part of this one's value.
-}
fieldBlocks :: Text -> [Text] -> [Text]
fieldBlocks name ls = go (filter (not . isComment) ls)
  where
    go [] = []
    go (l : rest)
        | isField name l =
            let base = indent l
                (cont, rest') = span (continues base) rest
             in flatten (afterColon l : cont) : go rest'
        | otherwise = go rest
    continues base l
        | T.null (T.strip l) = True
        | isConditional l = False
        | otherwise = not (indent l <= base && (isFieldStart l || isStanza l))
    flatten = T.unwords . T.words . T.unwords

{- | A comment states nothing about the package. Dropping it before the fold
keeps a commented-out field from reading as part of the one above it.
-}
isComment :: Text -> Bool
isComment l = "--" `T.isPrefixOf` T.dropWhile isSpace' l

isField :: Text -> Text -> Bool
isField name l =
    name
        `T.isPrefixOf` lowered
        && ":" `T.isPrefixOf` T.dropWhile isSpace' (T.drop (T.length name) lowered)
  where
    lowered = T.toLower (T.dropWhile isSpace' l)

{- | A field opens a line: a name, then a colon, which cabal lets the author
align away from the name.
-}
isFieldStart :: Text -> Bool
isFieldStart l =
    not (T.null s)
        && isAlpha (T.head s)
        && ":" `T.isPrefixOf` T.dropWhile isSpace' (T.dropWhile isFieldNameChar s)
  where
    s = T.dropWhile isSpace' l

isStanza :: Text -> Bool
isStanza l = firstWord l `elem` ("library" : stanzaWords)

{- | A conditional ends the enclosing field's value: what follows belongs to
the branch, and a field it names is found as its own block.
-}
isConditional :: Text -> Bool
isConditional l = firstWord l `elem` ["if", "else", "elif"]

firstWord :: Text -> Text
firstWord = T.toLower . T.takeWhile (not . isSpace') . T.dropWhile isSpace'

afterColon :: Text -> Text
afterColon = T.drop 1 . T.dropWhile (/= ':')

-- --- module names -----------------------------------------------------------

{- | The module names a folded block states. A token that cannot be a module
name is dropped, so a conditional's own words never read as modules.
-}
moduleNames :: Text -> [Text]
moduleNames = filter isModuleName . T.split isSeparator
  where
    isSeparator c = c == ',' || c == ' ' || c == '\t'

isModuleName :: Text -> Bool
isModuleName t = case T.uncons t of
    Just (c, _) -> isAsciiUpper c && T.all isModuleChar t
    Nothing -> False
  where
    isModuleChar c = isAlpha c || isDigit c || c `elem` ("._'" :: String)

dedup :: [Text] -> [Text]
dedup = go S.empty
  where
    go _ [] = []
    go seen (x : xs)
        | x `S.member` seen = go seen xs
        | otherwise = x : go (S.insert x seen) xs

-- --- cache rows -------------------------------------------------------------

{- | One package per line: name, homepage, synopsis, space-separated modules.
Every value is whitespace-normalised at parse, so a row is always one line.
-}
renderFactsRow :: Text -> PkgFacts -> Text
renderFactsRow name f =
    T.intercalate
        "\t"
        [name, pfHomepage f, pfSynopsis f, T.unwords (pfModules f)]

parseFactsRow :: Text -> Maybe (Text, PkgFacts)
parseFactsRow row = case T.splitOn "\t" row of
    (n : rest)
        | not (T.null (T.strip n)) ->
            Just
                ( T.strip n
                , PkgFacts (col 0 rest) (col 1 rest) (T.words (col 2 rest))
                )
    _ -> Nothing
  where
    col i cols = case drop i cols of
        (c : _) -> c
        [] -> ""

-- --- character predicates ---------------------------------------------------

isSpace' :: Char -> Bool
isSpace' c = c == ' ' || c == '\t'

isAlpha :: Char -> Bool
isAlpha c = isAsciiLower c || isAsciiUpper c

isFieldNameChar :: Char -> Bool
isFieldNameChar c = isAlpha c || isDigit c || c == '_' || c == '-'

indent :: Text -> Int
indent = T.length . T.takeWhile isSpace'
