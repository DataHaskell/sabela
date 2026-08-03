{-# LANGUAGE OverloadedStrings #-}

{- | A parsed search request. Retrieval works from this structure, never from
rewritten prose: a query is decomposed into terms once, and every probe is
derived from those terms.
-}
module Sabela.AI.Search.Need (
    Need (..),
    parseNeed,
    denoise,
    keywords,
    salientWords,
    isTypeOrName,
    isSingleToken,
    noiseTokens,
    stopWords,
) where

import Data.Char (isAlphaNum)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T

data Need = Need
    { needRaw :: Text
    , needCleaned :: Text
    , needTerms :: [Text]
    , needScope :: Set Text
    }
    deriving (Eq, Show)

parseNeed :: Set Text -> Text -> Need
parseNeed scope raw =
    Need
        { needRaw = stripped
        , needCleaned = cleaned
        , needTerms = salientWords stripped
        , needScope = scope
        }
  where
    stripped = dropDeclKeyword (T.strip raw)
    cleaned = denoise stripped

{- | A leading @data@\/@type@\/@newtype@\/@class@ is the Haskell syntax the user
typed around the name they want, not a term to search for.
-}
dropDeclKeyword :: Text -> Text
dropDeclKeyword q = case T.words q of
    (kw : rest@(_ : _)) | T.toLower kw `elem` declKeywords -> T.unwords rest
    _ -> q
  where
    declKeywords = ["data", "type", "newtype", "class"]

{- | Query terms with their case intact. Case is load-bearing for Hoogle — a
capitalised word searches types, a lower-case one searches names — so terms
are only folded when they are compared against a row, never when probed.
-}
salientWords :: Text -> [Text]
salientWords q =
    let kept = filter (not . isNoise) (T.words q)
     in if null kept then T.words q else kept
  where
    -- A single character carries no retrieval signal; left in, the type
    -- variables of a query like "Lens s t a b" outvote the name.
    isNoise w =
        let l = T.toLower w
         in T.length (T.filter isAlphaNum w) < 2
                || l `elem` stopWords
                || l `elem` noiseTokens

isTypeOrName :: Text -> Bool
isTypeOrName q = "->" `T.isInfixOf` q || "::" `T.isInfixOf` q

isSingleToken :: Text -> Bool
isSingleToken q = length (T.words q) <= 1

denoise :: Text -> Text
denoise q =
    let stripped = foldr stripPhrase (T.toLower q) noisePhrases
        kept = filter (`notElem` noiseTokens) (T.words stripped)
     in if null kept then q else T.unwords kept
  where
    stripPhrase p t = T.replace (" " <> p <> " ") " " (pad t)
    pad t = " " <> t <> " "

noiseTokens :: [Text]
noiseTokens =
    [ "haskell"
    , "ghc"
    , "hackage"
    , "library"
    , "libraries"
    , "package"
    , "packages"
    , "function"
    , "functions"
    , "module"
    , "modules"
    ]

noisePhrases :: [Text]
noisePhrases =
    [ "in haskell"
    , "how do i"
    , "how to"
    , "i want to"
    , "i need to"
    ]

keywords :: Text -> [Text]
keywords q =
    let ws = T.words q
        kept = filter (\w -> T.toLower w `notElem` stopWords) ws
     in if null kept then ws else kept

stopWords :: [Text]
stopWords =
    [ "a"
    , "an"
    , "the"
    , "how"
    , "to"
    , "of"
    , "is"
    , "are"
    , "be"
    , "two"
    , "in"
    , "on"
    , "for"
    , "and"
    , "or"
    , "with"
    , "from"
    , "into"
    , "that"
    , "this"
    , "as"
    , "by"
    , "it"
    , "do"
    , "i"
    , "want"
    , "need"
    , "given"
    ]
