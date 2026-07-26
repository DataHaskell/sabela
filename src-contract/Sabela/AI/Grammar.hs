{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Grammar (
    BrowseEntry (..),
    ImportStyle (..),
    Envelope (..),
    parseBrowse,
    normalizeName,
    displayEnvelope,
    applyEnvelope,
    grammarTerminals,
    grammarPromptBlock,
    discoverGrammarBlock,
) where

import Data.Char (isAsciiLower)
import Data.Text (Text)
import qualified Data.Text as T

data BrowseEntry = BrowseEntry
    { beName :: Text
    , beType :: Text
    }
    deriving (Eq, Show)

data ImportStyle = Unqualified | QualifiedAs Text
    deriving (Eq, Show)

data Envelope = Bare | DisplaySvgUnpack | DisplayHtml | Hole
    deriving (Eq, Show)

parseBrowse :: Text -> [BrowseEntry]
parseBrowse = concatMap entry . dropBraceRegions . joinWrapped . T.lines
  where
    entry l = case splitSig l of
        Just (lhs, rhs)
            | isValueBinding lhs
            , completeType rhs ->
                [BrowseEntry (T.strip lhs) (T.strip rhs)]
        _ -> []

joinWrapped :: [Text] -> [Text]
joinWrapped = go
  where
    go (a : b : rest)
        | continues a b = go ((a <> " " <> T.strip b) : rest)
        | otherwise = a : go (b : rest)
    go ls = ls
    continues a b =
        T.isPrefixOf " " b
            || any (`T.isSuffixOf` T.stripEnd a) ["::", "->", "=>", ",", "{"]
            || any (`T.isPrefixOf` T.stripStart b) ["->", "=>", "="]

dropBraceRegions :: [Text] -> [Text]
dropBraceRegions = go (0 :: Int)
  where
    go _ [] = []
    go depth (l : rest)
        | depth > 0 = go (depth + braceDelta l) rest
        | otherwise = l : go (max 0 (braceDelta l)) rest
    braceDelta l = T.count "{" l - T.count "}" l

completeType :: Text -> Bool
completeType rhs =
    not (T.null t)
        && T.count "{" t == T.count "}" t
        && not (any (`T.isSuffixOf` t) [",", "::", "->", "=>"])
  where
    t = T.stripEnd rhs

splitSig :: Text -> Maybe (Text, Text)
splitSig l
    | T.isPrefixOf " " l = Nothing
    | otherwise = case T.breakOn " :: " l of
        (lhs, rhs)
            | T.null rhs -> Nothing
            | otherwise -> Just (lhs, T.drop 4 rhs)

isValueBinding :: Text -> Bool
isValueBinding lhs = case T.words lhs of
    [w] -> notElem w keywords && startsLowerName w
    _ -> False
  where
    keywords = ["type", "data", "newtype", "class", "pattern", "instance"]

startsLowerName :: Text -> Bool
startsLowerName w = case T.uncons (lastComponent w) of
    Just (c, _) -> c == '_' || isAsciiLower c
    Nothing -> False

lastComponent :: Text -> Text
lastComponent = last . T.splitOn "."

normalizeName :: ImportStyle -> Text -> Text
normalizeName Unqualified n = lastComponent n
normalizeName (QualifiedAs p) n = p <> "." <> lastComponent n

displayEnvelope :: Text -> Envelope
displayEnvelope sig
    | result == "IO ()" = Bare
    | result == "Text" = DisplaySvgUnpack
    | any (`T.isInfixOf` result) ["Html", "HtmlPlot", "SVG", "Svg"] = DisplayHtml
    | otherwise = Hole
  where
    result = resultType sig

resultType :: Text -> Text
resultType = T.strip . last . splitArrows
  where
    splitArrows = map T.strip . T.splitOn " -> "

applyEnvelope :: Envelope -> Text -> Text
applyEnvelope Bare call = call
applyEnvelope DisplaySvgUnpack call = "displaySvg (T.unpack (" <> call <> "))"
applyEnvelope DisplayHtml call = "displayHtml (" <> call <> ")"
applyEnvelope Hole call = "_ (" <> call <> ")"

grammarTerminals :: [Text]
grammarTerminals = ["bars", "lineGraph", "pie"]

grammarPromptBlock :: Text
grammarPromptBlock =
    T.unlines
        [ "## Finding things — search, don't guess or recall from memory:"
        , ""
        , "  search_capability <desc>   a package for a capability (all Hackage) + its `-- cabal: build-depends:` line + key API"
        , "  find_function <name|mod>   a function by name/keyword, or a module's exports"
        , "  find_by_type <type>        a function whose type fits, e.g. [Double] -> Picture"
        , "  list_bindings              values/types already in scope this session — reuse them"
        , "  check_type <expr>          the type of an expression or name"
        , "  find_example_cell <idiom>  a paste-able cell, e.g. the typed-column CSV reader (a wrong column is a compile error)"
        ]

discoverGrammarBlock :: Text
discoverGrammarBlock =
    T.unlines
        [ "## Finding things — search, don't guess or recall from memory:"
        , ""
        , "  discover <query>   ANY function/package/module: by name (\"divvy\"), goal type (\"[Int] -> Int\"), module (\"Granite.Svg\"), or description (\"edit distance\"). Every hit names its module, package, version, and install state, with the `-- cabal:` line when a package is hidden or not installed; a miss lists what was consulted."
        , "  list_bindings      values/types already in scope this session — reuse them"
        , "  check_type <expr>  the type of an expression or name"
        ]
