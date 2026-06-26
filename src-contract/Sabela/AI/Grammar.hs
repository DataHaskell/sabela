{-# LANGUAGE OverloadedStrings #-}

{- | Grammar primitives for the live-@:browse@ synth ('Sabela.AI.Grammar.Synth'):
parse a browse into terminals, normalise a qualified name to the form the cell
writes, and pick the type-directed display envelope. Plus 'grammarPromptBlock', a
short prompt that points the model at the discovery search tools and the
@-- cabal:@ mechanism, rather than carrying hard-coded worked examples.
-}
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
) where

import Data.Char (isAsciiLower)
import Data.Text (Text)
import qualified Data.Text as T

{- | One @name :: type@ line of @:browse@ output. @beName@ keeps the qualifier
@:browse@ prints (e.g. @Granite.Svg.bars@); @beType@ is the rendered signature.
-}
data BrowseEntry = BrowseEntry
    { beName :: Text
    , beType :: Text
    }
    deriving (Eq, Show)

{- | How the sketch imports a module, which decides whether a @:browse@
qualifier survives normalisation. @Unqualified@ (the @import Granite.Svg@ case)
strips the qualifier; @QualifiedAs@ keeps a chosen prefix (the @Plot.*@ case,
@import qualified ... as Plot@).
-}
data ImportStyle = Unqualified | QualifiedAs Text
    deriving (Eq, Show)

{- | The display surface a plotter's result type selects: an @IO ()@ plotter is
called bare, a @Text@ plotter is fed through @displaySvg . T.unpack@, an
@Html@/@SVG@ producer through @displayHtml@, and anything else leaves a hole for
the model to fill.
-}
data Envelope = Bare | DisplaySvgUnpack | DisplayHtml | Hole
    deriving (Eq, Show)

{- | Parse @:browse@ output into the @name :: type@ entries, dropping @type@,
@data@, @newtype@, @class@, @pattern@, and continuation lines that carry no
top-level value binding.
-}
parseBrowse :: Text -> [BrowseEntry]
parseBrowse = concatMap entry . T.lines
  where
    entry l = case splitSig l of
        Just (lhs, rhs)
            | isValueBinding lhs -> [BrowseEntry (T.strip lhs) (T.strip rhs)]
        _ -> []

-- | Split a line on the top-level @::@, if it has one and starts in column 0.
splitSig :: Text -> Maybe (Text, Text)
splitSig l
    | T.isPrefixOf " " l = Nothing
    | otherwise = case T.breakOn " :: " l of
        (lhs, rhs)
            | T.null rhs -> Nothing
            | otherwise -> Just (lhs, T.drop 4 rhs)

{- | The LHS of a @::@ is a value binding when it is a single lower-or-qualified
identifier, not a @type@/@data@/@class@/@pattern@ keyword line.
-}
isValueBinding :: Text -> Bool
isValueBinding lhs = case T.words lhs of
    [w] -> notElem w keywords && startsLowerName w
    _ -> False
  where
    keywords = ["type", "data", "newtype", "class", "pattern", "instance"]

{- | True when the (possibly qualified) name's final component begins with a
lowercase letter, i.e. it is a function rather than a constructor or type.
-}
startsLowerName :: Text -> Bool
startsLowerName w = case T.uncons (lastComponent w) of
    Just (c, _) -> c == '_' || isAsciiLower c
    Nothing -> False

-- | The component after the final @.@ of a qualified name.
lastComponent :: Text -> Text
lastComponent = last . T.splitOn "."

{- | Normalise a @:browse@ name to the form the sketch writes. Under
@Unqualified@ the module qualifier is stripped (@Granite.Svg.bars@ -> @bars@);
under @QualifiedAs p@ the chosen prefix replaces the module path
(@Plot.bar@ -> @Plot.bar@, keeping the qualifier).
-}
normalizeName :: ImportStyle -> Text -> Text
normalizeName Unqualified n = lastComponent n
normalizeName (QualifiedAs p) n = p <> "." <> lastComponent n

{- | The display envelope a plotter's result type selects. The result type is
the final arrow component of the signature.
-}
displayEnvelope :: Text -> Envelope
displayEnvelope sig
    | result == "IO ()" = Bare
    | result == "Text" = DisplaySvgUnpack
    | any (`T.isInfixOf` result) ["Html", "HtmlPlot", "SVG", "Svg"] = DisplayHtml
    | otherwise = Hole
  where
    result = resultType sig

-- | The final component of a function signature, after the last top-level @->@.
resultType :: Text -> Text
resultType = T.strip . last . splitArrows
  where
    splitArrows = map T.strip . T.splitOn " -> "

{- | Wrap a call expression in its envelope. @Bare@ leaves it; @Text@ feeds it
through @displaySvg . T.unpack@; @Html@/@SVG@ through @displayHtml@; @Hole@
leaves a typed blank for the model.
-}
applyEnvelope :: Envelope -> Text -> Text
applyEnvelope Bare call = call
applyEnvelope DisplaySvgUnpack call = "displaySvg (T.unpack (" <> call <> "))"
applyEnvelope DisplayHtml call = "displayHtml (" <> call <> ")"
applyEnvelope Hole call = "_ (" <> call <> ")"

{- | The canonical plotter terminals the synth must surface from a Granite.Svg
@:browse@; the generator coverage test keys each against that surface.
-}
grammarTerminals :: [Text]
grammarTerminals = ["bars", "lineGraph", "pie"]

{- | A short, single-screen reference to the discovery search tools and the
@-- cabal:@ / display mechanics, so the prompt carries no long, easily-overfit
list of hard-coded examples.
-}
grammarPromptBlock :: Text
grammarPromptBlock =
    T.unlines
        [ "## Finding things — search, don't guess or recall from memory:"
        , ""
        , "  find_package <task>        a package + its `-- cabal: build-depends:` line"
        , "  find_function <name|mod>   a function by name/keyword, or a module's exports"
        , "  find_by_type <type>        a function whose type fits, e.g. [Double] -> Picture"
        , "  list_bindings              values/types already in scope this session — reuse them"
        , "  check_type <expr>          the type of an expression or name"
        , "  find_example_cell <idiom>  a paste-able cell, e.g. the typed-column CSV reader (a wrong column is a compile error)"
        , ""
        , "Install a package by running a cell whose FIRST line is `-- cabal: build-depends: <pkg>`."
        , "A plotter returns Text — show it with `displaySvg (T.unpack (...))`."
        , "Avoid `main = do`, a top-level `let`, hand-parsing data, and `:set -package`."
        ]
