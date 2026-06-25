{-# LANGUAGE OverloadedStrings #-}

{- | E1 on-the-fly grammar prompting. A small grammar of the notebook plotting
idiom is synthesised from live @:browse@ output: each plotter combinator becomes
a terminal, qualified names are normalised against how the sketch imports their
module, and a type-directed display envelope wraps the call so the result shows
in the notebook. The grammar block plus the worked (request, grammar, cell)
few-shot triplets are injected into both the eval and the production system
prompt so the model predicts the grammar first and then writes the cell against
it, instead of falling back to generic Haskell.

The verb-to-Granite sketch is the grammar's start symbol: a plot request
expands to one plotter terminal applied to its series and options, then wrapped
in the display envelope its result type selects.
-}
module Sabela.AI.Grammar (
    BrowseEntry (..),
    ImportStyle (..),
    Envelope (..),
    parseBrowse,
    normalizeName,
    displayEnvelope,
    applyEnvelope,
    FewShot (..),
    fewShots,
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

{- | A worked (request, minimal grammar, cell) triplet. @fsGrammar@ lists the
plotter terminals the cell uses in their normalised (unqualified) form, so the
coverage test can key each against the @:browse@ surface.
-}
data FewShot = FewShot
    { fsRequest :: Text
    , fsGrammar :: [Text]
    , fsCell :: Text
    }
    deriving (Eq, Show)

{- | The gold few-shot triplets, lifted from @examples/plotting.md@. Each cell
is the real working pattern; @fsGrammar@ records the normalised plotter
terminals it stands on.
-}
fewShots :: [FewShot]
fewShots =
    [ FewShot
        "Bar chart of quarterly sales"
        ["bars"]
        ( T.unlines
            [ "-- cabal: build-depends: text, granite"
            , "{-# LANGUAGE OverloadedStrings #-}"
            , "import qualified Data.Text as T"
            , "import Granite.Svg"
            , ""
            , "displaySvg (T.unpack (bars [(\"Q1\",12),(\"Q2\",18),(\"Q3\",9)] defPlot {plotTitle=\"Sales\"}))"
            ]
        )
    , FewShot
        "Line chart of monthly trends"
        ["lineGraph"]
        ( T.unlines
            [ "-- cabal: build-depends: text, granite"
            , "{-# LANGUAGE OverloadedStrings #-}"
            , "import qualified Data.Text as T"
            , "import Granite.Svg"
            , ""
            , "displaySvg (T.unpack (lineGraph [(\"A\", [(1,100),(2,120),(3,115)])] defPlot {plotTitle=\"Trends\"}))"
            ]
        )
    , FewShot
        "Pie chart of market share"
        ["pie"]
        ( T.unlines
            [ "-- cabal: build-depends: text, granite"
            , "{-# LANGUAGE OverloadedStrings #-}"
            , "import qualified Data.Text as T"
            , "import Granite.Svg"
            , ""
            , "displaySvg (T.unpack (pie [(\"Alpha\",0.35),(\"Beta\",0.25),(\"Gamma\",0.4)] defPlot {plotTitle=\"Share\"}))"
            ]
        )
    ]

{- | Every plotter terminal the few-shots stand on, keyed on the normalised
(unqualified) form. The coverage test asserts each is produced by @:browse@ plus
normalisation plus the display surface.
-}
grammarTerminals :: [Text]
grammarTerminals = concatMap fsGrammar fewShots

{- | The grammar block plus worked few-shot triplets, ready to inject into the
system prompt. The start symbol is the verb-to-Granite sketch; the productions
name the discovered plotter terminals and the display envelope.
-}
grammarPromptBlock :: Text
grammarPromptBlock =
    T.unlines $
        [ "## Plotting grammar (predict this first, then write the cell)"
        , ""
        , "A plot request expands to the verb-to-Granite sketch:"
        , ""
        , "  plot      ::= envelope (plotter series options)"
        , "  plotter   ::= bars | lineGraph | pie | scatter | boxPlot | ..."
        , "                (the real names :browse Granite.Svg surfaced;"
        , "                 write them UNQUALIFIED, e.g. `bars`, dropping the"
        , "                 module prefix the browse output prints)"
        , "  options   ::= defPlot { plotTitle = ..., ... }"
        , "  envelope  ::= displaySvg . T.unpack    -- when the plotter returns Text"
        , "             |  <bare call>              -- when it returns IO ()"
        , "             |  displayHtml              -- when it returns Html/SVG"
        , ""
        , "Predict the minimal grammar for the request, then write ONE cell"
        , "against it. Do not write `main = do`, do not hand-parse CSV, do not"
        , "reach for a plotting library the grammar did not name."
        , ""
        , "### Worked examples"
        ]
            ++ concatMap renderFewShot fewShots
            ++ dataFrameSection

{- | The dataframe idiom for loading and reading a CSV in the working directory,
verified against dataframe-2.3.0.0. Needs @TypeApplications@ and
@OverloadedStrings@ (the latter for @D.col@'s name argument).
-}
dataFrameSection :: [Text]
dataFrameSection =
    [ ""
    , "## DataFrame grammar (load and read a CSV in the working directory)"
    , ""
    , "A data request over a named CSV expands to:"
    , ""
    , "  load   ::= df <- D.readCsv \"<file>.csv\""
    , "  column ::= D.columnAsList (D.col @<Type> \"<name>\") df"
    , "  result ::= a top-level binding over the column lists (sum, mean, zip, ...)"
    , ""
    , "### Worked examples"
    ]
        ++ renderCell
            "Total revenue from revenue.csv (columns month, revenue)"
            dfTotalCell
        ++ renderCell
            "Bar chart of revenue by month from revenue.csv"
            dfChartCell

renderCell :: Text -> [Text] -> [Text]
renderCell request cell =
    ["", "Request: " <> request, "Cell:", "```haskell"] ++ cell ++ ["```"]

dfTotalCell :: [Text]
dfTotalCell =
    [ "-- cabal: build-depends: dataframe, text"
    , "{-# LANGUAGE TypeApplications #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "import qualified DataFrame as D"
    , ""
    , "df <- D.readCsv \"revenue.csv\""
    , "revenueTotal :: Double"
    , "revenueTotal = sum (D.columnAsList (D.col @Double \"revenue\") df)"
    ]

dfChartCell :: [Text]
dfChartCell =
    [ "-- cabal: build-depends: dataframe, granite, text"
    , "{-# LANGUAGE TypeApplications #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "import qualified DataFrame as D"
    , "import qualified Data.Text as T"
    , "import Granite.Svg"
    , ""
    , "df <- D.readCsv \"revenue.csv\""
    , "displaySvg (T.unpack (bars (zip (D.columnAsList (D.col @T.Text \"month\") df) (D.columnAsList (D.col @Double \"revenue\") df)) defPlot {plotTitle = \"Monthly Revenue\"}))"
    ]

renderFewShot :: FewShot -> [Text]
renderFewShot fs =
    [ ""
    , "Request: " <> fsRequest fs
    , "Grammar: plot ::= displaySvg . T.unpack ("
        <> T.intercalate " | " (fsGrammar fs)
        <> " series options)"
    , "Cell:"
    , "```haskell"
    ]
        ++ T.lines (T.stripEnd (fsCell fs))
        ++ ["```"]
