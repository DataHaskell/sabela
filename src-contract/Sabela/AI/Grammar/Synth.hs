{-# LANGUAGE OverloadedStrings #-}

{- | Synthesise a notebook-DSL grammar from live @:browse@ output. Where
'Sabela.AI.Grammar' carries the static cold-start block, this turns the real,
version-current surface a @:browse M@ returned into a grammar fragment: each
value binding becomes a terminal written in the form the cell imports it
(normalised), tagged with the display envelope its result type selects, under the
verb-to-sketch start symbol. Injected after a discover/browse so the model
predicts against names that actually exist rather than inventing them.
-}
module Sabela.AI.Grammar.Synth (
    Surface (..),
    synthesizeGrammar,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Grammar (
    BrowseEntry (..),
    Envelope (..),
    ImportStyle (..),
    displayEnvelope,
    normalizeName,
    parseBrowse,
 )

{- | One browsed module: its name, how the cell imports it (which decides
qualifier normalisation), and the raw @:browse@ text it returned.
-}
data Surface = Surface
    { surfModule :: Text
    , surfStyle :: ImportStyle
    , surfBrowse :: Text
    }
    deriving (Eq, Show)

{- | Render the live grammar for one or more browsed surfaces: a header naming
the start symbol, then per surface its discovered terminals in normalised write
form, each tagged with the display envelope its result type selects. A surface
whose browse holds no value bindings contributes only its heading.
-}
synthesizeGrammar :: [Surface] -> Text
synthesizeGrammar surfaces =
    T.unlines (header ++ concatMap renderSurface surfaces)
  where
    header =
        [ "## Live API grammar (synthesised from :browse — use ONLY these names)"
        , ""
        , "A request expands to one discovered terminal applied to its arguments,"
        , "then shown through the envelope its result type selects. Write each name"
        , "in the form below; do not invent a name or reach for another library."
        ]

renderSurface :: Surface -> [Text]
renderSurface (Surface m style browse) =
    [ ""
    , "### " <> m <> "  (" <> importHint style <> ")"
    ]
        ++ map (renderTerminal style) (parseBrowse browse)

renderTerminal :: ImportStyle -> BrowseEntry -> Text
renderTerminal style (BrowseEntry name typ) =
    "  "
        <> normalizeName style name
        <> " :: "
        <> typ
        <> envHint (displayEnvelope typ)

importHint :: ImportStyle -> Text
importHint Unqualified = "imported unqualified — drop the module prefix"
importHint (QualifiedAs p) =
    "imported qualified as " <> p <> " — keep the " <> p <> ". prefix"

envHint :: Envelope -> Text
envHint DisplaySvgUnpack = "   -- show: displaySvg (T.unpack (…))"
envHint DisplayHtml = "   -- show: displayHtml (…)"
envHint Bare = "   -- returns IO (); call it directly"
envHint Hole = ""
