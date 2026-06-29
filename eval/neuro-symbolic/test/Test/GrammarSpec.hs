{-# LANGUAGE OverloadedStrings #-}

module Test.GrammarSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Grammar (
    BrowseEntry (..),
    Envelope (..),
    ImportStyle (..),
    applyEnvelope,
    displayEnvelope,
    grammarPromptBlock,
    grammarTerminals,
    normalizeName,
    parseBrowse,
 )
import Sabela.AI.Grammar.Synth (Surface (..), synthesizeGrammar)

graniteSvgBrowse :: Text
graniteSvgBrowse =
    T.unlines
        [ "Granite.Svg.area :: [(Text, [(Double, Double)])] -> Granite.Plot -> Text"
        , "Granite.Svg.bars :: [(Text, Double)] -> Granite.Plot -> Text"
        , "Granite.Svg.boxPlot :: [(Text, [Double])] -> Granite.Plot -> Text"
        , "Granite.Svg.lineGraph :: [(Text, [(Double, Double)])] -> Granite.Plot -> Text"
        , "Granite.Svg.pie :: [(Text, Double)] -> Granite.Plot -> Text"
        , "Granite.Svg.scatter :: [(Text, [(Double, Double)])] -> Granite.Plot -> Text"
        , "type Granite.Plot :: *"
        , "data Granite.Plot = Granite.Plot {Granite.widthChars :: Int}"
        , "Granite.defPlot :: Granite.Plot"
        ]

-- | A slice of @:browse@ for the qualified @Plot@ module (IO HtmlPlot readers).
plotBrowse :: Text
plotBrowse =
    T.unlines
        [ "Plot.plotBars :: Text -> DataFrame -> IO Plot.HtmlPlot"
        , "Plot.showInDefaultBrowser :: Plot.HtmlPlot -> IO ()"
        ]

{- | The normalised plotter terminals :browse + normalisation surface for the
unqualified Granite.Svg sketch.
-}
graniteTerminals :: [Text]
graniteTerminals =
    [ normalizeName Unqualified (beName e)
    | e <- parseBrowse graniteSvgBrowse
    ]

spec :: Spec
spec = describe "E1 grammar prompting" $ do
    describe "parseBrowse" $ do
        it "keeps value bindings and drops type/data/keyword lines" $
            map beName (parseBrowse graniteSvgBrowse)
                `shouldBe` [ "Granite.Svg.area"
                           , "Granite.Svg.bars"
                           , "Granite.Svg.boxPlot"
                           , "Granite.Svg.lineGraph"
                           , "Granite.Svg.pie"
                           , "Granite.Svg.scatter"
                           , "Granite.defPlot"
                           ]

    describe "normalizeName (normalization fixture)" $ do
        it "strips the qualifier for an unqualified-imported module -> bars" $
            normalizeName Unqualified "Granite.Svg.bars" `shouldBe` "bars"

        it "never leaves the qualified form for an unqualified import" $
            normalizeName Unqualified "Granite.Svg.bars"
                `shouldNotBe` "Granite.Svg.bars"

        it "keeps the qualifier for Plot.* (qualified import)" $
            normalizeName (QualifiedAs "Plot") "Plot.plotBars"
                `shouldBe` "Plot.plotBars"

    describe "displayEnvelope (envelope fixture)" $ do
        it "picks displaySvg . T.unpack for a Text plotter" $ do
            let bars =
                    [beType e | e <- parseBrowse graniteSvgBrowse, beName e == "Granite.Svg.bars"]
            map displayEnvelope bars `shouldBe` [DisplaySvgUnpack]
            applyEnvelope DisplaySvgUnpack "bars xs defPlot"
                `shouldBe` "displaySvg (T.unpack (bars xs defPlot))"

        it "picks a bare call for an IO () plotter" $ do
            let ios =
                    [ beType e | e <- parseBrowse plotBrowse, beName e == "Plot.showInDefaultBrowser"
                    ]
            map displayEnvelope ios `shouldBe` [Bare]
            applyEnvelope Bare "Plot.showInDefaultBrowser p"
                `shouldBe` "Plot.showInDefaultBrowser p"

        it "picks displayHtml for an Html/SVG producer" $
            displayEnvelope "Text -> DataFrame -> IO Plot.HtmlPlot"
                `shouldBe` DisplayHtml

    describe "GENERATOR coverage (terminals from :browse + normalization + surface)" $ do
        it
            "every few-shot terminal is produced by the Granite.Svg surface, keyed on the unqualified form"
            $ mapM_
                (\t -> (t `elem` graniteTerminals) `shouldBe` True)
                grammarTerminals

        it "every few-shot terminal normalises to a single unqualified word" $
            mapM_
                (\t -> ("." `T.isInfixOf` t) `shouldBe` False)
                grammarTerminals

    describe "grammarPromptBlock (short: points at the search tools)" $ do
        let block = grammarPromptBlock
        it "points at find_package and find_example_cell instead of dumping examples" $ do
            ("find_package" `T.isInfixOf` block) `shouldBe` True
            ("find_example_cell" `T.isInfixOf` block) `shouldBe` True
        it "keeps the -- cabal: mechanism but carries no worked example cells" $ do
            ("-- cabal: build-depends:" `T.isInfixOf` block) `shouldBe` True
            ("D.readCsv \"revenue.csv\"" `T.isInfixOf` block) `shouldBe` False
            ("Granite.Svg.bars" `T.isInfixOf` block) `shouldBe` False

        it "nudges toward the typed-column idiom for CSV column work" $ do
            ("typed-column" `T.isInfixOf` block) `shouldBe` True
            ("compile error" `T.isInfixOf` block) `shouldBe` True

    describe "synthesizeGrammar (live :browse -> grammar prior)" $ do
        let graniteBlock =
                synthesizeGrammar [Surface "Granite.Svg" Unqualified graniteSvgBrowse]
            plotBlock =
                synthesizeGrammar [Surface "Plot" (QualifiedAs "Plot") plotBrowse]

        it "lists discovered plotters in normalised (unqualified) form" $ do
            ("bars ::" `T.isInfixOf` graniteBlock) `shouldBe` True
            ("defPlot ::" `T.isInfixOf` graniteBlock) `shouldBe` True
            ("Granite.Svg.bars" `T.isInfixOf` graniteBlock) `shouldBe` False

        it "tags a Text plotter with the displaySvg envelope" $
            ("displaySvg (T.unpack" `T.isInfixOf` graniteBlock) `shouldBe` True

        it "drops type/data/class lines, keeping only value terminals" $
            ("data Granite.Plot" `T.isInfixOf` graniteBlock) `shouldBe` False

        it "keeps the qualifier and Html envelope for a qualified IO-Html reader" $ do
            ("Plot.plotBars ::" `T.isInfixOf` plotBlock) `shouldBe` True
            ("displayHtml" `T.isInfixOf` plotBlock) `shouldBe` True

        it "marks an IO () terminal as called directly" $
            ("returns IO (); call it directly" `T.isInfixOf` plotBlock) `shouldBe` True

        it "synthesises only from the surface, never inventing a name" $ do
            let names = map beName (parseBrowse graniteSvgBrowse)
                normalised = map (normalizeName Unqualified) names
            mapM_
                (\n -> ((n <> " ::") `T.isInfixOf` graniteBlock) `shouldBe` True)
                normalised

        it "drops noise terminals (error/diagnostic, trace, internal currency)" $ do
            let noisy =
                    T.unlines
                        [ "D.readCsv :: FilePath -> IO D.DataFrame"
                        , "D.columnNotFound :: T.Text -> T.Text -> [T.Text] -> String"
                        , "D.ttrace :: String -> a -> a"
                        , "D.combineAndVec :: D.CondVec -> D.CondVec -> D.CondVec"
                        ]
                block = synthesizeGrammar [Surface "DataFrame" (QualifiedAs "D") noisy]
            ("readCsv ::" `T.isInfixOf` block) `shouldBe` True
            ("columnNotFound" `T.isInfixOf` block) `shouldBe` False
            ("ttrace" `T.isInfixOf` block) `shouldBe` False
            ("combineAndVec" `T.isInfixOf` block) `shouldBe` False
