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
    discoverGrammarBlock,
    displayEnvelope,
    grammarPromptBlock,
    grammarTerminals,
    normalizeName,
    parseBrowse,
 )
import Sabela.AI.Grammar.Synth (
    Surface (..),
    exclusivityViolations,
    synthesizeGrammar,
    synthesizeGrammarProven,
    usedNames,
 )

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
        it "points at the product search tools instead of dumping examples" $ do
            ("find_function" `T.isInfixOf` block) `shouldBe` True
            ("find_example_cell" `T.isInfixOf` block) `shouldBe` True
        it "keeps the -- cabal: mechanism but carries no worked example cells" $ do
            ("-- cabal: build-depends:" `T.isInfixOf` block) `shouldBe` True
            ("D.readCsv \"revenue.csv\"" `T.isInfixOf` block) `shouldBe` False
            ("Granite.Svg.bars" `T.isInfixOf` block) `shouldBe` False
        it "nudges toward the typed-column idiom for CSV column work" $ do
            ("typed-column" `T.isInfixOf` block) `shouldBe` True
            ("compile error" `T.isInfixOf` block) `shouldBe` True

    describe "discoverGrammarBlock (the siza surface's cheat-sheet)" $ do
        let block = discoverGrammarBlock
        it "points at discover, not the tools that catalogue no longer offers" $ do
            ("discover" `T.isInfixOf` block) `shouldBe` True
            ("find_function" `T.isInfixOf` block) `shouldBe` False
            ("search_capability" `T.isInfixOf` block) `shouldBe` False
            ("find_example_cell" `T.isInfixOf` block) `shouldBe` False
        it "keeps the -- cabal: install mechanism" $
            ("-- cabal: build-depends:" `T.isInfixOf` block) `shouldBe` True

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

    describe "exclusivity-language lint (R9.7 / search-api §6)" $ do
        it "flags the banned exclusivity fixtures" $ do
            exclusivityViolations "use ONLY these names" `shouldNotBe` []
            exclusivityViolations "Use only these names; nothing else exists"
                `shouldNotBe` []
            exclusivityViolations "no other names may be written"
                `shouldNotBe` []
        it "passes plain non-exclusive card text" $
            exclusivityViolations
                "## Live API grammar — verified names, not exhaustive"
                `shouldBe` []
        it "every synthesized card is lint-clean (generated surfaces)" $
            mapM_
                ( \surfs ->
                    exclusivityViolations (synthesizeGrammar surfs) `shouldBe` []
                )
                surfaceGrid

    describe "R9.7 completeness: a post-compile card includes every proven name" $ do
        it "a compile-proven name survives the noise filter (fixture first)" $ do
            -- `metric` returns String (noise class) and `colE` returns internal
            -- currency (noise class); the compiled cell uses BOTH, so the card
            -- must include them — the revenueTotal `col` omission class.
            let browse =
                    T.unlines
                        [ "D.readCsv :: FilePath -> IO D.DataFrame"
                        , "D.metric :: D.DataFrame -> String"
                        , "D.colE :: T.Text -> D.NumExpr Double"
                        ]
                cell = "total = D.metric df\ne = D.colE \"revenue\""
                block =
                    synthesizeGrammarProven
                        (usedNames cell)
                        [Surface "DataFrame" (QualifiedAs "D") browse]
            ("D.metric ::" `T.isInfixOf` block) `shouldBe` True
            ("D.colE ::" `T.isInfixOf` block) `shouldBe` True
        it "unproven noise stays filtered alongside proven names" $ do
            let browse =
                    T.unlines
                        [ "D.metric :: D.DataFrame -> String"
                        , "D.ttrace :: String -> a -> a"
                        ]
                block =
                    synthesizeGrammarProven
                        (usedNames "x = D.metric df")
                        [Surface "DataFrame" (QualifiedAs "D") browse]
            ("D.metric ::" `T.isInfixOf` block) `shouldBe` True
            ("ttrace" `T.isInfixOf` block) `shouldBe` False
        it "property: every parsed-uses ∩ export-set name appears (generated grid)" $
            mapM_
                ( \(cell, browse, style, expectPresent) -> do
                    let block =
                            synthesizeGrammarProven
                                (usedNames cell)
                                [Surface "M" style browse]
                    mapM_
                        ( \n ->
                            ((cell, n), (n <> " ::") `T.isInfixOf` block)
                                `shouldBe` ((cell, n), True)
                        )
                        expectPresent
                )
                completenessGrid

{- | Generated surface grid for the lint property: every import style crossed
with clean, noisy, and empty browse bodies.
-}
surfaceGrid :: [[Surface]]
surfaceGrid =
    [ [Surface "M" style browse]
    | style <- [Unqualified, QualifiedAs "Q"]
    , browse <-
        [ ""
        , "M.f :: Int -> Text"
        , "M.err :: Int -> String\nM.ttrace :: String -> a -> a"
        , graniteSvgBrowse
        ]
    ]
        ++ [[]]

{- | Generated (cell, browse, style, expected-present) cases: the proven set
is the cell's lexical uses; expectation lists each uses ∩ exports name in
its normalised written form.
-}
completenessGrid :: [(Text, Text, ImportStyle, [Text])]
completenessGrid =
    [
        ( "y = shout x"
        , "M.shout :: Int -> String"
        , Unqualified
        , ["shout"]
        )
    ,
        ( "y = Q.shout x\nz = Q.quiet y"
        , "M.shout :: Int -> String\nM.quiet :: Int -> Int"
        , QualifiedAs "Q"
        , ["Q.shout", "Q.quiet"]
        )
    ,
        ( "y = plain 1"
        , "M.plain :: Int -> Int"
        , Unqualified
        , ["plain"]
        )
    ]
