{-# LANGUAGE OverloadedStrings #-}

{- | General invariants over GENERATED module APIs (R3.10/R3.6/R3.9), not a
granite reproduction: for functions, operators, record types with fields,
classes, and mixed surfaces — every function export renders with a
paste-valid signature, field fragments never displace functions, no
package-version or internal-module token survives to the rendered surface,
the card is one envelope shape, and the 2.5k budget reconciles
shown/omitted/total over export lists of size 1..200.
-}
module Test.GrammarCardSpec (grammarCardSpec) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Grammar (ImportStyle (..))
import Sabela.AI.Grammar.Synth (
    Surface (..),
    cardCounts,
    synthesizeGrammarBounded,
    synthesizeGrammarProven,
 )

-- | One generated export: its name, full signature, and export class.
data GenExport = GenExport
    { geName :: Text
    , geType :: Text
    , geClass :: ExportClass
    }

data ExportClass = FnExport | OpExport
    deriving (Eq)

-- | A generated module API: functions/operators plus record and class decls.
data GenApi = GenApi
    { gaLabel :: Text
    , gaExports :: [GenExport]
    , gaRecords :: [(Text, [(Text, Text)])]
    , gaClasses :: [(Text, [(Text, Text)])]
    }

genApis :: [GenApi]
genApis =
    [ GenApi
        "functions-only"
        [ GenExport "alpha" "Int -> Int" FnExport
        , GenExport "beta" "[(Text, Double)] -> Plot -> Text" FnExport
        ]
        []
        []
    , GenApi
        "operators"
        [ GenExport "(<+>)" "Widget -> Widget -> Widget" OpExport
        , GenExport "gamma" "Double -> Widget" FnExport
        ]
        []
        []
    , GenApi
        "record-with-fields"
        [GenExport "bars" "[(Text, Double)] -> Plot -> Text" FnExport]
        [
            ( "Plot"
            , [("tickIndex", "Int"), ("lo", "Double"), ("hi", "Double")]
            )
        ]
        []
    , GenApi
        "class-decl"
        [GenExport "delta" "Air -> Air" FnExport]
        []
        [("Breezy", [("waft", "a -> Air"), ("becalm", "Air -> a")])]
    , GenApi
        "mixed"
        [ GenExport "epsilon" "Sky -> Rain" FnExport
        , GenExport
            "zeta"
            "Columnable a => Text -> DataFrame -> [a]"
            FnExport
        ]
        [("Sky", [("clouds", "[Cloud]"), ("wind", "Wind")])]
        [("Wet", [("soak", "a -> Rain")])]
    ]

{- | Render an API as GHC-style @:browse@ text: wrapped signatures for long
types, indented record fields and class methods.
-}
renderBrowse :: GenApi -> Text
renderBrowse api =
    T.intercalate "\n" (concatMap expLines (gaExports api) ++ decls)
  where
    expLines e
        | T.length (geType e) > 30 =
            [geName e <> " ::", "  " <> geType e]
        | otherwise = [geName e <> " :: " <> geType e]
    decls =
        concatMap recordLines (gaRecords api)
            ++ concatMap classLines (gaClasses api)
    recordLines (ty, fields) =
        ("data " <> ty)
            : [ "  "
                    <> lead i
                    <> fn
                    <> " :: "
                    <> ft
                    <> close i (length fields)
              | (i, (fn, ft)) <- zip [1 :: Int ..] fields
              ]
      where
        lead i = if i == 1 then "= " <> ty <> " {" else "   "
        close i n = if i == n then "}" else ","
    classLines (cls, methods) =
        ("class " <> cls <> " a where")
            : ["  " <> mn <> " :: " <> mt | (mn, mt) <- methods]

-- | The flattening leak variant: every line loses its leading whitespace.
stripIndent :: Text -> Text
stripIndent = T.unlines . map T.stripStart . T.lines

surfaceOf :: Text -> Surface
surfaceOf = Surface "Gen.Module" Unqualified

cardOf :: Text -> Text
cardOf browseText = synthesizeGrammarProven [] [surfaceOf browseText]

variants :: GenApi -> [(Text, Text)]
variants api =
    [ ("ghc-layout", renderBrowse api)
    , ("stripped-indent", stripIndent (renderBrowse api))
    ]

grammarCardSpec :: Spec
grammarCardSpec = describe "synthesized card over generated APIs (R3.10)" $ do
    describe "every function export renders with a paste-valid signature" $
        forM_ genApis $ \api ->
            forM_ (variants api) $ \(vlabel, browseText) ->
                it (T.unpack (gaLabel api <> "/" <> vlabel)) $ do
                    let card = cardOf browseText
                    forM_ [e | e <- gaExports api, geClass e == FnExport] $ \e ->
                        card
                            `shouldSatisfy` T.isInfixOf (geName e <> " :: " <> geType e)

    describe "field fragments never displace functions" $
        forM_ genApis $ \api ->
            forM_ (variants api) $ \(vlabel, browseText) ->
                it (T.unpack (gaLabel api <> "/" <> vlabel)) $ do
                    let cardLines = T.lines (cardOf browseText)
                    forM_ cardLines $ \l -> do
                        l `shouldSatisfy` (not . T.isSuffixOf ",")
                        l `shouldSatisfy` (not . T.isInfixOf "}")
                    forM_ (concatMap snd (gaRecords api)) $ \(fn, _) ->
                        [l | l <- cardLines, (fn <> " ::") `T.isPrefixOf` T.stripStart l]
                            `shouldBe` []

    describe "no package-version or internal-module token in the surface" $
        it "strips pkg-version qualifiers and .Internal. paths" $ do
            let browseText =
                    T.unlines
                        [ "decode :: ByteString -> Maybe aeson-2.3.1.0:Data.Aeson.Types.Internal.Value"
                        , "encode :: aeson-2.3.1.0:Data.Aeson.Types.Internal.Value -> ByteString"
                        ]
                card = cardOf browseText
            card `shouldSatisfy` (not . T.isInfixOf "aeson-2.3.1.0:")
            card `shouldSatisfy` (not . T.isInfixOf ".Internal.")
            card `shouldSatisfy` T.isInfixOf "decode :: ByteString -> Maybe Value"

    describe "one envelope shape" $
        forM_ genApis $ \api ->
            it (T.unpack (gaLabel api)) $ do
                let card = cardOf (renderBrowse api)
                T.isInfixOf "Live API grammar" card `shouldBe` True

    describe "budget with reconciling counts over 1..200 exports (R3.9)" $
        forM_ ([1, 2, 5, 20, 80, 200] :: [Int]) $ \n ->
            it ("export list of size " <> show n) $ do
                let browseText =
                        T.unlines
                            [ "fn"
                                <> T.pack (show i)
                                <> " :: Columnable a => Text -> DataFrame -> [a]"
                            | i <- [1 .. n]
                            ]
                    card =
                        synthesizeGrammarBounded 2500 [] [surfaceOf browseText]
                    (shown, omitted, total) = cardCounts card
                T.length card `shouldSatisfy` (<= 2500)
                total `shouldBe` n
                shown + omitted `shouldBe` total
