{-# LANGUAGE OverloadedStrings #-}

{- | What a cell's trailing expression does now that GHCi prints it through
the injected 'Display' class: a value that opted in renders itself, and every
value that did not prints exactly as it printed before.
-}
module Test.DisplaySpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import Test.Hspec

import Sabela.Diagnose (Guidance (..), diagnoseWith)
import Sabela.Errors (parseErrors)
import Sabela.Model (
    Cell (..),
    CellError (..),
    CellType (..),
    Notebook (..),
 )
import Sabela.Output (displayPrelude)
import Sabela.Output.Display (
    displayPragmas,
    interactivePrintName,
    interactivePrintSetting,
 )
import Sabela.Output.Optional (
    OptionalDisplay (..),
    declaresPackage,
    optionalDisplays,
    wantedBy,
 )
import Sabela.SessionTypes (CellLang (..))
import Test.Live (liveSpecs, requireExecutable)

spec :: Spec
spec = describe "type-directed display" $ do
    assemblySpec
    printerNameSpec
    dataFrameOfferSpec
    liveSpecs renderingSpec

{- | A display for a package's type can only compile where the package is, so
each is offered by what the notebook declared — except the support library,
which the harness puts in every session itself.
-}
dataFrameOfferSpec :: Spec
dataFrameOfferSpec = describe "the displays offered beside the prelude" $ do
    it "sees a declared package through whatever bounds it carries" $
        map (`declaresPackage` "dataframe") (map declaring pinnedSpellings)
            `shouldBe` map (const True) pinnedSpellings
    it "does not take one package's name for another's" $
        declaresPackage (declaring "build-depends: dataframe-viz, text") "dataframe"
            `shouldBe` False
    it "offers the dataframe table view only to a notebook that asked" $ do
        map odName (offered (declaring "build-depends: dataframe"))
            `shouldContain` ["dataframe"]
        map odName (offered (Notebook "n" [])) `shouldNotContain` ["dataframe"]
    it "offers the notebook vocabulary to every session" $
        map odName (offered (Notebook "n" [])) `shouldContain` ["sabela-notebook"]
  where
    offered nb = filter (wantedBy nb) optionalDisplays

pinnedSpellings :: [Text]
pinnedSpellings =
    [ "build-depends: dataframe"
    , "build-depends: dataframe, text"
    , "build-depends: dataframe == 3.3.0.0, text"
    , "build-depends: text, dataframe >= 3 && < 4"
    ]

declaring :: Text -> Notebook
declaring meta =
    Notebook
        "n"
        [Cell 1 CodeCell Haskell ("-- cabal: " <> meta <> "\nx = 1") [] Nothing False]

{- | The printer is ours, but the diagnostics about it are the user's. GHCi
names whatever printed a value it could not show, and a reader — the advice
rules included — has to find the name it has always found there.
-}
printerNameSpec :: Spec
printerNameSpec = describe "a value the session cannot show" $ do
    it "is reported against the printer a stock session names" $ do
        let msgs = map ceMessage (parseErrors unshowable)
        msgs `shouldSatisfy` any (T.isInfixOf "a use of \8216print\8217")
        msgs `shouldSatisfy` all (not . T.isInfixOf interactivePrintName)
    it "still earns the advice that names the wrapper to reach for" $ do
        let advice =
                concatMap
                    (diagnoseWith Nothing "pic" . ceMessage)
                    (parseErrors unshowable)
        map gCategory advice `shouldBe` ["unshowable-result"]

unshowable :: Text
unshowable =
    T.unlines
        [ "<interactive>:9:1: error: [GHC-39999]"
        , "    \8226 No instance for \8216Show Picture\8217"
        , "        arising from a use of \8216" <> interactivePrintName <> "\8217"
        , "    \8226 In a stmt of an interactive GHCi command: "
            <> interactivePrintName
            <> " it"
        ]

{- | The two orderings the injection depends on: the extensions are set before
the block that needs them, and printing is handed over only once the printer
exists.
-}
assemblySpec :: Spec
assemblySpec = describe "the injected block" $ do
    it "sets its extensions before the block that needs them" $ do
        displayPragmas `shouldNotBe` []
        [p | p <- displayPragmas, indexOf p > indexOf ":{"] `shouldBe` []
    it "hands GHCi the printer only after the block that defines it" $
        indexOf interactivePrintSetting `shouldSatisfy` (> indexOf ":}")
  where
    indexOf needle =
        length (takeWhile (not . T.isPrefixOf needle) (T.lines displayPrelude))

renderingSpec :: Spec
renderingSpec =
    describe "a trailing expression in a real GHCi" $
        it "renders what opted in, and nothing else" $ do
            requireExecutable "ghci"
            out <- probe
            report out (checks out)

checks :: Text -> [(String, Bool)]
checks out =
    [ ("an opted-in record rendered as a grid of its fields", hasAll grid)
    , ("a value carrying its own MIME type rendered as that type", hasAll markdown)
    , ("a list of scalars printed as it always did", hasAll ["[1,2,3]"])
    , ("a string printed as it always did", hasAll ["\"a string\""])
    , ("a record that did not opt in printed as it always did", hasAll untouched)
    , ("nothing errored", not (hasAll ["error:"]))
    ]
  where
    hasAll needles = all (`T.isInfixOf` out) needles
    grid =
        [ "<!-- MIME:text/html -->"
        , "class=\"sabela-grid\""
        , "\"cols\":[\"city\",\"temp\"]"
        , "sabelaGridMountAll()"
        ]
    markdown = ["<!-- MIME:text/markdown -->", "# heading"]
    untouched = ["[Untabulated 1]"]

report :: Text -> [(String, Bool)] -> Expectation
report out results = case [label | (label, ok) <- results, not ok] of
    [] -> pure ()
    failed ->
        expectationFailure
            (unlines failed <> "\nghci said:\n" <> T.unpack out)

{- | One session that injects the real prelude and then evaluates one
expression per claim, so the answers are the compiler's and not a model of it.
-}
probe :: IO Text
probe = do
    (_, out, err) <-
        readProcessWithExitCode "ghci" ["-v0", "-ignore-dot-ghci"] (T.unpack script)
    pure (T.pack out <> T.pack err)

script :: Text
script =
    T.unlines
        [ displayPrelude
        , ":{"
        , "import GHC.Generics (Generic)"
        , ":}"
        , ":{"
        , "data Reading = Reading"
        , "    { city :: SabelaBase.String"
        , "    , temp :: SabelaBase.Double"
        , "    }"
        , "    deriving (SabelaBase.Show, Generic, Tabular)"
        , "data Untabulated = Untabulated SabelaBase.Int deriving (SabelaBase.Show)"
        , ":}"
        , "[Reading \"Bulawayo\" 27.5]"
        , "md \"# heading\""
        , "[1, 2, 3 :: SabelaBase.Int]"
        , "\"a string\""
        , "[Untabulated 1]"
        ]
