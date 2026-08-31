{-# LANGUAGE OverloadedStrings #-}

{- | C1-15c: which rendering a silent cell gets is settled by the notebook,
not by the caller's prose and not by the name of the value's type.
-}
module Test.RenderContractSpec (renderContractSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.RenderContract (
    displayCandidates,
    displayProbe,
    repairDisplayContract,
 )

renderContractSpec :: Spec
renderContractSpec = describe "R10-T3 display contract" $ do
    shapeSpec
    goalSpec
    verdictSpec

shapeSpec :: Spec
shapeSpec = describe "what is proposed" $ do
    it "offers an unpacking for every textual representation" $ do
        let cs = displayCandidates "show the chart" "bars xs plot"
        cs `shouldSatisfy` any (T.isInfixOf "displaySvg (T.unpack (bars xs plot))")
        cs `shouldSatisfy` any (T.isInfixOf "displaySvg (TL.unpack (bars xs plot))")
        cs `shouldSatisfy` any (T.isInfixOf "displaySvg (bars xs plot)")

    it "brings each unpacking's import with it" $
        [ c
        | c <- displayCandidates "show it" "renderZephyr x"
        , "TL.unpack" `T.isInfixOf` c
        ]
            `shouldSatisfy` all (T.isInfixOf "import qualified Data.Text.Lazy as TL")

    it "wraps a multiline bare expression as one expression" $
        displayCandidates "show the chart" "bars\n  xs\n  defPlot"
            `shouldSatisfy` any (T.isInfixOf "displaySvg (T.unpack (bars xs defPlot))")

    it "appends a display of a top-level binding rather than replacing it" $
        displayCandidates "show this" "plotCumulus = renderCumulus input"
            `shouldSatisfy` any
                (T.isInfixOf "plotCumulus = renderCumulus input\ndisplaySvg (plotCumulus)")

    it "sniffs the medium out of the content before the prose" $ do
        mediaIn (displayCandidates "compute it" "\"<svg><path/></svg>\"")
            `shouldBe` ["displaySvg"]
        mediaIn (displayCandidates "compute it" "\"<table><tr></tr></table>\"")
            `shouldBe` ["displayHtml"]

    it "never double-wraps a source that already renders" $ do
        displayCandidates "show it" "displaySvg (T.unpack chart)" `shouldBe` []
        displayCandidates
            "show it"
            "DFDisplay.display DFDisplay.defaultDisplayOptions (housing)"
            `shouldBe` []

    it "proposes more than one rendering, so the notebook has a choice" $
        property $
            forAll genSource $ \src ->
                forAll genGoal $ \goal ->
                    length (displayCandidates goal src) `shouldSatisfy` (>= 3)

    it "proposes the same renderings for a value no table has ever seen" $
        property $
            forAll genIdent $ \name ->
                length (displayCandidates "show it" name)
                    === length (displayCandidates "show it" "housing")

{- | PROPERTY 1. The goal string may pick the medium and nothing else: it can
never decide whether a value is rendered, nor how it is unpacked.
-}
goalSpec :: Spec
goalSpec = describe "goal-independence (C1-15c PROPERTY 1)" $ do
    it "offers a rendering for the same sources whatever the goal says" $
        property $
            forAll genSource $ \src ->
                forAll genGoal $ \g1 ->
                    forAll genGoal $ \g2 ->
                        null (displayCandidates g1 src)
                            === null (displayCandidates g2 src)

    it "changes only the display function when the goal changes" $
        property $
            forAll genSource $ \src ->
                forAll genGoal $ \g1 ->
                    forAll genGoal $ \g2 ->
                        map anonymised (displayCandidates g1 src)
                            === map anonymised (displayCandidates g2 src)

{- | PROPERTY 2. The rendering that survives is the one the notebook accepted.
Nothing about the value's type name enters the decision, so moving the
notebook's verdict moves the outcome with it.
-}
verdictSpec :: Spec
verdictSpec = describe "the notebook decides (C1-15c PROPERTY 2)" $ do
    it "keeps whichever candidate the notebook shows output for" $
        property $
            forAll genSource $ \src ->
                forAll genGoal $ \goal ->
                    let cs = displayCandidates goal src
                     in not (null cs) ==> ioProperty $ do
                            kept <-
                                mapM
                                    (\k -> fst <$> runOffer (is (displayProbe (cs !! k))) (is (cs !! k)) goal src)
                                    [0 .. length cs - 1]
                            pure $
                                counterexample (show (cs, kept)) $
                                    kept == map Just cs

    it "puts the cell back when a rendering it committed shows nothing" $
        property $
            forAll genSource $ \src ->
                forAll genGoal $ \goal ->
                    let cs = displayCandidates goal src
                     in not (null cs) ==> ioProperty $ do
                            (kept, seen) <- runOffer (const True) (const False) goal src
                            pure $
                                counterexample (show (map tcName seen)) $
                                    isNothing kept
                                        && lastReplacement seen == Just src

    it "commits nothing at all when the kernel rejects every rendering" $
        property $
            forAll genSource $ \src ->
                forAll genGoal $ \goal ->
                    let cs = displayCandidates goal src
                     in not (null cs) ==> ioProperty $ do
                            (kept, seen) <- runOffer (const False) (const False) goal src
                            pure $
                                counterexample (show (map tcName seen)) $
                                    isNothing kept
                                        && isNothing (lastReplacement seen)
                                        && length [c | c <- seen, tcName c == "try"]
                                            == length cs

    it "never asks the notebook about a value it will not offer to render" $ do
        (kept, seen) <-
            runOffer (const True) (const True) "show it" "displaySvg (T.unpack chart)"
        kept `shouldBe` Nothing
        map tcName seen `shouldBe` []

    it "does not offer a display repair for a healthy prose acknowledgement" $ do
        seen <- newIORef []
        let prose =
                ToolCall
                    "insert_cell"
                    ( object
                        [ "cell_type" .= ("ProseCell" :: Text)
                        , "source" .= ("A written explanation." :: Text)
                        ]
                    )
            ack =
                Right
                    ( ToolOk
                        ( object
                            [ "cellId" .= (3 :: Int)
                            , "execution" .= Null
                            ]
                        )
                    )
            dispatch tc = modifyIORef' seen (++ [tc]) >> pure ack
        repairDisplayContract "show it" dispatch prose ack `shouldReturn` Nothing
        readIORef seen `shouldReturn` []

is :: Text -> Text -> Bool
is = (==)

-- | The display functions a candidate list actually reaches for.
mediaIn :: [Text] -> [Text]
mediaIn cs = [d | d <- displayFunctions, any (T.isInfixOf d) cs]

{- | A candidate with its display function blanked out, so two candidates can
be compared for everything except the medium.
-}
anonymised :: Text -> Text
anonymised c = foldr (`T.replace` "display") c displayFunctions

displayFunctions :: [Text]
displayFunctions = ["displayMarkdown", "displayHtml", "displaySvg"]

-- | Goals with and without the display words the old prose gate looked for.
genGoal :: Gen Text
genGoal =
    elements
        [ "show the chart"
        , "render the visual"
        , "write a summary report"
        , "compute the median by group"
        , "tabulate the counts"
        , "fit the model"
        , ""
        ]

genSource :: Gen Text
genSource = do
    name <- genIdent
    arg <- genIdent
    elements
        [ name <> " " <> arg
        , name <> " = frobnicate " <> arg
        , "import Quux.Internal\n" <> name <> " " <> arg
        , "-- cabal: build-depends: zzfrob\n" <> name <> " " <> arg
        ]

genIdent :: Gen Text
genIdent = elements ["housing", "grid", "wibble", "zephyrOf", "quuxTable", "m1"]

{- | Drives one repair against a notebook that type-checks the renderings
@fits@ accepts and shows output for the ones @shows_@ accepts.
-}
runOffer ::
    (Text -> Bool) -> (Text -> Bool) -> Text -> Text -> IO (Maybe Text, [ToolCall])
runOffer fits shows_ goal src = do
    calls <- newIORef []
    let original = ToolCall "insert_cell" (object ["source" .= src])
    r <-
        repairDisplayContract
            goal
            (dispatchOver calls fits shows_)
            original
            (cellOutcome [])
    seen <- readIORef calls
    pure (fmap (sourceOf . fst) r, seen)

dispatchOver ::
    IORef [ToolCall] ->
    (Text -> Bool) ->
    (Text -> Bool) ->
    ToolCall ->
    IO (Either Text ToolOutcome)
dispatchOver calls fits shows_ tc = do
    modifyIORef' calls (++ [tc])
    pure $ case tcName tc of
        "list_cells" -> Right (ToolOk cellsSnapshot)
        "try"
            | fits (argOf "code" tc) -> Right (ToolOk (object []))
            | otherwise -> Right (ToolErr (object ["error" .= ("no" :: Text)]))
        "replace_cell_source"
            | shows_ (sourceOf tc) -> Right (ToolOk (cellValue [svgOutput]))
            | otherwise -> Right (ToolOk (cellValue []))
        _ -> Right (ToolOk (object []))

cellOutcome :: [Value] -> Either Text ToolOutcome
cellOutcome = Right . ToolOk . cellValue

cellValue :: [Value] -> Value
cellValue outputs =
    object
        [ "cellId" .= (3 :: Int)
        , "execution" .= object ["ok" .= True, "outputs" .= outputs]
        ]

svgOutput :: Value
svgOutput =
    object ["oiMime" .= ("image/svg+xml" :: Text), "oiOutput" .= ("<svg/>" :: Text)]

cellsSnapshot :: Value
cellsSnapshot =
    object
        [ "cells"
            .= [ object ["id" .= (3 :: Int), "hasError" .= False, "defines" .= ([] :: [Text])]
               ]
        ]

lastReplacement :: [ToolCall] -> Maybe Text
lastReplacement seen =
    case reverse [sourceOf c | c <- seen, tcName c == "replace_cell_source"] of
        (s : _) -> Just s
        [] -> Nothing

sourceOf :: ToolCall -> Text
sourceOf = argOf "new_source"

argOf :: Text -> ToolCall -> Text
argOf k tc = case tcArgs tc of
    Object o -> case KM.lookup (K.fromText k) o of
        Just (String s) -> s
        _ -> ""
    _ -> ""
