{-# LANGUAGE OverloadedStrings #-}

{- | R6-T3 construct-facet steering, unit half (R5.6/R5.7/R5.9): shape-keyed
steering to @mode="construct"@ by miss 2, followable against the synthetic
catalogue, plus the budget-pressure floor. Loop half: "Test.SteerLoopSpec".
-}
module Test.SteerSpec (
    steerSpec,
    foundHidden,
    missEnvOf,
    steerTarget,
    world,
) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Discover.History (
    SearchLedger,
    emptyLedger,
    ledgerClose,
    ledgerPressure,
    ledgerRecord,
    ledgerShortcut,
 )
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Steer (constructSteer, goalTypeOf)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    NotebookEnv (..),
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Siza.Agent.DiscoverTool (runDiscoverCall)
import Test.CatalogueSim (SimWorld (..), producerPkgs, simWorldCall)
import Test.DiscoverFixtures (
    SynPkg (..),
    hitText,
    hitsOf,
    installNamesFileWith,
    stateOf,
    textField,
 )

-- Fixtures -------------------------------------------------------------------

envP :: NotebookEnv
envP = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hk0 :: HackageInfo
hk0 = HackageInfo True []

-- | A miss envelope for a query (no source held an answer).
missEnvOf :: Text -> Value
missEnvOf q =
    discoverEnvelope
        envP
        (interpret envP q)
        8
        [okAnswer "session" [], okAnswer "hoogle" []]
        hk0

-- | A found hidden-package answer: a cabal-line fact, NOT call-ready (no sig).
foundHidden :: Value
foundHidden =
    discoverEnvelope
        envP
        (interpret envP "cumulus")
        8
        [okAnswer "session" [hiddenHit]]
        (HackageInfo True ["cumulus"])
  where
    hiddenHit =
        (mkHit "bars" "Cumulus.Plot" "cumulus")
            { dhInstall = InstHidden
            , dhCabal = Just "-- cabal: build-depends: cumulus"
            }

-- | The scripted literal-minded ledger walk: shortcut else record, in order.
scriptLedger :: [(Text, Value)] -> (SearchLedger, [Value])
scriptLedger = foldl step (emptyLedger, [])
  where
    step (led, outs) (q, v) = case ledgerShortcut led q of
        Just out -> (led, outs ++ [out])
        Nothing ->
            let (led2, out) = ledgerRecord q v led
             in (led2, outs ++ [out])

adviceOf :: Value -> Text
adviceOf v = textField "next" v <> " " <> textField "summary" v

-- | Parse the steering's named type out of an advice text (literal caller).
steerTarget :: Text -> Maybe Text
steerTarget c = case T.breakOn marker c of
    (_, rest)
        | T.null rest -> Nothing
        | otherwise ->
            let ty = T.takeWhile (/= ',') (T.drop (T.length marker) rest)
             in if T.null ty then Nothing else Just ty
  where
    marker = "For a value of type "

steered :: Value -> Bool
steered v = "mode=\"construct\"" `T.isInfixOf` adviceOf v

-- | The producer catalogue world (session sees the exposed packages).
world :: SimWorld
world = SimWorld [p | p <- pkgs, not (spHidden p)] pkgs
  where
    pkgs = producerPkgs ("plume", "framing", "styling")

runWorldConstruct :: Text -> IO Value
runWorldConstruct ty = do
    out <-
        runDiscoverCall
            True
            (simWorldCall world)
            ty
            (object ["mode" .= ("construct" :: Text)])
    pure $ case out of
        ToolOk v -> v
        ToolErr v -> v

steerSpec :: Spec
steerSpec = describe "construct-facet steering (R6-T3: R5.6/R5.7/R5.9)" $ do
    shapeSpec
    ladderSpec
    followableSpec
    pressureSpec

-- Shape classification (never a library name) --------------------------------

shapeSpec :: Spec
shapeSpec = describe "goalTypeOf classifies by name shape alone" $ do
    it "producer-prefixed and bare-type names classify to their goal type"
        $ forM_
            [ ("defaultPlot", "Plot")
            , ("mkPlot", "Plot")
            , ("mkStyle", "Style")
            , ("makeChart", "Chart")
            , ("newFrame", "Frame")
            , ("emptyFrame", "Frame")
            , ("initState", "State")
            , ("createWidget", "Widget")
            , ("LegendPos", "LegendPos")
            , ("Style", "Style")
            ]
        $ \(n, ty) -> (n, goalTypeOf n) `shouldBe` (n, Just ty)
    it "value, qualified, prose and bare-prefix shapes never classify"
        $ forM_
            [ "col"
            , "bars"
            , "D.col"
            , "Granite.Svg"
            , "default"
            , "mk"
            , "value of type Plot"
            , ""
            , "foldl'"
            , "colX"
            , "default_plot"
            ]
        $ \n -> (n, goalTypeOf n) `shouldBe` (n, Nothing)
    it "the steering text is a function of the goal type, never a library" $ do
        constructSteer "mkStyle" `shouldBe` constructSteer "makeStyle"
        case constructSteer "defaultPlot" of
            Nothing -> expectationFailure "defaultPlot must steer"
            Just s ->
                forM_ ["plume", "framing", "styling", "granite", "dataframe"] $ \lib ->
                    (lib, lib `T.isInfixOf` T.toLower s) `shouldBe` (lib, False)

-- Steered by miss 2 through the ladder ---------------------------------------

ladderSpec :: Spec
ladderSpec = describe "a value-of-type miss cluster is steered by miss 2" $ do
    it "the defaultPlot class: miss 1 is unsteered, miss 2 names the facet" $ do
        let (_, outs) =
                scriptLedger
                    [("newPlot", missEnvOf "newPlot"), ("`newPlot`", missEnvOf "`newPlot`")]
        steered (head outs) `shouldBe` False
        steered (outs !! 1) `shouldBe` True
        steerTarget (adviceOf (outs !! 1)) `shouldBe` Just "Plot"
    it "the LegendPos class (bare type name) is steered by miss 2 too" $ do
        let (_, outs) =
                scriptLedger
                    [ ("LegendPos", missEnvOf "LegendPos")
                    , ("`LegendPos`", missEnvOf "`LegendPos`")
                    ]
        steered (outs !! 1) `shouldBe` True
        steerTarget (adviceOf (outs !! 1)) `shouldBe` Just "LegendPos"
    it "a value-shaped cluster (col) is never steered, at any rung" $ do
        let (_, outs) =
                scriptLedger
                    [ ("col", missEnvOf "col")
                    , ("`col`", missEnvOf "`col`")
                    , ("col ", missEnvOf "col ")
                    ]
        forM_ outs $ \o -> steered o `shouldBe` False
    it "R5.7: a post-close miss is never steered back into searching" $ do
        let closed = ledgerClose emptyLedger
            (_, out) = ledgerRecord "newChart" (missEnvOf "newChart") closed
            advice = T.toLower (adviceOf out)
        steered out `shouldBe` False
        forM_ ["call discover", "retry", "different shape", "search again"] $ \p ->
            (p, p `T.isInfixOf` advice) `shouldBe` (p, False)

-- Followability against the catalogue ----------------------------------------

followableSpec :: Spec
followableSpec = describe "the steering text is followable (R4.4/R5.9)" $ do
    it "following the newPlot steer verbatim finds ready-made producers" $ do
        installNamesFileWith ["plume", "framing", "styling"]
        let (_, outs) =
                scriptLedger
                    [("newPlot", missEnvOf "newPlot"), ("`newPlot`", missEnvOf "`newPlot`")]
        case steerTarget (adviceOf (outs !! 1)) of
            Nothing -> expectationFailure "no steer target named"
            Just ty -> do
                v <- runWorldConstruct ty
                stateOf v `shouldBe` "found"
                map (hitText "name") (take 3 (hitsOf v))
                    `shouldSatisfy` elem "defaultPlot"
    it "following a bare-type steer (Style) reaches the hoogle-only producer" $ do
        installNamesFileWith ["plume", "framing", "styling"]
        v <- runWorldConstruct "Style"
        stateOf v `shouldBe` "found"
        map (hitText "name") (take 3 (hitsOf v)) `shouldSatisfy` elem "mkStyle"

-- Budget-pressure floor on the miss ladder (R5.6) ----------------------------

pressureSpec :: Spec
pressureSpec = describe "the miss ladder's budget-pressure floor" $ do
    let withFact = fst (ledgerRecord "cumulus" foundHidden emptyLedger)
    it "under no pressure a fresh cluster's first miss is rung 1" $ do
        let (_, out) = ledgerRecord "moonbeam" (missEnvOf "moonbeam") withFact
        adviceOf out `shouldSatisfy` (not . T.isInfixOf "Already held")
    it "at mid-budget pressure a fresh cluster's FIRST miss carries held facts" $ do
        let led = ledgerPressure 2 withFact
            (_, out) = ledgerRecord "moonbeam" (missEnvOf "moonbeam") led
        adviceOf out `shouldSatisfy` T.isInfixOf "Already held"
        adviceOf out `shouldSatisfy` T.isInfixOf "build-depends: cumulus"
    it "at floor pressure a fresh cluster's FIRST miss is act-or-blocker" $ do
        let led = ledgerPressure 3 withFact
            (_, out) = ledgerRecord "moonbeam" (missEnvOf "moonbeam") led
        adviceOf out `shouldSatisfy` T.isInfixOf "Act on what is held"
        steered out `shouldBe` False
