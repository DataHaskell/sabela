{-# LANGUAGE OverloadedStrings #-}

module Test.SteerSpec (
    steerSpec,
    foundHidden,
    missEnvOf,
    world,
) where

import Control.Monad (forM_)
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.History (
    SearchLedger,
    emptyLedger,
    ledgerPressure,
    ledgerRecord,
    ledgerShortcut,
 )
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Steer (goalTypeOf)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    NotebookEnv (..),
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Test.CatalogueSim (SimWorld (..), producerPkgs)
import Test.DiscoverFixtures (SynPkg (..), textField)

envP :: NotebookEnv
envP = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hk0 :: HackageInfo
hk0 = HackageInfo True []

missEnvOf :: Text -> Value
missEnvOf q =
    discoverEnvelope
        envP
        (interpret envP q)
        8
        [okAnswer "session" [], okAnswer "hoogle" []]
        hk0

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

world :: SimWorld
world = SimWorld [p | p <- pkgs, not (spHidden p)] pkgs
  where
    pkgs = producerPkgs ("plume", "framing", "styling")

steerSpec :: Spec
steerSpec = describe "goal shape classification and the miss record" $ do
    shapeSpec
    recordSpec

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

bannedAdvice :: [Text]
bannedAdvice =
    [ "mode=\"construct\""
    , "find_by_type"
    , "act on what is held"
    , "write the deliverable now"
    , "state the blocker"
    , "retry"
    , "different shape"
    , "search again"
    ]

recordSpec :: Spec
recordSpec = describe "a miss reports, it does not instruct" $ do
    let withFact = fst (ledgerRecord "cumulus" foundHidden emptyLedger)
        noAdvice out =
            forM_ bannedAdvice $ \p ->
                (p, p `T.isInfixOf` T.toLower (adviceOf out)) `shouldBe` (p, False)

    it "no rung of a value-of-type cluster steers" $ do
        let (_, outs) =
                scriptLedger
                    [ ("newPlot", missEnvOf "newPlot")
                    , ("`newPlot`", missEnvOf "`newPlot`")
                    , ("newPlot ", missEnvOf "newPlot ")
                    ]
        mapM_ noAdvice outs

    it "distinct concept misses are not taught to ask by type" $ do
        let (_, outs) =
                scriptLedger
                    [ ("pictures", missEnvOf "pictures")
                    , ("overlay", missEnvOf "overlay")
                    , ("color", missEnvOf "color")
                    ]
        mapM_ noAdvice outs

    it "under budget pressure the record still only reports" $ do
        let led = ledgerPressure 3 withFact
            (_, out) = ledgerRecord "moonbeam" (missEnvOf "moonbeam") led
        noAdvice out

    it "a pressured miss still carries the facts already held" $ do
        let led = ledgerPressure 2 withFact
            (_, out) = ledgerRecord "moonbeam" (missEnvOf "moonbeam") led
        adviceOf out `shouldSatisfy` T.isInfixOf "build-depends: cumulus"
