{-# LANGUAGE OverloadedStrings #-}

{- | Mode/filter-agnostic truthfulness (search-api.md 3.3, R1.1/R3.7): a name
resolvable under ANY (mode, filter) tuple is never not_found under another
— the full-grid false-denial ledger, determinism, and the topMonth fixture.
-}
module Test.DiscoverModeGridSpec (discoverModeGridSpec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Test.DiscoverFixtures

-- | The three renderings of the one index (section 3.3).
gridModes :: [Text]
gridModes = ["search", "inventory", "construct"]

{- | The (name, module filter, package filter) rows of the grid: exports under
their defining module and package, modules and packages under themselves,
plus notebook binding, alias spelling and builtin (filter-less rows).
-}
gridRows :: [(Text, Maybe Text, Maybe Text)]
gridRows =
    [ (n, Just m, Just (spName p))
    | p <- synHoogle
    , (m, es) <- spModules p
    , (n, _) <- es
    ]
        ++ [(m, Just m, Just (spName p)) | p <- synHoogle, (m, _) <- spModules p]
        ++ [(spName p, Nothing, Just (spName p)) | p <- synHoogle]
        ++ [(n, Nothing, Nothing) | n <- ["gustTotal", "Z.gust", "displayHtml"]]

-- | Every (query, mode, filter) tuple of one grid row: no filter, module, package.
rowTuples :: (Text, Maybe Text, Maybe Text) -> [(Text, Text, Value)]
rowTuples (n, m, p) =
    [ (n, mode, args)
    | mode <- gridModes
    , args <-
        object ["mode" .= mode]
            : [object ["mode" .= mode, "module" .= f] | Just f <- [m]]
            ++ [object ["mode" .= mode, "package" .= f] | Just f <- [p]]
    ]

gridTuples :: [(Text, Text, Value)]
gridTuples = concatMap rowTuples gridRows

-- | One full evaluation of the grid, independently constructed per call.
evalGrid :: IO [((Text, Text, Value), Value)]
evalGrid =
    mapM (\t@(q, _, args) -> (,) t <$> runCatArgs q args) gridTuples

discoverModeGridSpec :: Spec
discoverModeGridSpec =
    beforeAll_ installNamesFile $
        describe "mode/filter-agnostic truthfulness (section 3.3)" $ do
            it "false-denial ledger over the full (name x mode x filter) grid is empty" $ do
                answered <- evalGrid
                let resolvable q =
                        or [stateOf v == "found" | ((q', _, _), v) <- answered, q' == q]
                    denials =
                        [ (q, mode, args)
                        | ((q, mode, args), v) <- answered
                        , stateOf v == "not_found"
                        , resolvable q
                        ]
                denials `shouldBe` []

            it "is deterministic across two independently constructed evaluations" $ do
                a <- evalGrid
                b <- evalGrid
                map snd a `shouldBe` map snd b

            describe "the topMonth-off turn-14/16/18 shapes (red-then-green fixture)" $ do
                it
                    "t14: the re-exported accessor under module=<re-exporting module> answers found"
                    $ do
                        v <-
                            runFrame
                                "colList"
                                (object ["module" .= ("Frame" :: Text)])
                        stateOf v `shouldBe` "found"
                it
                    "t16: the plain accessor resolves under mode=construct (redirect, never not_found)"
                    $ do
                        v <-
                            runFrame
                                "colGet"
                                (object ["mode" .= ("construct" :: Text)])
                        stateOf v `shouldBe` "found"
                it
                    "t18: the frame module itself answers found under mode=inventory + its own filter"
                    $ do
                        v <-
                            runFrame
                                "Frame"
                                ( object
                                    [ "mode" .= ("inventory" :: Text)
                                    , "module" .= ("Frame" :: Text)
                                    ]
                                )
                        stateOf v `shouldBe` "found"

            it
                "an imported-package miss answers write-and-observe first (section 8, round 7)"
                $ do
                    v <- runCatArgs "Z.gustNope" (object [])
                    stateOf v `shouldBe` "not_found"
                    T.toLower (textField "next" v)
                        `shouldSatisfy` ("write the cell" `T.isInfixOf`)

{- | The dataframe-shaped synthetic package (structure only, no bench library
names): a defining module, a re-exporting module, and a constrained typed twin
— the topMonth-off attribution/mode shapes.
-}
framePkgs :: [SynPkg]
framePkgs =
    [ SynPkg
        "frameio"
        "2.1.0"
        False
        [ ("Frame", frameApi)
        , ("Frame.Ops", frameApi)
        ,
            ( "Frame.Typed"
            ,
                [
                    ( "colGet"
                    , "(KnownSymbol s, Columnable a) => TExpr s -> Typed a"
                    )
                ]
            )
        ]
    ]
  where
    frameApi =
        [ ("colGet", "Text -> Expr a")
        , ("colList", "Columnable a => Expr a -> Frame -> [a]")
        ]

runFrame :: Text -> Value -> IO Value
runFrame = runCatArgsIn framePkgs
