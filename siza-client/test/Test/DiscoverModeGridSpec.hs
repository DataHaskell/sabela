{-# LANGUAGE OverloadedStrings #-}

module Test.DiscoverModeGridSpec (discoverModeGridSpec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Test.DiscoverFixtures

gridModes :: [Text]
gridModes = ["search", "inventory", "construct"]

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
                "an imported-package miss answers try-and-observe first (section 8, round 7)"
                $ do
                    v <- runCatArgs "Z.gustNope" (object [])
                    stateOf v `shouldBe` "not_found"
                    T.toLower (textField "next" v)
                        `shouldSatisfy` ("try" `T.isInfixOf`)

            it "never steers a miss at a speculative cell write" $ do
                v <- runCatArgs "Z.gustNope" (object [])
                let next = T.toLower (textField "next" v)
                next `shouldNotSatisfy` ("write the cell" `T.isInfixOf`)
                next `shouldNotSatisfy` ("red cell" `T.isInfixOf`)

            {- The route to what IS there for an imported module is the
            compiler, not a package listing: inventory states no signature. -}
            it "points a miss at what would list what IS there" $ do
                v <- runCatArgs "Z.gustNope" (object [])
                let next = T.toLower (textField "next" v)
                next `shouldSatisfy` ("exports" `T.isInfixOf`)
                next `shouldNotSatisfy` ("inventory" `T.isInfixOf`)

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
