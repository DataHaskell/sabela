{-# LANGUAGE OverloadedStrings #-}

{- | Ranking generality (R7-T2, search-api.md section 7): imported packages
lead (R4.5); signature plainness among same-name exact hits over a grid;
exact-first preserved; rankings invariant under library-name substitution.
-}
module Test.DiscoverRankPlainSpec (discoverRankPlainSpec) where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    MatchKind (..),
    NotebookEnv (..),
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (
    SynPkg (..),
    catalogueExports,
    hitText,
    hitsOf,
    installNamesFile,
    installNamesFileWith,
    runCatArgs,
    runCatArgsIn,
    stateOf,
    synHackageNames,
 )

env0 :: NotebookEnv
env0 = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hk0 :: HackageInfo
hk0 = HackageInfo True []

{- | A universe where the notebook-imported package (zephyr, via Zephyr.Core)
exports the queried name from a NON-imported sibling module, against an
equally-installed competitor in another package.
-}
importedBandPkgs :: [SynPkg]
importedBandPkgs =
    [ SynPkg
        "zephyr"
        "1.2.0"
        False
        [ ("Zephyr.Core", [("gust", "Int -> Wind")])
        , ("Zephyr.More", [("strum", "Int -> Strum")])
        ]
    , SynPkg "brill" "3.0.0" False [("Brill.Main", [("strum", "Int -> Strum")])]
    ]

-- | A generated signature with @c@ constraint atoms and @t@ type-level args.
sigWith :: Int -> Int -> Text
sigWith c t = constraints <> "Text -> Expr a" <> tyArgs
  where
    constraints
        | c <= 0 = ""
        | otherwise =
            "("
                <> T.intercalate ", " ["C" <> tShow i <> " a" | i <- [1 .. c]]
                <> ") => "
    tyArgs = T.concat [" @S" <> tShow i | i <- [1 .. t]]
    tShow = T.pack . show

-- | The plainness grid: every strictly-plainer/heavier same-name exact pair.
plainPairs :: [((Int, Int), (Int, Int))]
plainPairs =
    [ (a, b)
    | a <- grid
    , b <- grid
    , a < b
    ]
  where
    grid = [(c, t) | c <- [0 .. 2], t <- [0 .. 2]]

rankedNames :: [DHit] -> [Text]
rankedNames hits =
    map (hitText "module") . hitsOf $
        discoverEnvelope env0 (interpret env0 "colx") 8 [okAnswer "hoogle" hits] hk0

discoverRankPlainSpec :: Spec
discoverRankPlainSpec = describe "ranking generality (R7-T2)" $ do
    describe "imported/in-session package band (R4.5)" $ do
        it
            "an imported package's exact hit ranks #1 even from a non-imported sibling module"
            $ do
                installNamesFileWith (synHackageNames ++ ["brill"])
                v <- runCatArgsIn importedBandPkgs "strum" (object [])
                stateOf v `shouldBe` "found"
                case hitsOf v of
                    (h : _) -> hitText "package" h `shouldBe` "zephyr"
                    [] -> expectationFailure "no hits"
        it "whole catalogue: every exact query with an imported-package hit ranks it #1" $ do
            installNamesFile
            failures <- concat <$> mapM importedFirstViolation catalogueExports
            failures `shouldBe` []

    describe "signature plainness among same-name exact hits (section 7)" $ do
        it
            "the plainer variant leads over the whole (constraints x type-level args) grid"
            $ do
                let failures =
                        [ (plainKey, heavyKey)
                        | (plainKey, heavyKey) <- plainPairs
                        , let plain =
                                (mkHit "colx" "Mod.Zz" "framex")
                                    { dhType = uncurry sigWith plainKey
                                    , dhInstall = InstInstalled
                                    }
                              heavy =
                                (mkHit "colx" "Mod.Aa" "framex")
                                    { dhType = uncurry sigWith heavyKey
                                    , dhInstall = InstInstalled
                                    }
                        , rankedNames [heavy, plain] /= ["Mod.Zz", "Mod.Aa"]
                        ]
                failures `shouldBe` []

        it "plainness never lifts a signatureless stub above a signature-carrying hit" $ do
            let stub =
                    (mkHit "colx" "Mod.Aa" "framex")
                        { dhInstall = InstInstalled
                        }
                sig =
                    (mkHit "colx" "Mod.Zz" "framex")
                        { dhType = sigWith 2 1
                        , dhInstall = InstInstalled
                        }
            rankedNames [stub, sig] `shouldBe` ["Mod.Zz", "Mod.Aa"]

        it "exact-first is strictly preserved under the new sub-keys" $ do
            let exactHeavy =
                    (mkHit "colx" "Mod.Aa" "framex")
                        { dhType = sigWith 3 2
                        , dhInstall = InstAbsentKnown
                        }
                weakPlain =
                    (mkHit "colxLike" "Mod.Zz" "framex")
                        { dhType = sigWith 0 0
                        , dhKind = MkSubstring
                        , dhInstall = InstInstalled
                        }
            rankedNames [weakPlain, exactHeavy] `shouldBe` ["Mod.Aa", "Mod.Zz"]

    describe "library-name substitution invariance" $
        it "ranking decisions are byte-identical under a package-name substitution" $ do
            let namesA = ("garneta", "citrine", "beryl")
                namesB = ("opalite", "jaspers", "topaz")
            installNamesFileWith
                (synHackageNames ++ tripleList namesA ++ tripleList namesB)
            va <- runCatArgsIn (subsPkgs namesA) "colx" (object [])
            vb <- runCatArgsIn (subsPkgs namesB) "colx" (object [])
            substitute (zip (tripleList namesA) (tripleList namesB)) (jsonText va)
                `shouldBe` jsonText vb

{- | The notebook imports Zephyr.Core (fixture cell 0): whenever a query's
answer holds any zephyr hit, that hit must lead (R4.5).
-}
importedFirstViolation :: Text -> IO [Text]
importedFirstViolation n = do
    v <- runCatArgs n (object [])
    let pkgs = map (hitText "package") (hitsOf v)
    pure
        [ n <> ": imported-package hit not #1 in " <> T.pack (show pkgs)
        | "zephyr" `elem` pkgs
        , take 1 pkgs /= ["zephyr"]
        ]

{- | Structure held constant, names substitutable: same modules, same
signatures, only the package identities differ (equal lengths pairwise).
-}
subsPkgs :: (Text, Text, Text) -> [SynPkg]
subsPkgs (pA, pB, pC) =
    [ SynPkg pA "1.0.0" False [("Gem.One", [("colx", "Text -> Expr a")])]
    , SynPkg pB "1.0.0" False [("Gem.Two", [("colx", "(C1 a) => Text -> Expr a")])]
    , SynPkg pC "1.0.0" True [("Gem.Sty", [("colx", "Int -> Style")])]
    ]

tripleList :: (Text, Text, Text) -> [Text]
tripleList (a, b, c) = [a, b, c]

jsonText :: Value -> Text
jsonText = TE.decodeUtf8 . LBS.toStrict . encode

substitute :: [(Text, Text)] -> Text -> Text
substitute pairs t = foldl (\acc (a, b) -> T.replace a b acc) t pairs
