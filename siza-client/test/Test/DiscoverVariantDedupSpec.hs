{-# LANGUAGE OverloadedStrings #-}

{- | R6-T1 / R3.2 / R3.1: same-package name-variant dedup in the rank path,
pinned against the run-20260720-130012 bars-at-limit-5 defect over a
synthetic catalogue at EVERY limit >= 1, with conservation re-asserted.
-}
module Test.DiscoverVariantDedupSpec (discoverVariantDedupSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    Interpreted (..),
    MatchKind (..),
    NotebookEnv (..),
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (field, hitText, hitsOf)

env0 :: NotebookEnv
env0 = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hk0 :: HackageInfo
hk0 = HackageInfo True []

intField :: Text -> Value -> Int
intField k v = case field k v of
    Just (Number n) -> round n
    _ -> -1

{- | The run-130012 shape under a synthetic package: one exact hit with a
signature, three exact signatureless package\/name-variant stubs, and two
semantic hits — the one with the signature ranked below the installed one.
-}
run130012Hits :: [DHit]
run130012Hits =
    [ (mkHit "Chart" "Zephyr.Spec" "zephyr")
        { dhType = "DataFrame -> [Layer] -> Chart"
        , dhInstall = InstHidden
        }
    , (mkHit "zephyr" "unknown" "zephyr"){dhInstall = InstHidden}
    , (mkHit "Zephyr" "Zephyr" "zephyr"){dhInstall = InstHidden}
    , (mkHit "zephyr" "Zephyr" "zephyr"){dhInstall = InstHidden}
    , (mkHit "lineChart" "Sabela.Notebook" "sabela-notebook")
        { dhType = "Canvas -> [(Double, Double)] -> Picture"
        , dhKind = MkSemantic
        , dhInstall = InstInstalled
        }
    , (mkHit "gustBars" "Zephyr.Svg" "zephyr")
        { dhType = "[(Text, Double)] -> Plot -> Text"
        , dhKind = MkSemantic
        , dhInstall = InstHidden
        }
    ]

proseInterp :: Interpreted
proseInterp = interpret env0 "bar chart zephyr"

runMerge :: Int -> [DHit] -> Value
runMerge limit hits =
    discoverEnvelope env0 proseInterp limit [okAnswer "hoogle" hits] hk0

{- | Same-package name-variant stub: signatureless, name is a case\/hyphen
variant of its own package.
-}
isVariantStubRow :: Value -> Bool
isVariantStubRow h =
    T.null (hitText "type" h)
        && norm (hitText "name" h) == norm (hitText "package" h)
  where
    norm = T.toLower . T.filter (/= '-')

discoverVariantDedupSpec :: Spec
discoverVariantDedupSpec =
    describe "same-package name-variant dedup (R6-T1, R3.2/R3.1)" $ do
        it "the bars-at-limit-5 class: the signature hit is shown at limit 5" $ do
            let v = runMerge 5 run130012Hits
            map (hitText "name") (hitsOf v)
                `shouldSatisfy` elem "gustBars"
        it "at every limit >= 1: at most ONE variant row per package shown" $
            forM_ [1 .. 8 :: Int] $ \limit -> do
                let v = runMerge limit run130012Hits
                    variants = filter isVariantStubRow (hitsOf v)
                length variants `shouldSatisfy` (<= 1)
        it "at every limit: a signature hit is never displaced by variant rows" $
            forM_ [1 .. 8 :: Int] $ \limit -> do
                let v = runMerge limit run130012Hits
                    shown = hitsOf v
                    sigOmitted =
                        intField "omitted" v > 0
                            && "gustBars" `notElem` map (hitText "name") shown
                    variantCount = length (filter isVariantStubRow shown)
                (limit, sigOmitted && variantCount > 1) `shouldBe` (limit, False)
        it "conservation: shown+omitted == total at every limit" $
            forM_ [1 .. 8 :: Int] $ \limit -> do
                let v = runMerge limit run130012Hits
                intField "shown" v `shouldBe` length (hitsOf v)
                intField "shown" v + intField "omitted" v
                    `shouldBe` intField "total" v
        it "the surviving variant row keeps install state and the cabal line" $ do
            let v = runMerge 8 run130012Hits
                variants = filter isVariantStubRow (hitsOf v)
            map (hitText "install") variants `shouldBe` ["hidden"]
            map (hitText "cabal") variants
                `shouldBe` ["-- cabal: build-depends: zephyr"]
        it "a real function named like its package is never fused away" $ do
            let hits =
                    [ (mkHit "stratus" "Stratus.Core" "stratus")
                        { dhType = "Int -> Int"
                        , dhInstall = InstInstalled
                        }
                    , (mkHit "stratus" "unknown" "stratus")
                        { dhInstall = InstInstalled
                        }
                    , (mkHit "Stratus" "Stratus" "stratus")
                        { dhInstall = InstInstalled
                        }
                    ]
                v = runMerge 8 hits
            filter (not . T.null . hitText "type") (hitsOf v)
                `shouldSatisfy` ((== 1) . length)
            length (filter isVariantStubRow (hitsOf v)) `shouldBe` 1
        it "variants of DIFFERENT packages never fuse with each other" $ do
            let hits =
                    [ (mkHit "zephyr" "Zephyr" "zephyr"){dhInstall = InstHidden}
                    , (mkHit "stratus" "Stratus" "stratus")
                        { dhInstall = InstHidden
                        }
                    ]
                v = runMerge 8 hits
            length (hitsOf v) `shouldBe` 2
        it "the property holds over a generated multi-package catalogue" $
            forM_ ["gale", "sirocco", "mistral-lite"] $ \pkg -> do
                let modName = T.toTitle (T.filter (/= '-') pkg)
                    hits =
                        [ (mkHit pkg "unknown" pkg){dhInstall = InstHidden}
                        , (mkHit modName modName pkg){dhInstall = InstHidden}
                        , (mkHit pkg modName pkg){dhInstall = InstHidden}
                        , (mkHit "draw" (modName <> ".Svg") pkg)
                            { dhType = "[(Text, Double)] -> Text"
                            , dhKind = MkSemantic
                            , dhInstall = InstHidden
                            }
                        ]
                forM_ [1 .. 6 :: Int] $ \limit -> do
                    let v = runMerge limit hits
                    length (filter isVariantStubRow (hitsOf v))
                        `shouldSatisfy` (<= 1)
                    intField "shown" v + intField "omitted" v
                        `shouldBe` intField "total" v
