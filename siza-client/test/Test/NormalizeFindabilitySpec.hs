{-# LANGUAGE OverloadedStrings #-}

{- | R7.6 cross-check (R6-T1): every name\/key the normalizer proposes is
findable by discover on the same catalogue — one catalogue, two consumers —
so heal proposals and search advice can never diverge.
-}
module Test.NormalizeFindabilitySpec (normalizeFindabilitySpec) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.NormalizeProposals (
    bindingKeywords,
    foldCabalComments,
    proposedRename,
    renameKeywordBindings,
 )
import Siza.Agent.Discover.Classify (envAnswer)
import Siza.Agent.Discover.Guidance (cabalLine)
import Siza.Agent.Discover.Interpret (envFromCells, interpret, stripDecoration)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Types (HackageInfo (..))
import Test.DiscoverFixtures (hitText, hitsOf, stateOf)

hk0 :: HackageInfo
hk0 = HackageInfo True []

-- | The confusable-hyphen and key-variant grid (mirrors the sabela-side spec).
corruptions :: [Text]
corruptions =
    [ "-- cabal: " <> T.map swapFor key <> ": "
    | key <-
        [ "build-depends"
        , "build-depend"
        , "build-dependency"
        , "build-dependencies"
        , "build-deps"
        ]
    ]
  where
    swapFor '-' = '\x2011'
    swapFor c = c

normalizeFindabilitySpec :: Spec
normalizeFindabilitySpec =
    describe "R7.6: normalizer proposals are discover-findable (R6-T1)" $ do
        it "every keyword-rename proposal resolves via the environment layer" $
            forM_ bindingKeywords $ \kw -> do
                let proposal = proposedRename kw
                    (healed, _) =
                        renameKeywordBindings (kw <> " = 5")
                    env = envFromCells [(healed, [proposal])]
                    interp = interpret env proposal
                    v =
                        discoverEnvelope
                            env
                            interp
                            8
                            [envAnswer env interp]
                            hk0
                stateOf v `shouldBe` "found"
                map (hitText "name") (take 1 (hitsOf v))
                    `shouldBe` [proposal]
                map (hitText "install") (take 1 (hitsOf v))
                    `shouldBe` ["notebook"]
        it "a primed identifier survives interpretation (the foldl' class)" $ do
            stripDecoration "resample'" `shouldBe` "resample'"
            stripDecoration "`resample'`" `shouldBe` "resample'"
            stripDecoration "`resample''" `shouldBe` "resample'"
        it "a folded cabal key is byte-identical to discover's own cabal line" $
            forM_ ["zephyr", "stratus", "cumulus"] $ \pkg ->
                forM_ corruptions $ \broken ->
                    fst (foldCabalComments (broken <> pkg))
                        `shouldBe` cabalLine pkg
