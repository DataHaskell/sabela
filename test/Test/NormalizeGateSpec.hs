{-# LANGUAGE OverloadedStrings #-}

{- | R5-T3: ONE acceptance law over every machine rewrite (search-api.md
section 9.3). The pre-run normalizer is a candidate GENERATOR; its output is
kept iff the same 'acceptRepair' law the repair cascade uses admits it at the
parse stage — else the submission is preserved byte-identically and the
attempt is disclosed as attempted-and-reverted.
-}
module Test.NormalizeGateSpec (spec) where

import Control.Monad (forM_, when)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.NormalizeGate (
    acceptsRewrite,
    gatedNormalizeInsert,
    gatedRewrite,
    parseHealth,
 )
import Sabela.AI.RepairDispatch (acceptRepair)
import Sabela.AI.SelfHeal (selfHealNote)
import Sabela.Model (CellType (..))
import Sabela.Parse.Normalize (normalizeInsert)

-- | The run-085948 regression: a stripped let with its dangling @in@.
danglingIn :: Text
danglingIn = "let x = 5\nin x * 2"

{- | Generated cell sources: let-forms x keyword binding names x well-formed
x unparseable-before-AND-after. No library-specific case anywhere.
-}
sourceGrid :: [Text]
sourceGrid =
    [ "let xs = [1,2,3]"
    , "let a = 1\nlet b = 2"
    , "let x = 1\n    y = 2\nz = x + y"
    , danglingIn
    , "let x = 5\n    in x * 2"
    , "let data = 5"
    , "let type = 3"
    , "let newtype = 9"
    , "x = 1"
    , "x = 1\ny = 2"
    , "main = do\n  print 1"
    , "main :: IO ()\nmain = print 2"
    , "-- cabal: build-depends: zephyr\nimport Zephyr.Core\nlet w = gust 3"
    , "v <- readLn\nlet k = v + 1"
    , "let (( = )\nin ((("
    , "(((("
    , ""
    ]

spec :: Spec
spec = describe "one acceptance law over every rewrite (section 9.3)" $ do
    describe "parseability monotonicity over the generated grid (R7.5)" $
        forM_ sourceGrid $ \src ->
            it ("never asserts a worse-parsing source: " <> show (T.take 30 src)) $ do
                let (_, src', notes) = gatedNormalizeInsert CodeCell src
                if src' == src
                    then do
                        -- Preserved byte-identically; an attempted rewrite is
                        -- disclosed as reverted, never silently dropped.
                        let (_, cand, _) = normalizeInsert CodeCell src
                        when (cand /= src) $
                            notes
                                `shouldSatisfy` any
                                    ( \n ->
                                        "attempted" `T.isInfixOf` n
                                            && "reverted" `T.isInfixOf` n
                                    )
                    else acceptsRewrite src src' `shouldBe` True

    describe "the dangling-in fixture (run-085948) is reverted and disclosed" $ do
        it "gatedNormalizeInsert preserves the submission byte-identically" $ do
            let (_, src', notes) = gatedNormalizeInsert CodeCell danglingIn
            src' `shouldBe` danglingIn
            notes `shouldSatisfy` any (T.isInfixOf "reverted")
        it "the ungated generator would have shipped the parse error (red pin)" $
            -- The generator's raw output IS broken; only the gate saves it.
            normalizeInsert CodeCell danglingIn
                `shouldSatisfy` \(_, cand, _) -> cand /= danglingIn
        it "the replace-path rewrite is gated identically" $
            gatedRewrite danglingIn `shouldBe` danglingIn
        it "a well-formed let rewrite is still kept on both paths" $ do
            let (_, src', _) = gatedNormalizeInsert CodeCell "let xs = [1,2,3]"
            src' `shouldBe` "xs = [1,2,3]"
            gatedRewrite "let xs = [1,2,3]" `shouldBe` "xs = [1,2,3]"

    describe "acceptance-law universality: the ONE cascade law, no second rule" $ do
        it "the gate's verdict IS acceptRepair over parse healths (whole grid)" $
            forM_ sourceGrid $ \src -> do
                let (_, cand, _) = normalizeInsert CodeCell src
                    lawVerdict =
                        acceptRepair
                            Set.empty
                            [("candidate", parseHealth src)]
                            [("candidate", parseHealth cand)]
                            "candidate"
                    (_, kept, _) = gatedNormalizeInsert CodeCell src
                (src, kept == cand || cand == src)
                    `shouldBe` (src, lawVerdict || cand == src)
        it "library-name substitution never changes the verdict" $ do
            let template lib m =
                    "-- cabal: build-depends: "
                        <> lib
                        <> "\nimport "
                        <> m
                        <> "\nlet w = f 3\nin w"
                verdictFor lib m =
                    let s = template lib m
                        (_, src', _) = gatedNormalizeInsert CodeCell s
                     in src' == s
                spellings =
                    [ ("granite", "Granite.Svg")
                    , ("zephyr", "Zephyr.Core")
                    , ("stratus", "Stratus.Air")
                    ] ::
                        [(Text, Text)]
            map (uncurry verdictFor) spellings
                `shouldSatisfy` (\vs -> length (Set.toList (Set.fromList vs)) == 1)

    describe "note honesty (R7.1/R7.2/R7.4)" $
        forM_ sourceGrid $ \src ->
            it ("notes match the outcome: " <> show (T.take 30 src)) $ do
                let (_, src', notes) = gatedNormalizeInsert CodeCell src
                    (_, cand, _) = normalizeInsert CodeCell src
                if src' /= src
                    then do
                        -- Kept: the notes name the rewrite; the heal note
                        -- attaches the ACTUAL post-edit source.
                        notes `shouldSatisfy` (not . null)
                        case selfHealNote src src' of
                            Nothing -> expectationFailure "kept rewrite lost its note"
                            Just _ -> pure ()
                    else do
                        -- Reverted or untouched: no self-heal note can exist
                        -- (pre == post), so no note attaches to a failed op.
                        selfHealNote src src' `shouldBe` Nothing
                        when (cand == src) $
                            notes
                                `shouldSatisfy` (not . any (T.isInfixOf "reverted"))
