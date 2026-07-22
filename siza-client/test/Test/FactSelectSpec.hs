{-# LANGUAGE OverloadedStrings #-}

{- | R7-T4 (R3.9/R5.6/R5.7): held facts reach the nudge as a ranked selection
keyed on evidence shape, never a package block-list; monotone under
irrelevant additions and bounded in count and bytes.
-}
module Test.FactSelectSpec (factSelectSpec) where

import Control.Monad (forM_)
import Data.Aeson (object, (.=))
import Data.List (subsequences)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.Advice (harvestFacts, maxHeldFacts)
import Siza.Agent.Discover.FactSelect (
    FactContext,
    factContext,
    factsByteBudget,
    selectFacts,
 )

-- | The run-181440 revenueTotal deliverable context: goal + drafted cell.
ctx181440 :: FactContext
ctx181440 =
    factContext
        "Compute the total revenue: revenueTotal :: Double summed from sales.csv"
        ["total = D.sum (D.col \"revenue\") df"]
        ["col"]

-- | The call-ready signature fact the nudge exists to echo.
sigFact :: Text
sigFact = "`col` :: Text -> Expr a — found in DataFrame (dataframe)"

-- | The cluster-referenced actionable survivor: hidden package + cabal line.
survivorFact :: Text
survivorFact =
    "dataframe-core (hidden): -- cabal: build-depends: dataframe-core \
    \— provides `col`"

{- | The run-181440 noise rows: absent-known packages surfaced by stray
queries (toTyped on a revenue task; maxBy/max pollution on topMonth).
-}
noiseFacts :: [Text]
noiseFacts =
    [ "yesod-core (absent-known): -- cabal: build-depends: yesod-core \
      \— provides `toTyped`"
    , "hledger-web (absent-known): -- cabal: build-depends: hledger-web \
      \— provides `toTyped`"
    , "streaming (absent-known): -- cabal: build-depends: streaming \
      \— provides `maxBy`"
    , "weigh (absent-known): -- cabal: build-depends: weigh — provides `weigh`"
    , "mongoDB (absent-known): -- cabal: build-depends: mongoDB \
      \— provides `find`"
    , "criterion (absent-known): -- cabal: build-depends: criterion \
      \— provides `bench`"
    ]

relevantFacts :: [Text]
relevantFacts = [sigFact, survivorFact]

factSelectSpec :: Spec
factSelectSpec = describe "held-facts ranked selection (R7-T4)" $ do
    describe "R5.6 relevance property over generated mixed ledgers" $ do
        it "carries every relevance-passing fact and none failing it" $
            forM_ (take 40 (subsequences noiseFacts)) $ \noise ->
                forM_ [id, reverse] $ \order -> do
                    let ledger = order (interleave relevantFacts noise)
                    selectFacts ctx181440 ledger
                        `shouldMatchList` relevantFacts
        it "is monotone: adding irrelevant facts never changes the output" $
            forM_ (take 40 (subsequences noiseFacts)) $ \noise ->
                selectFacts ctx181440 (relevantFacts ++ noise)
                    `shouldBe` selectFacts ctx181440 relevantFacts
        it "keeps a context-resolved signature ahead of unrelated card facts" $ do
            let unrelated =
                    [ "`noise" <> T.pack (show i) <> "` :: Int — found in Zephyr"
                    | i <- [1 .. maxHeldFacts]
                    ]
            selectFacts ctx181440 (unrelated ++ [sigFact]) `shouldContain` [sigFact]

    describe "R3.9 bound over the grid" $ do
        it "never exceeds maxHeldFacts" $
            forM_ (take 40 (subsequences noiseFacts)) $ \noise ->
                length (selectFacts ctx181440 (relevantFacts ++ noise))
                    `shouldSatisfy` (<= maxHeldFacts)
        it "stays within the byte budget even with many relevant facts" $ do
            let many =
                    [ "`col"
                        <> n
                        <> "` :: Text -> Expr a — found in DataFrame \
                           \(dataframe)"
                    | i <- [1 .. 30 :: Int]
                    , let n = T.pack (show i)
                    ]
                wideCtx =
                    factContext
                        "goal"
                        ["x = " <> T.unwords ["D.col" <> T.pack (show i) | i <- [1 .. 30 :: Int]]]
                        []
                picked = selectFacts wideCtx many
            sum (map T.length picked) `shouldSatisfy` (<= factsByteBudget)
            length picked `shouldSatisfy` (<= maxHeldFacts)

    describe "the run-181440 fixture (red-then-green)" $ do
        it "drops every stray absent-known row; the survivor survives" $ do
            let picked = selectFacts ctx181440 (interleave relevantFacts noiseFacts)
            picked `shouldContain` [survivorFact]
            picked `shouldContain` [sigFact]
            forM_ noiseFacts $ \f -> picked `shouldSatisfy` notElem f
        it "an absent-known row the deliverable names DOES survive" $ do
            let ctx =
                    factContext
                        "benchmark it with criterion"
                        []
                        ["bench"]
                fact =
                    "criterion (absent-known): -- cabal: build-depends: \
                    \criterion — provides `bench`"
            selectFacts ctx [fact] `shouldBe` [fact]

    describe "harvest strength (R4-T2 discipline)" $ do
        it "a non-exact hit's cabal row is never harvested into held facts" $ do
            let env =
                    object
                        [ "query" .= ("maxBy" :: Text)
                        , "state" .= ("found" :: Text)
                        , "hits" .= [hit "substring"]
                        ]
            harvestFacts env `shouldBe` []
        it "an exact hit's cabal row carries its provides tag" $ do
            let env =
                    object
                        [ "query" .= ("maxBy" :: Text)
                        , "state" .= ("found" :: Text)
                        , "hits" .= [hit "exact"]
                        ]
            harvestFacts env
                `shouldSatisfy` any
                    (T.isInfixOf "streaming (absent-known)")
            harvestFacts env `shouldSatisfy` any (T.isInfixOf "provides `maxBy`")
  where
    hit kind =
        object
            [ "name" .= ("maxBy" :: Text)
            , "module" .= ("Streaming.Prelude" :: Text)
            , "package" .= ("streaming" :: Text)
            , "version" .= ("0.2.4" :: Text)
            , "install" .= ("absent-known" :: Text)
            , "matchKind" .= (kind :: Text)
            , "origin" .= ("hoogle" :: Text)
            , "cabal" .= ("-- cabal: build-depends: streaming" :: Text)
            ]
    interleave (x : xs) (y : ys) = x : y : interleave xs ys
    interleave xs ys = xs ++ ys
