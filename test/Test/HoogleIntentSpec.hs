{-# LANGUAGE OverloadedStrings #-}

{- | The action-verb classifier (docs/discover/HANDOFF.md "exact-name-first
is intent-blind"): table-driven over the live-failure corpus plus one
request per action class.
-}
module Test.HoogleIntentSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Sabela.AI.HoogleIntent (
    ActionClass (..),
    actionNeedQueries,
    classifyAction,
    intentQueries,
 )

-- | (request, expected class, a query substring 'intentQueries' must include).
corpus :: [(Text, Maybe ActionClass, Maybe Text)]
corpus =
    [ ("plot a sine wave", Just RenderAction, Just "chart library")
    , ("animate it", Just RenderAction, Just "chart library")
    , ("use hgg to create a scatter plot", Just RenderAction, Just "chart library")
    , ("load housing.csv", Just LoadAction, Just "data import")
    , ("read the parquet file", Just LoadAction, Just "data import")
    , ("compute the average price", Just ComputeAction, Just "aggregation function")
    , ("aggregate sales by region", Just ComputeAction, Just "aggregation function")
    ,
        ( "install the plotting library"
        , Just InstallAction
        , Just "package installation"
        )
    , ("what is a Monad", Nothing, Nothing)
    , ("divvy", Nothing, Nothing)
    ]

renderCorpus :: [Text]
renderCorpus = [r | (r, Just RenderAction, _) <- corpus]

spec :: Spec
spec = describe "HoogleIntent action classifier" $ do
    describe "classifyAction" $
        mapM_ caseSpec corpus

    describe "intentQueries" $ do
        it "emits the class's need queries, not the bare object noun" $
            mapM_ needSpec corpus

        it "the three live-failure specimens verbatim classify as render" $ do
            classifyAction "plot a sine wave" `shouldBe` Just RenderAction
            classifyAction "animate it" `shouldBe` Just RenderAction
            classifyAction "use hgg to create a scatter plot" `shouldBe` Just RenderAction

        it
            "every render-class request emits an action-shaped query, never just the object noun alone"
            $ mapM_
                ( \req ->
                    intentQueries req
                        `shouldSatisfy` any (`elem` actionNeedQueries RenderAction)
                )
                renderCorpus

        it "an unclassifiable prose request emits no action-need queries" $ do
            intentQueries "what is a Monad" `shouldBe` []
            intentQueries "divvy" `shouldBe` []

        it "an earlier verb wins when a request names more than one action" $
            -- "install" precedes the render-shaped "plotting" token.
            classifyAction "install the plotting library" `shouldBe` Just InstallAction
  where
    caseSpec (req, expected, _) =
        it ("classifies " <> show req) $
            classifyAction req `shouldBe` expected

    needSpec (_, Nothing, _) = pure ()
    needSpec (req, Just _, Just needle) =
        intentQueries req `shouldSatisfy` (needle `elem`)
    needSpec (req, Just cls, Nothing) =
        intentQueries req `shouldBe` actionNeedQueries cls
