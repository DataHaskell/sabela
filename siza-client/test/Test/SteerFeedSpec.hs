{-# LANGUAGE OverloadedStrings #-}

{- | The found-but-goal-type-unsatisfied steer feed (R7-T2): a found answer
whose hits ALL fail the goal-type/name-shape check counts in the miss
cluster and fires the construct steer; the not_found path is unregressed.
-}
module Test.SteerFeedSpec (steerFeedSpec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.History (SearchLedger, emptyLedger, ledgerRecord)
import Test.DiscoverFixtures (field, stateOf, textField)

-- | A found envelope for @q@ whose hits are the given (name, type) pairs.
foundWith :: Text -> [(Text, Text)] -> Value
foundWith q hits =
    object
        [ "query" .= q
        , "state" .= ("found" :: Text)
        , "interpreted" .= object ["shape" .= ("name" :: Text)]
        , "hits"
            .= [ object
                    [ "name" .= n
                    , "type" .= t
                    , "module" .= ("Sty.Mod" :: Text)
                    , "package" .= ("stylex" :: Text)
                    , "matchKind" .= ("semantic" :: Text)
                    , "install" .= ("installed" :: Text)
                    ]
               | (n, t) <- hits
               ]
        , "total" .= length hits
        , "shown" .= length hits
        ]

missFor :: Text -> Value
missFor n =
    object
        [ "query" .= n
        , "state" .= ("not_found" :: Text)
        , "interpreted" .= object ["shape" .= ("name" :: Text)]
        , "next" .= ("No match for '" <> n <> "'." :: Text)
        , "total" .= (0 :: Int)
        ]

{- | The defaultPlotLineStyle class: generated value-of-type hunts, none of
whose found hits name the target or produce its goal type.
-}
unsatisfiedCases :: [(Text, [(Text, Text)])]
unsatisfiedCases =
    [ ("defaultPlotLineStyle", [("defaultStyle", "Style"), ("lineWidth", "Double")])
    , ("mkLegendPos", [("legendText", "Text")])
    , ("PlotArea", [("plotTitle", "Text -> Text")])
    ]

record :: Text -> Value -> SearchLedger -> (SearchLedger, Value)
record = ledgerRecord

steerFeedSpec :: Spec
steerFeedSpec = describe "found-but-unsatisfied steer feed (R7-T2)" $ do
    -- R8-T2 tightened this: an unsatisfied found walks the SAME ladder as a
    -- not_found, so the steer fires at rung 2, not on the first answer.
    it "fires the construct steer on every generated unsatisfied cluster by rung 2" $ do
        let failures =
                [ q
                | (q, hits) <- unsatisfiedCases
                , let (l1, _) = record q (foundWith q hits) emptyLedger
                      (_, out2) = record q (foundWith q hits) l1
                , not ("mode=\"construct\"" `T.isInfixOf` textField "next" out2)
                ]
        failures `shouldBe` []
    it "discloses the unsatisfied goal in-band on the FIRST wall" $ do
        let failures =
                [ q
                | (q, hits) <- unsatisfiedCases
                , let (_, out) = record q (foundWith q hits) emptyLedger
                , Just (Bool False) /= (field "goal" out >>= field "satisfied")
                ]
        failures `shouldBe` []

    it "counts unsatisfied found answers toward the miss cluster" $ do
        let q = "defaultPlotLineStyle"
            hits = [("defaultStyle", "Style")]
            (l1, _) = record q (foundWith q hits) emptyLedger
            (l2, _) = record q (foundWith q hits) l1
            (_, out3) = record q (missFor q) l2
        -- Two unsatisfied founds then a miss: the cluster escalation reaches
        -- the give-up rung, which must carry the legal cannot-help wording.
        T.toLower (textField "next" out3 <> textField "summary" out3)
            `shouldSatisfy` ("cannot help" `T.isInfixOf`)

    it "a satisfying hit (exact target name) feeds no steer" $ do
        let q = "defaultPlotLineStyle"
            (_, out) =
                record q (foundWith q [(q, "PlotLineStyle")]) emptyLedger
        textField "next" out
            `shouldNotSatisfy` ("mode=\"construct\"" `T.isInfixOf`)

    it "a goal-type-producing hit under another name feeds no steer" $ do
        let q = "defaultPlotLineStyle"
            (_, out) =
                record
                    q
                    (foundWith q [("styleOf", "Int -> PlotLineStyle")])
                    emptyLedger
        textField "next" out
            `shouldNotSatisfy` ("mode=\"construct\"" `T.isInfixOf`)

    it "the not_found steer trigger path is unregressed (rung-2 value-shape miss)" $ do
        let q = "defaultPlotLineStyle"
            (l1, _) = record q (missFor q) emptyLedger
            (_, out2) = record (q <> " ") (missFor q) l1
        stateOf out2 `shouldBe` "not_found"
        textField "next" out2
            `shouldSatisfy` ("mode=\"construct\"" `T.isInfixOf`)
