{-# LANGUAGE OverloadedStrings #-}

{- | The bounded absent-known tail (R7-T2, section 7) and the in-budget card
block (section 10): at every limit at most two absent-known cross-package
rows render, counts reconcile, and the card fits inside 2,500 chars.
-}
module Test.DiscoverAbsentBoundSpec (discoverAbsentBoundSpec) where

import Data.Aeson (Value (..))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Grammar (ImportStyle (Unqualified))
import Sabela.AI.Grammar.Synth (Surface (..))
import Siza.Agent.Discover (discoverGrammarMsg)
import Siza.Agent.Discover.Envelope (envelopeCharBudget, envelopeChars)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    NotebookEnv (..),
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (field, hitText, hitsOf, textField)

env0 :: NotebookEnv
env0 = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hk0 :: HackageInfo
hk0 = HackageInfo True []

{- | The maxBy shape: one session-installed exact hit plus six absent-known
exact hits across six not-installed packages (hoogle tier).
-}
installedMaxBy :: DHit
installedMaxBy =
    (mkHit "maxBy" "Frame.Ops" "frameio")
        { dhType = "(a -> a -> Ordering) -> [a] -> a"
        , dhOrigin = "session"
        }

absentMaxBys :: [DHit]
absentMaxBys =
    [ (mkHit "maxBy" ("Absent.M" <> n) ("absent-" <> T.toLower n))
        { dhType = "(a -> a -> Ordering) -> [a] -> a"
        , dhInstall = InstAbsentKnown
        }
    | i <- [1 .. 6 :: Int]
    , let n = T.pack (show i)
    ]

envelopeAt :: Int -> Value
envelopeAt limit =
    discoverEnvelope
        env0
        (interpret env0 "maxBy")
        limit
        [okAnswer "session" [installedMaxBy], okAnswer "hoogle" absentMaxBys]
        hk0

discoverAbsentBoundSpec :: Spec
discoverAbsentBoundSpec = describe "bounded absent-known tail + card budget (R7-T2)" $ do
    describe "absent-known rows are capped at <=2 at every limit 1..8" $
        it "holds, reconciles shown+omitted==total, and installed rows lead" $ do
            let failures = concatMap violationsAt [1 .. 8]
            failures `shouldBe` []

    it "the suppressed tail is disclosed as a counted line" $ do
        let v = envelopeAt 8
        textField "narrow" v `shouldSatisfy` ("more" `T.isInfixOf`)
        textField "narrow" v `shouldSatisfy` ("not-installed" `T.isInfixOf`)

    it "every shown row still carries provenance" $ do
        let v = envelopeAt 8
        mapM_
            (\h -> hitText "install" h `shouldNotBe` "")
            (hitsOf v)

    describe "the discover card block counts INSIDE the envelope budget" $
        it "a card synthesized at the cap serialises within 2,500 chars" $ do
            let surfaces =
                    [ Surface
                        "Frame.Ops"
                        Unqualified
                        ( T.unlines
                            [ "colFn"
                                <> T.pack (show i)
                                <> " :: Text -> Expr a -> Frame -> [a]"
                            | i <- [1 .. 200 :: Int]
                            ]
                        )
                    ]
            case discoverGrammarMsg ["colFn1"] surfaces of
                [v] -> envelopeChars v `shouldSatisfy` (<= envelopeCharBudget)
                other -> expectationFailure ("expected one card, got " ++ show (length other))

-- | The R3.4/R3.5 violations of one limit's envelope.
violationsAt :: Int -> [Text]
violationsAt limit =
    capViol ++ reconcileViol ++ budgetViol
  where
    v = envelopeAt limit
    hits = hitsOf v
    absentsShown = length [h | h <- hits, hitText "install" h == "absent-known"]
    capViol =
        [ "limit " <> tShow limit <> ": " <> tShow absentsShown <> " absent-known shown"
        | absentsShown > 2
        ]
    shown = numField "shown" v
    omitted = numField "omitted" v
    total = numField "total" v
    reconcileViol =
        [ "limit " <> tShow limit <> ": shown+omitted /= total"
        | shown + omitted /= total || shown /= length hits
        ]
    installedShown = [h | h <- hits, hitText "install" h == "installed"]
    budgetViol =
        [ "limit " <> tShow limit <> ": installed hit not shown first"
        | not (null hits)
        , null installedShown || hitText "install" (head hits) /= "installed"
        ]
    tShow = T.pack . show

numField :: Text -> Value -> Int
numField k v = case field k v of
    Just (Number n) -> round n
    _ -> 0
