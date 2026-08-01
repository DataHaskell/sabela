{-# LANGUAGE OverloadedStrings #-}

{- | `find_by_type` answers with a structured `fits` array, while every consumer
of a fit list reads GHC's plain rendering through `parseHoleFits`. Reading a
`result` field the payload never emitted left the type-directed tiers with an
empty blob, so they could never produce a candidate.
-}
module Test.FitsBlobSpec (fitsBlobSpec) where

import Data.Aeson (Value (..), object, (.=))
import Test.Hspec

import Sabela.AI.HoleFits (HoleFit (..), parseHoleFits)
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Repair (fitsBlob)

payload :: Value
payload =
    object
        [ "goal" .= ("_ :: Picture -> Picture -> Picture" :: String)
        , "shown" .= (2 :: Int)
        , "probe" .= ("typecheck-only; nothing was committed" :: String)
        , "fits"
            .= [ object
                    [ "write" .= ("mappend" :: String)
                    , "type" .= ("forall a. Monoid a => a -> a -> a" :: String)
                    , "refined" .= False
                    , "module" .= ("Prelude" :: String)
                    ]
               , object
                    [ "write" .= ("(<>)" :: String)
                    , "type" .= ("forall a. Semigroup a => a -> a -> a" :: String)
                    , "refined" .= False
                    ]
               ]
        ]

fitsBlobSpec :: Spec
fitsBlobSpec = describe "a find_by_type answer round-trips through the fit parser" $ do
    it "renders every fit the tool reported" $
        map hfWrite (parseHoleFits (fitsBlob (Right (ToolOk payload))))
            `shouldBe` ["mappend", "(<>)"]

    it "keeps each fit's type" $
        map hfType (parseHoleFits (fitsBlob (Right (ToolOk payload))))
            `shouldBe` ["forall a. Monoid a => a -> a -> a", "forall a. Semigroup a => a -> a -> a"]

    it "keeps the module where the tool gave one" $
        map hfModule (parseHoleFits (fitsBlob (Right (ToolOk payload))))
            `shouldBe` [Just "Prelude", Nothing]

    it "is empty when the tool found nothing" $
        fitsBlob
            (Right (ToolOk (object ["fits" .= ([] :: [Value]), "shown" .= (0 :: Int)])))
            `shouldBe` ""

    it "is empty when the call failed outright" $
        fitsBlob (Left "transport error") `shouldBe` ""

    it "never invents a fit from a payload without the field" $
        fitsBlob (Right (ToolOk (object ["goal" .= ("_ :: Int" :: String)])))
            `shouldBe` ""
