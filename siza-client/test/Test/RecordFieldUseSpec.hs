{-# LANGUAGE OverloadedStrings #-}

module Test.RecordFieldUseSpec (recordFieldUseSpec) where

import Data.Aeson (Key, Value (..), object, (.=))
import Data.Text (Text)
import Test.Hspec

import Siza.Agent.Discover.Classify (sessionAnswer)
import Siza.Agent.Discover.Types (
    DHit (..),
    Interpreted (..),
    SourceAnswer (..),
 )

interp :: Interpreted
interp = Interpreted "columnSeparator" "columnSeparator" Nothing "name" "" []

matchWith :: [(Key, Value)] -> Value
matchWith extra =
    object
        ( [ "module" .= ("DataFrame" :: Text)
          , "name" .= ("columnSeparator" :: Text)
          , "type" .= ("ReadOptions -> Char" :: Text)
          , "via" .= ("name" :: Text)
          ]
            <> extra
        )

hitsOf :: Value -> [DHit]
hitsOf card = saHits (sessionAnswer interp (Just (object ["matches" .= [card]])))

recordFieldUseSpec :: Spec
recordFieldUseSpec =
    describe "a record field's update syntax survives into the client's hit" $ do
        it "trails the import that reaches the name (A3)" $ do
            let [h] =
                    hitsOf
                        ( matchWith
                            [ ("import", String "import DataFrame (columnSeparator)")
                            , ("field", String "ReadOptions { columnSeparator = ... }")
                            ]
                        )
            dhUse h
                `shouldBe` Just
                    ( "import DataFrame (columnSeparator)"
                        <> "; record update: ReadOptions { columnSeparator = ... }"
                    )

        it "never stands alone as the way to reach the name (A3)" $ do
            let [h] =
                    hitsOf (matchWith [("field", String "ReadOptions { columnSeparator = ... }")])
            dhUse h `shouldBe` Nothing

        it "carries the import the session computed" $ do
            let [h] = hitsOf (matchWith [("import", String "import DataFrame (columnSeparator)")])
            dhUse h `shouldBe` Just "import DataFrame (columnSeparator)"

        it "leaves an ordinary function's use note for the generic import fallback" $ do
            let [h] = hitsOf (matchWith [])
            dhUse h `shouldBe` Nothing
