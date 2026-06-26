{-# LANGUAGE OverloadedStrings #-}

{- | The pure core of the server-side missing-extension repair: read the LANGUAGE
extension GHC says it needs, and enable it in the cell's pragma. The IO loop that
re-runs the cell is verified live; these pin the decisions it makes.
-}
module Test.ExtRepairSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.ExtRepair (addExtension, extFromResult)
import Sabela.AI.Types (ExecutionResult (..))
import Test.Hspec

-- | A GHC type error that carries the "intended to use" extension hint.
extErr :: Text
extErr =
    T.unlines
        [ "<interactive>:3:14: error: [GHC-83865]"
        , "    Couldn't match type \8216[Char]\8217 with \8216Text\8217"
        , "    Perhaps you intended to use OverloadedStrings"
        ]

raising :: Text -> Either Text ExecutionResult
raising msg = Right (ExecutionResult [] (Just msg) [] [])

spec :: Spec
spec = describe "Sabela.AI.ExtRepair" $ do
    describe "addExtension" $ do
        it "inserts a fresh pragma after a leading -- cabal: line" $
            addExtension "OverloadedStrings" "-- cabal: build-depends: text\nx = \"hi\""
                `shouldBe` "-- cabal: build-depends: text\n{-# LANGUAGE OverloadedStrings #-}\nx = \"hi\"\n"

        it "inserts a fresh pragma at the top when there is no cabal line" $
            addExtension "TemplateHaskell" "x = 1"
                `shouldBe` "{-# LANGUAGE TemplateHaskell #-}\nx = 1\n"

        it "merges into an existing LANGUAGE pragma" $
            addExtension "TemplateHaskell" "{-# LANGUAGE OverloadedStrings #-}\nx = 1"
                `shouldBe` "{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}\nx = 1\n"

        it "is a no-op when the extension is already enabled" $
            addExtension "OverloadedStrings" "{-# LANGUAGE OverloadedStrings #-}\nx = 1"
                `shouldBe` "{-# LANGUAGE OverloadedStrings #-}\nx = 1"

    describe "extFromResult" $ do
        it "reads the extension GHC says is missing" $
            extFromResult (raising extErr) `shouldBe` Just "OverloadedStrings"

        it "is Nothing when the failure names no extension" $
            extFromResult (raising "Couldn't match Int with Bool") `shouldBe` Nothing

        it "is Nothing on an abort (Left)" $
            extFromResult (Left "Cancelled") `shouldBe` Nothing
