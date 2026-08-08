{-# LANGUAGE OverloadedStrings #-}

{- | A scope only filters what the search brought back. Searched globally a
package the caller named loses on rank to whatever else answers to the same
word — the live 20260807 miss, where eight @module=Data.HodaTime.*@ searches
returned nothing while the index held the signature — so the scope has to reach
the search itself, not only the filter after it.
-}
module Test.DiscoverScopeReachSpec (discoverScopeReachSpec) where

import Data.Aeson (Value (..))
import Data.Text (Text)
import Test.Hspec

import Siza.Agent.Discover.Fetch (capabilityArgs)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Types (
    Interpreted,
    NotebookEnv (..),
    Scope (..),
    emptyScope,
 )
import Test.DiscoverFixtures (field)

emptyEnv :: NotebookEnv
emptyEnv = NotebookEnv [] [] [] [] [] []

interpOf :: Text -> Interpreted
interpOf = interpret emptyEnv

-- | Whether the built arguments carry a key with the value given.
sends :: Text -> Text -> Value -> Bool
sends k want v = field k v == Just (String want)

discoverScopeReachSpec :: Spec
discoverScopeReachSpec =
    describe "a scope reaches the search, not only the filter" $ do
        it "sends a package scope to the capability channel" $
            capabilityArgs True (Scope Nothing (Just "hodatime")) (interpOf "difference")
                `shouldSatisfy` sends "package" "hodatime"

        it "sends a module scope to the capability channel" $
            capabilityArgs
                True
                (Scope (Just "Data.HodaTime.Instant") Nothing)
                (interpOf "difference")
                `shouldSatisfy` sends "module" "Data.HodaTime.Instant"

        it "sends no scope key when the caller named none" $ do
            let v = capabilityArgs True emptyScope (interpOf "difference")
            field "package" v `shouldBe` Nothing
            field "module" v `shouldBe` Nothing

        it "still carries the query the caller wrote" $
            capabilityArgs True (Scope Nothing (Just "hodatime")) (interpOf "difference")
                `shouldSatisfy` sends "query" "difference"
